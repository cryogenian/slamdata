{-
Copyright 2016 SlamData, Inc.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
-}

module SlamData.Wiring
  ( Wiring(..)
  , CardEval
  , DeckRef
  , ActiveState
  , PendingMessage
  , DeckMessage(..)
  , StepByStepGuide(..)
  , makeWiring
  , getDeck
  , putDeck
  , putCardEval
  ) where

import SlamData.Prelude

import Control.Monad.Aff.AVar (takeVar, putVar, modifyVar)
import Control.Monad.Aff.Bus as Bus
import Control.Monad.Aff.Free (class Affable, fromAff, fromEff)
import Control.Monad.Aff.Promise (Promise, wait, defer)
import Control.Monad.Eff.Ref (Ref, newRef)
import Control.Monad.Fork (class MonadFork)

import Data.Array as Array
import Data.List (List(..), (:))
import Data.List as List
import Data.Map as Map
import Data.Set as Set

import SlamData.Effects (SlamDataEffects)
import SlamData.GlobalError as GE
import SlamData.Notification as N
import SlamData.Quasar.Auth.Authentication as Auth
import SlamData.Quasar.Data as Quasar
import SlamData.Quasar.Class (class QuasarDSL)
import SlamData.Quasar.Error as QE
import SlamData.GlobalMenu.Bus (SignInBus)
import SlamData.Workspace.Card.CardId (CardId)
import SlamData.Workspace.Card.Port (Port)
import SlamData.Workspace.Card.Port.VarMap as Port
import SlamData.Workspace.Deck.AdditionalSource (AdditionalSource)
import SlamData.Workspace.Deck.DeckId (DeckId)
import SlamData.Workspace.Deck.Model (Deck, deckIndex, decode, encode)
import SlamData.Workspace.Eval.Card as Card
import SlamData.Workspace.Eval.Deck as Deck
import SlamData.Wiring.Cache (Cache)
import SlamData.Wiring.Cache as Cache

import Utils.Path (DirPath)

type CardEval =
  { card ∷ DeckId × Card.Model
  , input ∷ Maybe (Promise Port)
  , output ∷ Maybe (Promise (Port × (Set.Set AdditionalSource)))
  }

type DeckRef = Promise (Either QE.QError Deck)

type PendingMessage =
  { source ∷ DeckId
  , pendingCard ∷ DeckId × Card.Model
  , cards ∷ Cache (DeckId × CardId) CardEval
  }

data DeckMessage
  = DeckFocused DeckId
  | URLVarMapsUpdated

data StepByStepGuide
  = CardGuide
  | FlipGuide

type ActiveState =
  { cardIndex ∷ Int
  }

newtype Wiring = Wiring
  { path ∷ DirPath
  , decks ∷ Cache DeckId DeckRef
  , activeState ∷ Cache DeckId ActiveState
  , cards ∷ Cache (DeckId × CardId) CardEval
  , pending ∷ Bus.BusRW PendingMessage
  , messaging ∷ Bus.BusRW DeckMessage
  , notify ∷ Bus.BusRW N.NotificationOptions
  , globalError ∷ Bus.BusRW GE.GlobalError
  , requestNewIdTokenBus ∷ Auth.RequestIdTokenBus
  , urlVarMaps ∷ Ref (Map.Map DeckId Port.URLVarMap)
  , signInBus ∷ SignInBus
  , hasIdentified ∷ Ref Boolean
  , presentStepByStepGuide ∷ Bus.BusRW StepByStepGuide
  , evalTick ∷ Ref Int
  , cards' ∷ Cache Card.Coord Card.Cell
  , decks' ∷ Cache Deck.Id Deck.Cell
  }

makeWiring
  ∷ ∀ m
  . (Affable SlamDataEffects m)
  ⇒ DirPath
  → Map.Map DeckId Port.URLVarMap
  → m Wiring
makeWiring path varMaps = fromAff do
  decks ← Cache.make
  activeState ← Cache.make
  cards ← Cache.make
  pending ← Bus.make
  messaging ← Bus.make
  notify ← Bus.make
  globalError ← Bus.make
  requestNewIdTokenBus ← Auth.authentication
  urlVarMaps ← fromEff (newRef varMaps)
  signInBus ← Bus.make
  hasIdentified ← fromEff (newRef false)
  presentStepByStepGuide ← Bus.make
  evalTick ← fromEff (newRef 0)
  cards' ← Cache.make
  decks' ← Cache.make
  pure $ Wiring
    { path
    , decks
    , activeState
    , cards
    , pending
    , messaging
    , notify
    , globalError
    , requestNewIdTokenBus
    , urlVarMaps
    , signInBus
    , hasIdentified
    , presentStepByStepGuide
    , evalTick
    , cards'
    , decks'
    }

putDeck
  ∷ ∀ m
  . ( Affable SlamDataEffects m
    , MonadReader Wiring m
    , MonadFork m
    , QuasarDSL m
    )
  ⇒ DeckId
  → Deck
  → m (Either QE.QError Unit)
putDeck deckId deck = do
  Wiring wiring ← ask
  ref ← defer do
    res ← Quasar.save (deckIndex wiring.path deckId) $ encode deck
    when (isLeft res) do
      void $ Cache.remove deckId wiring.decks
    pure $ const deck <$> res
  Cache.put deckId ref wiring.decks
  rmap (const unit) <$> wait ref

getDeck
  ∷ ∀ m
  . ( Affable SlamDataEffects m
    , MonadReader Wiring m
    , MonadFork m
    , MonadPar m
    , QuasarDSL m
    )
  ⇒ DeckId
  → m (Either QE.QError Deck)
getDeck =
  getDeck' >=> _.value >>> wait

getDeck'
  ∷ ∀ m
  . ( Affable SlamDataEffects m
    , MonadReader Wiring m
    , MonadFork m
    , MonadPar m
    , QuasarDSL m
    )
  ⇒ DeckId
  → m Deck.Cell
getDeck' deckId = do
  Wiring wiring ← ask
  let
    cacheVar  = Cache.unCache wiring.decks
    cacheVar' = Cache.unCache wiring.decks'
  decks  ← fromAff (takeVar cacheVar)
  decks' ← fromAff (takeVar cacheVar')
  case Map.lookup deckId decks' of
    Just cell → do
      fromAff do
        putVar cacheVar decks
        putVar cacheVar' decks'
      pure cell
    Nothing → do
      value ← defer do
        let
          path = deckIndex wiring.path deckId
        result ← runExceptT do
          deck  ← ExceptT $ (_ >>= decode >>> lmap QE.msgToQError) <$> Quasar.load path
          cards ← ExceptT $ populateCards deckId deck
          pure deck
        when (isLeft result) $ fromAff do
          modifyVar (Map.delete deckId) cacheVar
          modifyVar (Map.delete deckId) cacheVar'
        pure result
      cell ← { value, bus: _ } <$> fromAff Bus.make
      fromAff do
        putVar cacheVar (Map.insert deckId value decks)
        putVar cacheVar' (Map.insert deckId cell decks')
      pure cell

populateCards
  ∷ ∀ m
  . ( Affable SlamDataEffects m
    , MonadReader Wiring m
    , MonadFork m
    , MonadPar m
    , QuasarDSL m
    )
  ⇒ Deck.Id
  → Deck
  → m (Either QE.QError Unit)
populateCards deckId deck = runExceptT do
  Wiring wiring ← ask
  decks ←
    ExceptT $ sequence <$>
      parTraverse getDeck (Array.nub (fst <$> deck.mirror))

  case Array.last deck.mirror, List.fromFoldable deck.cards of
    Just _    , Nil    → pure unit
    Nothing   , cards  → fromAff $ threadCards wiring.cards' cards
    Just coord, c : cs → do
      cell ← do
        mb ← Cache.get coord wiring.cards'
        case mb of
          Nothing → QE.throw ("Card not found in eval cache: " <> show coord)
          Just a  → pure a
      let
        coord' = deckId × c.cardId
        cell' = cell { next = coord' : cell.next }
      fromAff do
        Cache.put coord cell' wiring.cards'
        threadCards wiring.cards' (c : cs)

  where
    threadCards cache = case _ of
      Nil         → pure unit
      c : Nil     → makeCell c Nil cache
      c : c' : cs → do
        makeCell c (pure c'.cardId) cache
        threadCards cache (c' : cs)

    makeCell card next cache = do
      bus ← Bus.make
      let
        coord = deckId × card.cardId
        value =
          { model: card
          , input: Nothing
          , output: Nothing
          , state: Nothing
          }
        cell =
          { bus
          , next: Tuple deckId <$> next
          , value
          }
      Cache.put coord cell cache

putCardEval
  ∷ ∀ m
  . (Affable SlamDataEffects m)
  ⇒ CardEval
  → Cache (DeckId × CardId) CardEval
  → m Unit
putCardEval step = Cache.put (map _.cardId step.card) step
