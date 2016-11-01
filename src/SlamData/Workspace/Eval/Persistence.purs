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

module SlamData.Workspace.Eval.Persistence where

import SlamData.Prelude

import Control.Monad.Aff.AVar (takeVar, putVar, modifyVar)
import Control.Monad.Aff.Bus as Bus
import Control.Monad.Aff.Free (class Affable, fromAff)
import Control.Monad.Aff.Promise (wait, defer)
import Control.Monad.Fork (class MonadFork, fork)

import Data.Array as Array
import Data.List (List(..), (:))
import Data.List as List
import Data.Map as Map

import SlamData.Effects (SlamDataEffects)
import SlamData.Quasar.Data as Quasar
import SlamData.Quasar.Class (class QuasarDSL)
import SlamData.Quasar.Error as QE
import SlamData.Workspace.Eval.Card as Card
import SlamData.Workspace.Eval.Deck as Deck
import SlamData.Wiring (Wiring)
import SlamData.Wiring as Wiring
import SlamData.Wiring.Cache as Cache

putDeck
  ∷ ∀ m
  . ( Affable SlamDataEffects m
    , MonadReader Wiring m
    , MonadFork m
    , QuasarDSL m
    )
  ⇒ Deck.Id
  → Deck.Model
  → m (Either QE.QError Unit)
putDeck deckId deck = do
  { path, eval } ← Wiring.expose
  ref ← defer do
    res ← Quasar.save (Deck.deckIndex path deckId) $ Deck.encode deck
    when (isLeft res) do
      void $ Cache.remove deckId eval.decks
    pure $ const deck <$> res
  -- FIXME
  -- Cache.put deckId ref wiring.decks
  rmap (const unit) <$> wait ref

-- | Loads a deck from a DeckId. Returns the model.
getDeck
  ∷ ∀ m
  . ( Affable SlamDataEffects m
    , MonadReader Wiring m
    , MonadFork m
    , MonadPar m
    , QuasarDSL m
    )
  ⇒ Deck.Id
  → m (Either QE.QError Deck.Model)
getDeck =
  getDeck' >=> _.value >>> wait

-- | Loads a deck from a DeckId. This has the effect of loading decks from
-- | which it extends (for mirroring) and populating the card graph. Returns
-- | the "cell" (model promise paired with its message bus).
getDeck'
  ∷ ∀ m
  . ( Affable SlamDataEffects m
    , MonadReader Wiring m
    , MonadFork m
    , MonadPar m
    , QuasarDSL m
    )
  ⇒ Deck.Id
  → m Deck.Cell
getDeck' deckId = do
  { path, eval } ← Wiring.expose
  let
    cacheVar = Cache.unCache eval.decks
  decks ← fromAff (takeVar cacheVar)
  case Map.lookup deckId decks of
    Just cell → do
      fromAff $ putVar cacheVar decks
      pure cell
    Nothing → do
      value ← defer do
        let
          deckPath = Deck.deckIndex path deckId
        result ← runExceptT do
          deck ← ExceptT $ (_ >>= Deck.decode >>> lmap QE.msgToQError) <$> Quasar.load deckPath
          _    ← ExceptT $ populateCards deckId deck
          pure deck
        when (isLeft result) $ fromAff do
          modifyVar (Map.delete deckId) cacheVar
        pure result
      cell ← { value, bus: _ } <$> fromAff Bus.make
      fromAff do
        putVar cacheVar (Map.insert deckId cell decks)
      forkDeckProcess deckId cell.bus
      pure cell

-- | Populates the card eval graph based on a deck model. This may fail as it
-- | also attempts to load/hydrate foreign cards (mirrors) as well.
populateCards
  ∷ ∀ m
  . ( Affable SlamDataEffects m
    , MonadReader Wiring m
    , MonadFork m
    , MonadPar m
    , QuasarDSL m
    )
  ⇒ Deck.Id
  → Deck.Model
  → m (Either QE.QError Unit)
populateCards deckId deck = runExceptT do
  { eval } ← Wiring.expose
  decks ←
    ExceptT $ sequence <$>
      parTraverse getDeck (Array.nub (fst <$> deck.mirror))

  case Array.last deck.mirror, List.fromFoldable deck.cards of
    Just _    , Nil    → pure unit
    Nothing   , cards  → lift $ threadCards eval.cards cards
    Just coord, c : cs → do
      cell ← do
        mb ← Cache.get coord eval.cards
        case mb of
          Nothing → QE.throw ("Card not found in eval cache: " <> show coord)
          Just a  → pure a
      let
        coord' = deckId × c.cardId
        cell' = cell { next = coord' : cell.next }
      Cache.put coord cell' eval.cards
      lift $ threadCards eval.cards (c : cs)

  where
    threadCards cache = case _ of
      Nil         → pure unit
      c : Nil     → makeCell c Nil cache
      c : c' : cs → do
        makeCell c (pure c'.cardId) cache
        threadCards cache (c' : cs)

    makeCell card next cache = do
      bus ← fromAff Bus.make
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
      forkCardProcess coord bus

forkLoop
  ∷ ∀ m r a
  . ( Affable SlamDataEffects m
    , MonadFork m
    )
  ⇒ (a → m Unit)
  → Bus.Bus (Bus.R' r) a
  → m Unit
forkLoop handler bus = void (fork loop)
  where
    loop = do
      msg ← fromAff (Bus.read bus)
      fork (handler msg)
      loop

forkDeckProcess
  ∷ ∀ m
  . ( Affable SlamDataEffects m
    , MonadReader Wiring m
    , MonadFork m
    , MonadPar m
    , QuasarDSL m
    )
  ⇒ Deck.Id
  → Bus.BusRW Deck.EvalMessage
  → m Unit
forkDeckProcess deckId = forkLoop case _ of
  _ → pure unit

forkCardProcess
  ∷ ∀ m
  . ( Affable SlamDataEffects m
    , MonadReader Wiring m
    , MonadFork m
    , MonadPar m
    , QuasarDSL m
    )
  ⇒ Card.Coord
  → Bus.BusRW Card.EvalMessage
  → m Unit
forkCardProcess coord@(deckId × cardId) = forkLoop case _ of
  _ → pure unit
