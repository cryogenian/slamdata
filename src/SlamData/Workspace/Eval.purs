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

module SlamData.Workspace.Eval
  ( evalGraph
  ) where

import SlamData.Prelude

import Control.Comonad.Cofree (Cofree)
import Control.Comonad.Cofree as Cofree
import Control.Monad.Aff.Bus as Bus
import Control.Monad.Aff.Free (class Affable, fromAff, fromEff)
import Control.Monad.Eff.Ref (readRef, modifyRef)

import Data.Foldable as F
import Data.Function (on)
import Data.List (List, (:))
import Data.List as List
import Data.Map (Map)
import Data.Map as Map

import SlamData.Effects (SlamDataEffects)
import SlamData.GlobalError as GE
import SlamData.Quasar.Class (class QuasarDSL)
import SlamData.Wiring (Wiring)
import SlamData.Wiring as Wiring
import SlamData.Wiring.Cache as Cache
import SlamData.Workspace.Card.Eval.Monad as CEM
import SlamData.Workspace.Eval.Card as Card
import SlamData.Workspace.Eval.Deck as Deck

type Tick = Int

type EvalGraph =
  Cofree List
    { coord ∷ Card.Coord
    , card ∷ Card.Cell
    , deck ∷ Deck.Cell
    }

evalGraph
  ∷ ∀ m
  . ( Affable SlamDataEffects m
    , MonadReader Wiring m
    , MonadPar m
    , QuasarDSL m
    )
  ⇒ Card.DisplayCoord
  → Card.Coord
  → m Unit
evalGraph source coord = do
  { eval } ← Wiring.expose
  tick ← nextTick
  graph ←
    unfoldGraph
      <$> Cache.snapshot eval.cards
      <*> Cache.snapshot eval.decks
      <*> pure coord
  for_ graph \graph' → do
    notifyDecks Deck.Pending graph'
    let
      input = fromMaybe Card.Initial (Cofree.head graph').card.value.input
    runEvalLoop source tick input graph'

runEvalLoop
  ∷ ∀ m
  . ( Affable SlamDataEffects m
    , MonadReader Wiring m
    , MonadPar m
    , QuasarDSL m
    )
  ⇒ Card.DisplayCoord
  → Tick
  → Card.Port
  → EvalGraph
  → m Unit
runEvalLoop source tick input graph = do
  { path, varMaps, eval } ← Wiring.expose
  let
    node = Cofree.head graph
    next = Cofree.tail graph
    -- FIXME
    trans = unsafePartial (fromRight (Card.modelToEval node.card.value.model.model))
    env = CEM.CardEnv
      { path
      , coord: node.coord
      , urlVarMaps: varMaps
      }
  fromAff $ Bus.write (Card.Pending source input) node.card.bus
  result ← Card.runCard env node.card.value.state input trans
  tick' ← currentTick
  when (tick ≡ tick') case result.output of
    Left err → do
      -- FIXME: report errors
      let
        value' = node.card.value { state = result.state }
        output = Card.CardError case GE.fromQError err of
          Left msg → msg
          _        → "Global error" -- FIXME
      updateCardValue node.coord value' eval.cards
      fromAff do
        for_ result.state \state →
          Bus.write (Card.StateChange state) node.card.bus
        Bus.write (Card.Complete source output) node.card.bus
      notifyDecks (Deck.Complete node.coord output) graph
    Right output → do
      let
        value' = node.card.value
          { input = Just input
          , output = Just output
          , state = result.state
          }
      updateCardValue node.coord value' eval.cards
      fromAff do
        for_ result.state \state →
          Bus.write (Card.StateChange state) node.card.bus
        Bus.write (Card.Complete source output) node.card.bus
      when (deckCompleted (fst node.coord) next) $ fromAff do
        Bus.write (Deck.Complete node.coord output) node.deck.bus
      parTraverse_ (runEvalLoop source tick output) next

  where
    updateCardValue
      ∷ Card.Coord
      → Card.EvalResult
      → Cache.Cache Card.Coord Card.Cell
      → m Unit
    updateCardValue key value =
      Cache.alter key case _ of
        Just cell → pure (Just cell { value = value })
        _         → pure Nothing

notifyDecks
  ∷ ∀ m
  . ( Affable SlamDataEffects m
    , Applicative m
    )
  ⇒ Deck.EvalMessage
  → EvalGraph
  → m Unit
notifyDecks msg = traverse_ (fromAff ∘ Bus.write msg ∘ _.bus ∘ snd) ∘ nubDecks

deckCompleted ∷ Deck.Id → List EvalGraph → Boolean
deckCompleted deckId = not ∘ F.any (eq deckId ∘ fst ∘ _.coord ∘ Cofree.head)

unfoldGraph
  ∷ Map Card.Coord Card.Cell
  → Map Deck.Id Deck.Cell
  → Card.Coord
  → Maybe EvalGraph
unfoldGraph cards decks coord =
  go
    <$> Map.lookup coord cards
    <*> Map.lookup (fst coord) decks
  where
    go card deck =
      Cofree.mkCofree { coord, card, deck }
        (List.catMaybes (unfoldGraph cards decks <$> card.next))

nubDecks ∷ EvalGraph → List (Deck.Id × Deck.Cell)
nubDecks = List.nubBy (eq `on` fst) ∘ go
  where
    go co =
      let
        node = Cofree.head co
        head = fst node.coord × node.deck
      in
        head : join (go <$> Cofree.tail co)

nextTick
  ∷ ∀ m
  . ( Affable SlamDataEffects m
    , MonadReader Wiring m
    )
  ⇒ m Int
nextTick = do
  { eval } ← Wiring.expose
  fromEff do
    modifyRef eval.tick (add 1)
    readRef eval.tick

currentTick
  ∷ ∀ m
  . ( Affable SlamDataEffects m
    , MonadReader Wiring m
    )
  ⇒ m Int
currentTick = do
  { eval } ← Wiring.expose
  fromEff (readRef eval.tick)
