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
  , notifyDecks
  ) where

import SlamData.Prelude

import Control.Comonad.Cofree as Cofree
import Control.Monad.Aff.Bus as Bus
import Control.Monad.Aff.Class (class MonadAff, liftAff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Ref (readRef, modifyRef)

import Data.Array as Array
import Data.Foldable as F
import Data.Function (on)
import Data.List (List, (:))
import Data.List as List

import SlamData.Effects (SlamDataEffects)
import SlamData.GlobalError as GE
import SlamData.Quasar.Class (class QuasarDSL)
import SlamData.Wiring (Wiring)
import SlamData.Wiring as Wiring
import SlamData.Wiring.Cache as Cache
import SlamData.Workspace.Card.Eval.Monad as CEM
import SlamData.Workspace.Eval.Card as Card
import SlamData.Workspace.Eval.Deck as Deck
import SlamData.Workspace.Eval.Graph (EvalGraph)

import Utils (censor)

type Tick = Int

evalGraph
  ∷ ∀ f m
  . ( MonadAff SlamDataEffects m
    , MonadAsk Wiring m
    , Parallel f m
    , QuasarDSL m
    )
  ⇒ Card.DisplayCoord
  → EvalGraph
  → m Unit
evalGraph source graph = do
  let
    input = fromMaybe Card.Initial (Cofree.head graph).card.value.input
  tick ← nextTick
  runEvalLoop source tick mempty input graph

runEvalLoop
  ∷ ∀ f m
  . ( MonadAff SlamDataEffects m
    , MonadAsk Wiring m
    , Parallel f m
    , QuasarDSL m
    )
  ⇒ Card.DisplayCoord
  → Tick
  → Array Card.Coord
  → Card.Port
  → EvalGraph
  → m Unit
runEvalLoop source tick trail input graph = do
  { path, varMaps, eval } ← Wiring.expose
  urlVarMaps ← Cache.snapshot varMaps
  let
    node = Cofree.head graph
    next = Cofree.tail graph
    trail' = Array.snoc trail node.coord
    env = CEM.CardEnv
      { path
      , coord: node.coord
      , urlVarMaps
      }
  liftAff $ Bus.write (Card.Pending source input) node.card.bus
  result ← Card.runCard env node.card.value.state input node.transition
  case result.output of
    Left err → do
      let
        output = Card.CardError case GE.fromQError err of
          Left msg → msg
          Right ge → GE.print ge
        value' = node.card.value
          { input = Just input
          , output = Just output
          , state = result.state
          , sources = result.sources
          , tick = Just tick
          }
      updateCardValue node.coord value' eval.cards
      liftAff do
        for_ result.state \state →
          Bus.write (Card.StateChange state) node.card.bus
        Bus.write (Card.Complete source output) node.card.bus
      notifyDecks (Deck.Complete trail' output) graph
    Right output → do
      let
        value' = node.card.value
          { input = Just input
          , output = Just output
          , state = result.state
          , sources = result.sources
          , tick = Just tick
          }
      updateCardValue node.coord value' eval.cards
      liftAff do
        for_ result.state \state →
          Bus.write (Card.StateChange state) node.card.bus
        Bus.write (Card.Complete source output) node.card.bus
      when (deckCompleted (fst node.coord) (List.mapMaybe censor (unwrap next))) $ liftAff do
        Bus.write (Deck.Complete trail' output) node.deck.bus
      flip parTraverse_ (unwrap next) case _ of
        Left (_ × deck) →
          liftAff $ Bus.write (Deck.Complete trail' output) deck.bus
        Right graph' →
          runEvalLoop source tick trail' output graph'
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
  . MonadAff SlamDataEffects m
  ⇒ Deck.EvalMessage
  → EvalGraph
  → m Unit
notifyDecks msg = traverse_ (liftAff ∘ Bus.write msg ∘ _.bus ∘ snd) ∘ nubDecks

deckCompleted ∷ Deck.Id → List EvalGraph → Boolean
deckCompleted deckId = not ∘ F.any (eq deckId ∘ fst ∘ _.coord ∘ Cofree.head)

nubDecks ∷ EvalGraph → List (Deck.Id × Deck.Cell)
nubDecks = List.nubBy (eq `on` fst) ∘ go
  where
    go co =
      let
        node = Cofree.head co
        head = fst node.coord × node.deck
      in
        head : join (goTail <$> unwrap (Cofree.tail co))

    goTail (Left a) = pure a
    goTail (Right co) = go co

nextTick
  ∷ ∀ m
  . ( MonadAff SlamDataEffects m
    , MonadAsk Wiring m
    )
  ⇒ m Int
nextTick = do
  { eval } ← Wiring.expose
  liftEff do
    modifyRef eval.tick (add 1)
    readRef eval.tick

currentTick
  ∷ ∀ m
  . ( MonadAff SlamDataEffects m
    , MonadAsk Wiring m
    )
  ⇒ m Int
currentTick = do
  { eval } ← Wiring.expose
  liftEff (readRef eval.tick)
