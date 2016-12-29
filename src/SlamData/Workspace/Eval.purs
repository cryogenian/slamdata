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
import Data.List (List)
import Data.Map (Map)

import SlamData.Effects (SlamDataEffects)
import SlamData.GlobalError as GE
import SlamData.Quasar.Class (class QuasarDSL)
import SlamData.Wiring (Wiring)
import SlamData.Wiring as Wiring
import SlamData.Wiring.Cache as Cache
import SlamData.Workspace.Card.Eval.Monad as CEM
import SlamData.Workspace.Card.Port.VarMap (URLVarMap)
import SlamData.Workspace.Eval.Card as Card
import SlamData.Workspace.Eval.Deck as Deck
import SlamData.Workspace.Eval.Graph (EvalGraph)
import SlamData.Workspace.Eval.Traverse (resolveUrlVarMaps)

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
  { varMaps, eval } ← Wiring.expose
  let
    input = fromMaybe Card.Initial (Cofree.head graph).card.input
  urlVarMaps ←
    resolveUrlVarMaps
      <$> Cache.snapshot eval.decks
      <*> Cache.snapshot eval.cards
      <*> liftEff (readRef varMaps)
  tick ← nextTick
  runEvalLoop urlVarMaps source tick mempty input graph

runEvalLoop
  ∷ ∀ f m
  . ( MonadAff SlamDataEffects m
    , MonadAsk Wiring m
    , Parallel f m
    , QuasarDSL m
    )
  ⇒ Map Card.Id URLVarMap
  → Card.DisplayCoord
  → Tick
  → Array Card.Id
  → Card.Port
  → EvalGraph
  → m Unit
runEvalLoop urlVarMaps source tick trail input graph = do
  { path, eval } ← Wiring.expose
  let
    node = Cofree.head graph
    next = Cofree.tail graph
    trail' = Array.snoc trail node.cardId
    env = CEM.CardEnv
      { path
      , cardId: node.cardId
      , urlVarMaps
      }
  liftAff $ Bus.write (Card.Pending source input) node.card.bus
  result ← Card.runCard env node.card.state input node.transition
  case result.output of
    Left err → do
      let
        output = Card.CardError case GE.fromQError err of
          Left msg → msg
          Right ge → GE.print ge
        card' = node.card
          { input = Just input
          , output = Just output
          , state = result.state
          , sources = result.sources
          , tick = Just tick
          }
      Cache.put node.cardId card' eval.cards
      liftAff do
        for_ result.state \state →
          Bus.write (Card.StateChange source state) node.card.bus
        Bus.write (Card.Complete source output) node.card.bus
      notifyDecks (Deck.Complete trail' output) graph
    Right output → do
      let
        card' = node.card
          { input = Just input
          , output = Just output
          , state = result.state
          , sources = result.sources
          , tick = Just tick
          }
      Cache.put node.cardId card' eval.cards
      liftAff do
        for_ result.state \state →
          Bus.write (Card.StateChange source state) node.card.bus
        Bus.write (Card.Complete source output) node.card.bus
      flip parTraverse_ (unwrap next) case _ of
        Left leaf →
          liftAff $ Bus.write (Deck.Complete trail' output) leaf.deck.bus
        Right graph' →
          runEvalLoop urlVarMaps source tick trail' output graph'

notifyDecks
  ∷ ∀ m
  . MonadAff SlamDataEffects m
  ⇒ Deck.EvalMessage
  → EvalGraph
  → m Unit
notifyDecks msg = traverse_ (liftAff ∘ Bus.write msg ∘ _.bus) ∘ pendingDecks

pendingDecks ∷ EvalGraph → List Deck.Cell
pendingDecks graph =
  either (pure ∘ _.deck) pendingDecks =<< unwrap (Cofree.tail graph)

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
