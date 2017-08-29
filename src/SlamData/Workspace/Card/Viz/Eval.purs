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

module SlamData.Workspace.Card.Viz.Eval
  ( evalTextLike
  , evalLabeled
  , evalChart
  , module M
  ) where

import SlamData.Prelude

import Control.Monad.Aff.Class (class MonadAff)
import Control.Monad.State (class MonadState, get, put)
import Control.Monad.Writer.Class (class MonadTell)
import Data.Lens (preview, _Just, (?~), (^?))
import Data.List as L
import Data.ListMap as LM
import Data.Map as Map
import Data.Set as Set
import Data.Variant (match)
import SlamData.Effects (SlamDataEffects)
import SlamData.Quasar.Class (class QuasarDSL, class ParQuasarDSL)
import SlamData.Quasar.Query as QQ
import SlamData.Workspace.Card.CardType.Select as Sel
import SlamData.Workspace.Card.Error as CE
import SlamData.Workspace.Card.Eval.Common as CEC
import SlamData.Workspace.Card.Eval.Monad as CEM
import SlamData.Workspace.Card.Eval.State as CES
import SlamData.Workspace.Card.Port as Port
import SlamData.Workspace.Card.Port.VarMap as VM
import SlamData.Workspace.Card.Setups.Auxiliary as Aux
import SlamData.Workspace.Card.Setups.Dimension as D
import SlamData.Workspace.Card.Setups.Semantics as Sem
import SlamData.Workspace.Card.Viz.Eval.Area as Area
import SlamData.Workspace.Card.Viz.Eval.Bar as Bar
import SlamData.Workspace.Card.Viz.Eval.Boxplot as Boxplot
import SlamData.Workspace.Card.Viz.Eval.Candlestick as Candlestick
import SlamData.Workspace.Card.Viz.Eval.Funnel as Funnel
import SlamData.Workspace.Card.Viz.Eval.Gauge as Gauge
import SlamData.Workspace.Card.Viz.Eval.Graph as Graph
import SlamData.Workspace.Card.Viz.Eval.Heatmap as Heatmap
import SlamData.Workspace.Card.Viz.Eval.Line as Line
import SlamData.Workspace.Card.Viz.Eval.Parallel as Parallel
import SlamData.Workspace.Card.Viz.Eval.Pie as Pie
import SlamData.Workspace.Card.Viz.Eval.PunchCard as PunchCard
import SlamData.Workspace.Card.Viz.Eval.Radar as Radar
import SlamData.Workspace.Card.Viz.Eval.Sankey as Sankey
import SlamData.Workspace.Card.Viz.Eval.Scatter as Scatter
import SlamData.Workspace.Card.Viz.Model as M
import SlamData.Workspace.Card.Viz.Renderer.Input.Model as TLR
import SlamData.Workspace.Card.Viz.Renderer.Select.Model as LR
import SqlSquared (SqlQuery)
import SqlSquared as Sql
import Utils (stringToNumber)
import Utils.SqlSquared (all, asRel, variRelation)

defaultSelectionVar ∷ String
defaultSelectionVar = "selection"

eval
  ∷ ∀ m v
  . MonadAff SlamDataEffects m
  ⇒ MonadAsk CEM.CardEnv m
  ⇒ MonadThrow (Variant (qerror ∷ CE.QError | v)) m
  ⇒ MonadTell CEM.CardLog m
  ⇒ MonadState CEM.CardState m
  ⇒ QuasarDSL m
  ⇒ ParQuasarDSL m
  ⇒ SqlQuery
  → Sql.Sql
  → Port.Resource
  → m Port.Out
eval sql selection r = do
  CEM.CardEnv { varMap, cardId } ← ask
  let varMap' = varMap # VM.insert cardId (VM.Var defaultSelectionVar) (VM.Expr selection)
  resource ← CE.liftQ $ CEC.localEvalResource sql varMap'
  pure $ Port.resourceOut cardId resource varMap'

evalLabeled
  ∷ ∀ m
  . MonadAff SlamDataEffects m
  ⇒ MonadAsk CEM.CardEnv m
  ⇒ MonadThrow CE.CardError m
  ⇒ MonadTell CEM.CardLog m
  ⇒ MonadState CEM.CardState m
  ⇒ QuasarDSL m
  ⇒ ParQuasarDSL m
  ⇒ LR.Model
  → Port.SetupSelectPort
  → m Port.Out
evalLabeled m p = do
  resourceVar × r ← CEM.extractResourcePair Port.Initial
  cardState ← get
  let
    lastUsedResource = cardState >>= preview CES._LastUsedResource

    availableValues =
      Set.fromFoldable $ Map.keys p.valueLabelMap

    selected
      -- Reloading
      | lastUsedResource ≡ Nothing
        ∧ (not $ Set.isEmpty m.selected)
        ∧ (not $ Set.isEmpty $ Set.intersection availableValues m.selected)  =
          m.selected
      -- same resource: take selected from model
      | lastUsedResource ≡ Just r =
          m.selected
      -- new resource and checkbox: empty selection
      | p.formInputType ≡ Sel.checkbox =
          Set.empty
      -- default selection is empty, new resource this is not checkbox: take first value
      | Set.isEmpty p.selectedValues =
          Set.fromFoldable $ L.head $ Map.keys p.valueLabelMap
      -- new resource, not checkbox: take default selection
      | otherwise =
          p.selectedValues

    semantics =
      foldMap Sem.semanticsToSql selected

    selection =
      Sql.set semantics

    sql =
      Sql.buildSelect
        $ all
        ∘ (Sql._relations
            ?~ (variRelation (unwrap resourceVar) # asRel "res"))
        ∘ (Sql._filter
             ?~ ( Sql.binop Sql.In
                    ( Sql.binop Sql.FieldDeref
                        ( Sql.ident "res" )
                        ( maybe (Sql.splice Nothing) (QQ.jcursorToSql Nothing)
                          $ p.projection ^? D._value ∘ D._projection ))
                    ( Sql.vari defaultSelectionVar)))

  put $ Just $ CEM.AutoSelect {lastUsedResource: r, autoSelect: selected}

  eval (Sql.Query mempty sql) selection r

evalTextLike
  ∷ ∀ m
  . MonadAff SlamDataEffects m
  ⇒ MonadAsk CEM.CardEnv m
  ⇒ MonadThrow CE.CardError m
  ⇒ MonadTell CEM.CardLog m
  ⇒ MonadState CEM.CardState m
  ⇒ QuasarDSL m
  ⇒ ParQuasarDSL m
  ⇒ TLR.Model
  → Port.SetupInputPort
  → m Port.Out
evalTextLike m p = do
  resourceVar × r ← CEM.extractResourcePair Port.Initial
  cardState ← get
  let
    selection = p.formInputType # match
      { numeric: const $ maybe Sql.null Sql.num $ stringToNumber m.value
      , time: const $ Sql.invokeFunction "TIME" $ pure $ Sql.string m.value
      , date: const $ Sql.invokeFunction "DATE" $ pure $ Sql.string m.value
      , datetime: const $ Sql.invokeFunction "TIMESTAMP" $ pure $ Sql.string m.value
      , text: const $ Sql.string m.value
      }
    sql =
      Sql.buildSelect
        $ all
        ∘ (Sql._relations
            ?~ (variRelation (unwrap resourceVar) # asRel "res"))
        ∘ (Sql._filter
             ?~ ( Sql.binop Sql.Eq
                    ( Sql.binop Sql.FieldDeref
                        ( Sql.ident "res" )
                        ( maybe (Sql.splice Nothing) (QQ.jcursorToSql Nothing)
                          $ p.projection ^? D._value ∘ D._projection ))
                    (Sql.vari defaultSelectionVar) ))

  eval (Sql.Query mempty sql) selection r

evalChart
  ∷ ∀ m
  . MonadState CEM.CardState m
  ⇒ MonadThrow CE.CardError m
  ⇒ QuasarDSL m
  ⇒ MonadAsk CEM.CardEnv m
  ⇒ Port.ChartInstructionsPort
  → M.ChartModel
  → Port.Resource
  → m Port.Port
evalChart port m resource = do
  CEM.CardEnv { path } ← ask
  state ← get
  resourceVar × _ ← CEM.extractResourcePair Port.Initial

  case state ^? _Just ∘ CES._Chart of
    Just st | st.eventRaised || setupUnchanged st port → do
      createPort $ st.extractData $ fromMaybe [] do
        cht ← m.chartType
        LM.eqListMap.lookup cht m.events
    _ → do
      results ← CE.liftQ $ CEC.sampleResource path resource Nothing
      let (extractData × options) = buildOptions
      put $ Just $ CES.ChartOptions
        { options
        , eventRaised: false
        , dimMap: port.dimMap
        , aux: port.aux
        , extractData: extractData resourceVar
        , chartType: port.chartType
        }
      createPort $ extractData resourceVar []
  where
  createPort _ =
    pure $ Port.ResourceKey Port.defaultResourceVar

  emptyOut resourceVar _ =
      Sql.buildSelect
        $ all
        ∘ (Sql._relations
            ?~ (variRelation (unwrap resourceVar) # asRel "res"))

  buildOptions = port.chartType # match
    { pie: const $ emptyOut × pure unit
    , line: const $ emptyOut × pure unit
    , bar: const $ emptyOut × pure unit
    , area: const $ emptyOut × pure unit
    , scatter: const $ emptyOut × pure unit
    , radar: const $ emptyOut × pure unit
    , funnel: const $ emptyOut × pure unit
    , graph: const $ emptyOut × pure unit
    , heatmap: const $ emptyOut × pure unit
    , sankey: const $ emptyOut × pure unit
    , gauge: const $ emptyOut × pure unit
    , boxplot: const $ emptyOut × pure unit
    , metric: const $ emptyOut × pure unit
    , punchCard: const $ emptyOut × pure unit
    , candlestick: const $ emptyOut × pure unit
    , parallel: const $ emptyOut × pure unit
    , pivot: const $ emptyOut × pure unit
    }

  mbAuxEq Nothing Nothing = true
  mbAuxEq (Just a) (Just b) = Aux.eq_ a b
  mbAuxEq _ _ = false

  setupUnchanged s p =
    s.dimMap ≡ p.dimMap
    && s.chartType ≡ p.chartType
    && mbAuxEq s.aux p.aux
