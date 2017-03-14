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

module SlamData.Workspace.Card.Setups.Chart.Bar.Eval
  ( eval
  , module SlamData.Workspace.Card.Setups.Chart.Bar.Model
  ) where

import SlamData.Prelude

import Control.Monad.State (class MonadState)
import Control.Monad.Throw (class MonadThrow)

import Data.Argonaut (JArray, Json)
import Data.Array as A
import Data.Map as M
import Data.Set as Set

import ECharts.Monad (DSL)
import ECharts.Commands as E
import ECharts.Types as ET
import ECharts.Types.Phantom (OptionI)
import ECharts.Types.Phantom as ETP

import SlamData.Quasar.Class (class QuasarDSL)
import SlamData.Workspace.Card.Setups.Common.Eval (type (>>))
import SlamData.Workspace.Card.Setups.Common.Eval as BCE
import SlamData.Workspace.Card.Setups.Chart.Bar.Model (Model, BarR, initialState, behaviour)
import SlamData.Workspace.Card.CardType.ChartType (ChartType(Bar))
import SlamData.Workspace.Card.Setups.Transform.Aggregation as Ag
import SlamData.Workspace.Card.Setups.Axis (Axes)
import SlamData.Workspace.Card.Setups.Axis as Ax
import SlamData.Workspace.Card.Setups.Semantics (getMaybeString, getValues)
import SlamData.Workspace.Card.Setups.Chart.ColorScheme (colors)
import SlamData.Workspace.Card.Setups.Chart.Common.Positioning as BCP
import SlamData.Workspace.Card.Eval.Monad as CEM
import SlamData.Workspace.Card.Port as Port
import SlamData.Workspace.Card.Setups.Behaviour as B

eval
  ∷ ∀ m
  . ( MonadState CEM.CardState m
    , MonadThrow CEM.CardError m
    , QuasarDSL m
    )
  ⇒ Model
  → Port.Resource
  → m Port.Port
eval m = BCE.buildChartEval Bar buildBar m \axes →
  B.defaultModel behaviour m initialState{axes = axes}

type BarSeries =
  { name ∷ Maybe String
  , items ∷ String >> Number
  }

type BarStacks =
  { stack ∷ Maybe String
  , series ∷ Array BarSeries
  }

buildBarData ∷ BarR → JArray → Array BarStacks
buildBarData r records = series
  where
  -- | maybe stack >> maybe parallel >> category >> values
  dataMap ∷ Maybe String >> Maybe String >> String >> Array Number
  dataMap =
    foldl dataMapFoldFn M.empty records

  dataMapFoldFn
    ∷ Maybe String >> Maybe String >> String >> Array Number
    → Json
    → Maybe String >> Maybe String >> String >> Array Number
  dataMapFoldFn acc js =
    let
      getMaybeStringFromJson = getMaybeString js
      getValuesFromJson = getValues js
    in case getMaybeStringFromJson r.category of
      Nothing → acc
      Just categoryKey →
        let
          mbStack =
            getMaybeStringFromJson =<< r.stack
          mbParallel =
            getMaybeStringFromJson =<< r.parallel
          values =
            getValuesFromJson $ pure r.value

          alterStackFn
            ∷ Maybe (Maybe String >> String >> Array Number)
            → Maybe (Maybe String >> String >> Array Number)
          alterStackFn Nothing =
            Just $ M.singleton mbParallel $ M.singleton categoryKey values
          alterStackFn (Just parallel) =
            Just $ M.alter alterParallelFn mbParallel parallel

          alterParallelFn
            ∷ Maybe (String >> Array Number)
            → Maybe (String >> Array Number)
          alterParallelFn Nothing =
            Just $ M.singleton categoryKey values
          alterParallelFn (Just category) =
            Just $ M.alter alterCategoryFn categoryKey category

          alterCategoryFn
            ∷ Maybe (Array Number)
            → Maybe (Array Number)
          alterCategoryFn Nothing = Just values
          alterCategoryFn (Just arr) = Just $ arr ⊕ values
        in
          M.alter alterStackFn mbStack acc

  series ∷ Array BarStacks
  series =
    foldMap mkBarStack $ M.toList dataMap

  mkBarStack
    ∷ Maybe String × (Maybe String >> String >> Array Number)
    → Array BarStacks
  mkBarStack (stack × sers) =
    [{ stack
     , series: foldMap mkBarSeries $ M.toList sers
     }]

  mkBarSeries
    ∷ Maybe String × (String >> Array Number)
    → Array BarSeries
  mkBarSeries (name × items) =
    [{ name
     , items: map (Ag.runAggregation r.valueAggregation) items
     }]


buildBar ∷ Axes → BarR → JArray → DSL OptionI
buildBar axes r records = do
  E.tooltip E.triggerAxis

  E.colors colors

  E.xAxis do
    E.axisType ET.Category
    E.enabledBoundaryGap
    E.items $ map ET.strItem xValues
    E.axisLabel do
      traverse_ E.interval xAxisConfig.interval
      E.rotate r.axisLabelAngle
      E.textStyle do
        E.fontFamily "Ubuntu, sans"

  E.yAxis do
    E.axisType ET.Value
    E.axisLabel $ E.textStyle do
      E.fontFamily "Ubuntu, sans"
    E.axisLine $ E.lineStyle $ E.width 1
    E.splitLine $ E.lineStyle $ E.width 1

  E.legend do
    E.textStyle $ E.fontFamily "Ubuntu, sans"
    case xAxisConfig.axisType of
      ET.Category | A.length seriesNames > 40 → E.hidden
      _ → pure unit
    E.items $ map ET.strItem seriesNames
    E.leftLeft
    E.topBottom

  E.grid BCP.cartesian

  E.series series

  where

  barData ∷ Array BarStacks
  barData = buildBarData r records

  xAxisConfig ∷ Ax.EChartsAxisConfiguration
  xAxisConfig = Ax.axisConfiguration $ Ax.axisType r.category axes

  seriesNames ∷ Array String
  seriesNames = case r.parallel of
    Just _ →
      A.fromFoldable
      $ foldMap (_.series ⋙ foldMap (_.name ⋙ foldMap Set.singleton ))
        barData
    Nothing →
      A.catMaybes $ map _.stack barData

  xValues ∷ Array String
  xValues =
    A.sortBy xSortFn
      $ A.fromFoldable
      $ foldMap (_.series
                 ⋙ foldMap (_.items
                            ⋙ M.keys
                            ⋙ Set.fromFoldable)) barData
  xSortFn ∷ String → String → Ordering
  xSortFn = Ax.compareWithAxisType $ Ax.axisType r.category axes

  series ∷ ∀ i. DSL (bar ∷ ETP.I|i)
  series = for_ barData \stacked →
    for_ stacked.series \serie → E.bar do
      E.buildItems $ for_ xValues \key →
        case M.lookup key serie.items of
          Nothing → E.missingItem
          Just v → E.addItem do
            E.name key
            E.buildValues do
              E.addStringValue key
              E.addValue v
      case r.parallel of
        Just _ → do
          for_ stacked.stack E.stack
          for_ serie.name E.name
        Nothing → do
          E.stack "default stack"
          for_ stacked.stack E.name
