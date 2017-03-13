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

module SlamData.Workspace.Card.Setups.Chart.Funnel.Eval
  ( eval
  , module SlamData.Workspace.Card.Setups.Chart.Funnel.Model
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

import SlamData.Common.Sort (Sort(..))
import SlamData.Common.Align (Align(..))
import SlamData.Quasar.Class (class QuasarDSL)
import SlamData.Workspace.Card.Setups.Common.Eval (type (>>))
import SlamData.Workspace.Card.Setups.Common.Eval as BCE
import SlamData.Workspace.Card.Setups.Chart.Common.Positioning as BCP
import SlamData.Workspace.Card.Setups.Chart.Funnel.Model (Model, FunnelR, initialState, behaviour)
import SlamData.Workspace.Card.CardType.ChartType (ChartType(Funnel))
import SlamData.Workspace.Card.Setups.Transform.Aggregation as Ag
import SlamData.Workspace.Card.Setups.Chart.ColorScheme (colors)
import SlamData.Workspace.Card.Setups.Semantics (getMaybeString, getValues)
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
eval m = BCE.buildChartEval Funnel (const buildFunnel) m \axes →
  B.defaultModel behaviour m initialState{axes = axes}

type FunnelSeries =
  { name ∷ Maybe String
  , items ∷ String >> Number
  , x ∷ Maybe Number
  , y ∷ Maybe Number
  , w ∷ Maybe Number
  , h ∷ Maybe Number
  , fontSize ∷ Maybe Int
  }

buildFunnelData ∷ FunnelR → JArray → Array FunnelSeries
buildFunnelData r records = series
  where
  -- | maybe series >> category >> values
  dataMap ∷ Maybe String >> String >> Array Number
  dataMap =
    foldl dataMapFoldFn M.empty records

  dataMapFoldFn
    ∷ Maybe String >> String >> Array Number
    → Json
    → Maybe String >> String >> Array Number
  dataMapFoldFn acc js =
    let
      getMaybeStringFromJson = getMaybeString js
      getValuesFromJson = getValues js
    in case getMaybeStringFromJson r.category of
      Nothing → acc
      Just categoryKey →
        let
          mbSeries =
            getMaybeStringFromJson =<< r.series
          values =
            getValuesFromJson $ pure r.value

          alterSeriesFn
            ∷ Maybe (String >> Array Number)
            → Maybe (String >> Array Number)
          alterSeriesFn Nothing =
            Just $ M.singleton categoryKey values
          alterSeriesFn (Just sers) =
            Just $ M.alter alterCategoryFn categoryKey sers

          alterCategoryFn
            ∷ Maybe (Array Number)
            → Maybe (Array Number)
          alterCategoryFn Nothing = Just values
          alterCategoryFn (Just arr) = Just $ arr ⊕ values
        in
          M.alter alterSeriesFn mbSeries acc

  rawSeries ∷ Array FunnelSeries
  rawSeries =
    foldMap mkOneSeries $ M.toList dataMap

  mkOneSeries
    ∷ Maybe String × (String >> Array Number)
    → Array FunnelSeries
  mkOneSeries (name × ss) =
    [{ name
     , x: Nothing
     , y: Nothing
     , w: Nothing
     , h: Nothing
     , fontSize: Nothing
     , items: map (Ag.runAggregation r.valueAggregation) ss
     }]

  series ∷ Array FunnelSeries
  series = adjustPosition rawSeries

  adjustPosition ∷ Array FunnelSeries → Array FunnelSeries
  adjustPosition = BCP.adjustRectangularPositions


buildFunnel ∷ FunnelR → JArray → DSL OptionI
buildFunnel r records = do
  E.tooltip do
    E.triggerItem
    E.textStyle do
      E.fontFamily "Ubuntu, sans"
      E.fontSize 12

  E.legend do
    E.items $ map ET.strItem legendNames
    E.topBottom
    E.textStyle do
      E.fontFamily "Ubuntu, sans"

  E.colors colors

  BCP.rectangularTitles funnelData

  E.series series

  where
  funnelData ∷ Array FunnelSeries
  funnelData = buildFunnelData r records

  legendNames ∷ Array String
  legendNames =
    A.fromFoldable
      $ foldMap (_.items ⋙ M.keys ⋙ Set.fromFoldable) funnelData

  series ∷ ∀ i. DSL (funnel ∷ ETP.I|i)
  series = for_ funnelData \{x, y, w, h, items, name: serieName} → E.funnel do
    traverse_ E.widthPct w
    traverse_ E.heightPct h
    for_ serieName E.name
    case r.order of
      Asc → E.ascending
      Desc → E.descending
    case r.align of
      LeftAlign → E.funnelLeft
      RightAlign → E.funnelRight
      CenterAlign → E.funnelCenter
    E.label do
      E.normal do
        E.textStyle $ E.fontFamily "Ubuntu, sans"
        E.positionInside
      E.emphasis do
        E.textStyle $ E.fontFamily "Ubuntu, sans"
        E.positionInside
    E.buildItems $ for_ (M.toList items) \(name × value) → E.addItem do
      E.name name
      E.value value
    traverse_ (E.top ∘ ET.Percent) y
    traverse_ (E.left ∘ ET.Percent) x
