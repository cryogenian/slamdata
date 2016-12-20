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

module SlamData.Workspace.Card.BuildChart.Candlestick.Eval
  ( eval
  , module SlamData.Workspace.Card.BuildChart.Candlestick.Model
  ) where

import SlamData.Prelude

import Control.Monad.State (class MonadState)
import Control.Monad.Throw (class MonadThrow)

import Data.Argonaut (JArray, Json)
import Data.Array as A
import Data.Map as M

import ECharts.Monad (DSL)
import ECharts.Commands as E
import ECharts.Types as ET
import ECharts.Types.Phantom (OptionI)

import SlamData.Quasar.Class (class QuasarDSL)
import SlamData.Workspace.Card.BuildChart.Common.Eval (type (>>))
import SlamData.Workspace.Card.BuildChart.Common.Eval as BCE
import SlamData.Workspace.Card.BuildChart.Aggregation as Ag
import SlamData.Workspace.Card.CardType.ChartType (ChartType(Candlestick))
import SlamData.Workspace.Card.BuildChart.Semantics as Sem
import SlamData.Workspace.Card.BuildChart.Common.Positioning as BCP
import SlamData.Workspace.Card.BuildChart.ColorScheme (colors)
import SlamData.Workspace.Card.Port as Port
import SlamData.Workspace.Card.BuildChart.Axis as Ax
import SlamData.Workspace.Card.Eval.Monad as CEM
import SlamData.Workspace.Card.BuildChart.Candlestick.Model (CandlestickR, Model)

import Utils.Foldable (enumeratedFor_)

eval
  ∷ ∀ m
  . ( MonadState CEM.CardState m
    , MonadThrow CEM.CardError m
    , QuasarDSL m
    )
  ⇒ Port.TaggedResourcePort
  → Model
  → m Port.Port
eval = BCE.buildChartEval Candlestick buildCandlestick

type HLOC a =
  { low ∷ a
  , high ∷ a
  , open ∷ a
  , close ∷ a
  }

type Series = String >> HLOC Number

type OnOneGrid =
  { w ∷ Maybe Number
  , h ∷ Maybe Number
  , x ∷ Maybe Number
  , y ∷ Maybe Number
  , name ∷ Maybe String
  , fontSize ∷ Maybe Int
  , items ∷ Series
  }

type CandlestickData = Array OnOneGrid

buildCandlestickData ∷ CandlestickR → JArray → CandlestickData
buildCandlestickData r records = series
  where
  dataMap ∷ Maybe String >> String >> HLOC (Array Number)
  dataMap =
    foldl dataMapFoldFn M.empty records

  dataMapFoldFn
    ∷ Maybe String >> String >> HLOC (Array Number)
    → Json
    → Maybe String >> String >> HLOC (Array Number)
  dataMapFoldFn acc js =
    let
      getMaybeString = Sem.getMaybeString js
      getValues = Sem.getValues js
    in case getMaybeString r.dimension of
      Nothing → acc
      Just dimKey →
        let
          high =
            getValues $ pure r.high
          low =
            getValues $ pure r.low
          open =
            getValues $ pure r.open
          close =
            getValues $ pure r.close

          hloc = {high, low, open, close}

          mbParallel =
            getMaybeString =<< r.parallel

          alterParallelFn
            ∷ Maybe (String >> HLOC (Array Number))
            → Maybe (String >> HLOC (Array Number))
          alterParallelFn Nothing =
            Just $ M.singleton dimKey hloc
          alterParallelFn (Just parallel) =
            Just $ M.alter alterDimFn dimKey parallel

          alterDimFn
            ∷ Maybe (HLOC (Array Number))
            → Maybe (HLOC (Array Number))
          alterDimFn Nothing = Just $ hloc
          alterDimFn (Just r') =
            Just { high: high ⊕ r'.high
                 , low: low ⊕ r'.low
                 , open: open ⊕ r'.open
                 , close: close ⊕ r'.close
                 }
        in
          M.alter alterParallelFn mbParallel acc

  rawSeries ∷ CandlestickData
  rawSeries =
    foldMap mkOneGridData $ M.toList dataMap

  mkOneGridData
    ∷ Maybe String × (String >> HLOC (Array Number))
    → Array OnOneGrid
  mkOneGridData (name × mp) =
    [ { x: Nothing
      , y: Nothing
      , w: Nothing
      , h: Nothing
      , fontSize: Nothing
      , name
      , items: map aggregateHLOC mp
      } ]

  aggregateHLOC ∷ HLOC (Array Number) → HLOC Number
  aggregateHLOC rr =
    { high: Ag.runAggregation r.highAggregation rr.high
    , low: Ag.runAggregation r.lowAggregation rr.low
    , open: Ag.runAggregation r.openAggregation rr.open
    , close: Ag.runAggregation r.closeAggregation rr.close
    }

  series ∷ CandlestickData
  series = BCP.adjustRectangularPositions rawSeries

buildCandlestick ∷ Ax.Axes → CandlestickR → JArray → DSL OptionI
buildCandlestick axes r records = do
  E.tooltip do
    E.triggerAxis
    E.textStyle do
      E.fontFamily "Ubuntu, sans"
      E.fontSize 12

  BCP.rectangularTitles candlestickData
  BCP.rectangularGrids candlestickData

  E.colors colors

  E.xAxes xAxes
  E.yAxes yAxes
  E.series series

  where
  candlestickData ∷ CandlestickData
  candlestickData = buildCandlestickData r records

  xValues ∷ OnOneGrid → Array String
  xValues  = sortX ∘ foldMap A.singleton ∘ M.keys ∘ _.items

  sortX ∷ Array String → Array String
  sortX = A.sortBy $ Ax.compareWithAxisType $ Ax.axisType r.dimension axes

  xAxes = enumeratedFor_ candlestickData \(ix × serie) → E.addXAxis do
    E.gridIndex ix
    E.axisType ET.Category
    E.axisLabel $ E.textStyle $ E.fontFamily "Ubuntu, sans"
    E.items $ map ET.strItem $ xValues serie

  yAxes = enumeratedFor_ candlestickData \(ix × _) → E.addYAxis do
    E.gridIndex ix
    E.axisType ET.Value

  series = enumeratedFor_  candlestickData \(ix × serie) → E.candlestick do
    for_ serie.name E.name
    E.xAxisIndex ix
    E.yAxisIndex ix
    E.buildItems $ for_ (xValues serie) \dim →
      for_ (M.lookup dim serie.items) \{high, low, open, close} → E.addItem $ E.buildValues do
        E.addValue open
        E.addValue close
        E.addValue low
        E.addValue high
