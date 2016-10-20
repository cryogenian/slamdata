module SlamData.Workspace.Card.BuildChart.Candlestick.Eval
  ( eval
  , module SlamData.Workspace.Card.BuildChart.Candlestick.Model
  ) where

import SlamData.Prelude

import Data.Argonaut (JArray, Json)
import Data.Array as A
import Data.Foldable as F
import Data.Int as Int
import Data.Map as M
import Data.Set as Set

import ECharts.Monad (DSL)
import ECharts.Commands as E
import ECharts.Types as ET
import ECharts.Types.Phantom (OptionI)
import ECharts.Types.Phantom as ETP

import Quasar.Types (FilePath)

import SlamData.Quasar.Class (class QuasarDSL)
import SlamData.Quasar.Error as QE
import SlamData.Workspace.Card.BuildChart.Common.Eval (type (>>))
import SlamData.Workspace.Card.BuildChart.Common.Eval as BCE
import SlamData.Workspace.Card.BuildChart.Aggregation as Ag
import SlamData.Workspace.Card.CardType.ChartType (ChartType(Candlestick))
import SlamData.Workspace.Card.BuildChart.Semantics as Sem
import SlamData.Workspace.Card.BuildChart.Common.Positioning as BCP (rectangularGrids, rectangularTitles, adjustRectangularPositions)
import SlamData.Workspace.Card.BuildChart.ColorScheme (colors)
import SlamData.Workspace.Card.Port as Port
import SlamData.Workspace.Card.BuildChart.Axis as Ax
import SlamData.Workspace.Card.Eval.CardEvalT as CET
import SlamData.Workspace.Card.BuildChart.Candlestick.Model (CandlestickR, Model)

import Utils.Foldable (enumeratedFor_)

eval
  ∷ ∀ m
  . (Monad m, QuasarDSL m)
  ⇒ Model
  → FilePath
  → Ax.Axes
  → CET.CardEvalT m Port.Port
eval Nothing _ _ =
  QE.throw "Please select axis to aggregate"
eval (Just conf) resource axes = do
  records ← BCE.records resource
  pure $ Port.ChartInstructions (buildCandlestick conf records axes) Candlestick

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
  , fontSize ∷ Maybe Number
  , series ∷ Maybe String >> Series
  }

type CandlestickData = Maybe String >> OnOneGrid

buildCandlestickData ∷ CandlestickR → JArray → CandlestickData
buildCandlestickData r records = series
  where

  dataMap ∷ Maybe String >> Maybe String >> String >> HLOC (Array Number)
  dataMap =
    foldl dataMapFoldFn M.empty records

  dataMapFoldFn
    ∷ Maybe String >> Maybe String >> String >> HLOC (Array Number)
    → Json
    → Maybe String >> Maybe String >> String >> HLOC (Array Number)
  dataMapFoldFn acc js =
    let
      getMaybeString = Sem.getMaybeString js
      getValues = Sem.getValues js
    in case getMaybeStringFromJson r.dimension of
      Nothing → acc
      Just dimKey →
        let
          high =
            getValues js $ pure r.high
          low =
            getValues js $ pure r.low
          open =
            getValues js $ pure r.open
          close =
            getValues js $ pure r.closes

          hloc = {high, low, open, close}

          mbParallel =
            getMaybeString =<< r.parallel

          mbSeries =
            getMaybeString =<< r.series

          alterParallelFn
            ∷ Maybe (Maybe String >> String >> HLOC (Array Number))
            → Maybe (Maybe String >> String >> HLOC (Array Number))
          alterParallelFn Nothing =
            Just $ M.singleton mbSeries $ M.singleton dimKey hloc
          alterParallelFn (Just parallel) =
            Just $ M.alter alterSeriesFn mbSeries parallel

          alterSeriesFn
            ∷ Maybe (String >> HLOC (Array Number))
            → Maybe (String >> HLOC (Array Number))
          alterSeriesFn Nothing =
            Just $ M.singleton dimKey hloc
          alterSeriesFn (Just dims) =
            Just $  M.alter alterDimFn dimKey dims

          alterDimFn
            ∷ Maybe (HLOC (Array Number))
            → Maybe (HLOC (Array Number))
          alterDimFn Nothing = hloc
          alterDimFn (Just r) =
            Just { high = high ⊕ r.high
                 , low = low ⊕ r.low
                 , open = open ⊕ r.open
                 , close = close ⊕ r.closes
                 }
        in
          M.alter alterParallelFn mbParallel acc

  rawSeries ∷ CandlestickData
  rawSeries =
    map mkOneGridData dataMap

  mkOneGridData
    ∷ Maybe String >> String >> HLOC (Array Number)
    → OnOneGrid
  mkOneGridData mp =
    { x: Nothing
    , y: Nothing
    , w: Nothing
    , h: Nothing
    , fontSize: Nothing
    , series: map mkSeries mp
    }

  mkSeries
    ∷ String >> HLOC (Array Number)
    → String >> HLOC Number
  mkSeries = map aggregateHLOC

  aggregateHLOC ∷ HLOC (Array Number) → HLOC Number
  aggregateHLOC r =
    { high: Ag.runAggregation r.highAggregation r
    , low: Ag.runAggregation r.lowAggregation r
    , open: Ag.openAggregation r.openAggregation r
    , close: Ag.closeAggregation r.closeAggregation r
    }

  series ∷ CandlestickData
  series = BCP.adjustRectangularPositions rawSeries

buildCandlestick ∷ CandlestickR → JArray → Ax.Axes → DSL OptionI
buildCandlestick r records axes = do
  E.tooltip do
    E.triggerAxis
    E.textStyle do
      E.fontFamily "Ubuntu, sans"
      E.fontSize

  BCP.rectangularTitles candlestickData
  BCP.rectangularGrids candlestickData

  E.colors colors

  E.legend do
    E.topBottom
    E.textStyle $ E.fontFamily "Ubuntu, sans"
    E.items $ map ET.strItem serieNames

  E.xAxes xAxes
  E.yAxes yAxes
  E.series series

  where
  candlestickData ∷ CandlestickData
  candlestickData = buildCandlestickData r records

  serieNames ∷ Array String
  serieNames =
    foldMap A.singleton
    $ foldMap (M.values ⋙ foldMap (_.series ⋙ M.keys ⋙ Set.fromFoldable))
    $ candlestickData

  xValues ∷ ∀ a. OnOneGrid → String
  xValues onOneGrid =
    sortX
    $ foldMap A.singleton
    $ foldMap (_.series ⋙ M.keys ⋙ Set.fromFoldable)
    $ onOneGrid

  sortX ∷ Array String → Array String
  sortX = A.sortBy $ Ax.compareWithAxisType $ Ax.axisType r.dimension axes

  xAxes = enumeratedFor_ candlestickData \(ix × onOneGrid) → E.addXAxis do
    E.gridIndex ix
    E.axisType ET.Category
    E.axisLabel $ E.textStyle $ E.fontFamily "Ubuntu, sans"
    E.items $ map ET.strItem $ xValues onOneGrid

  yAxes = enumeratedFor_ candlestickData \(ix × _) → E.addYAxis
    E.gridIndex ix
    E.axisType ET.Value

  series = enumeratedFor_  candlestickData \(ix × onOneGrid) →
    for_ (M.toList onOneGrid.series) \(mbName × serie) → do
      for_ mbName E.name
      E.xAxisIndex ix
      E.yAxisIndex ix

      E.buildItems $ for_ (xValues onOneGrid) \(xIx × dim) →
        for_ (M.lookup dim serie) $ E.additem ∘ E.buildValues ∘ E.addValue
