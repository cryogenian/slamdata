module SlamData.Workspace.Card.BuildChart.Radar.Eval
  ( eval
  , module SlamData.Workspace.Card.BuildChart.Radar.Model
  ) where

import SlamData.Prelude

import Data.Argonaut (JArray, JCursor, Json, cursorGet, toNumber, toString)
import Data.Array as A
import Data.Foldable as F
import Data.Lens ((^?))
import Data.Lens as Lens
import Data.Map as M
import Data.Set as Set
import Data.Int as Int

import ECharts.Monad (DSL)
import ECharts.Commands as E
import ECharts.Types as ET
import ECharts.Types.Phantom (OptionI)
import ECharts.Types.Phantom as ETP

import Quasar.Types (FilePath)

import SlamData.Quasar.Class (class QuasarDSL)
import SlamData.Quasar.Error as QE
import SlamData.Quasar.Query as QQ
import SlamData.Workspace.Card.BuildChart.Radar.Model (Model, RadarR)
import SlamData.Workspace.Card.CardType.ChartType (ChartType(Radar))
import SlamData.Workspace.Card.Chart.Aggregation as Ag
import SlamData.Workspace.Card.Chart.Axis (Axis, Axes, analyzeJArray)
import SlamData.Workspace.Card.Chart.Axis as Ax
import SlamData.Workspace.Card.Chart.BuildOptions.ColorScheme (colors)
import SlamData.Workspace.Card.Chart.Semantics as Sem
import SlamData.Workspace.Card.Eval.CardEvalT as CET
import SlamData.Workspace.Card.Port as Port
import SlamData.Workspace.Card.BuildChart.Radar.Model (RadarR)


eval
  ∷ ∀ m
  . (Monad m, QuasarDSL m)
  ⇒ Model
  → FilePath
  → CET.CardEvalT m Port.Port
eval Nothing _ =
  QE.throw "Please select axis to aggregate"
eval (Just conf) resource = do
  numRecords ←
    CET.liftQ $ QQ.count resource

  when (numRecords > 10000)
    $ QE.throw
    $ "The 10000 record limit for visualizations has been exceeded - the current dataset contains "
    ⊕ show numRecords
    ⊕ " records. "
    ⊕ "Please consider using a 'limit' or 'group by' clause in the query to reduce the result size."

  records ←
    CET.liftQ $ QQ.all resource

  pure $ Port.ChartInstructions (buildRadar conf records) Radar

infixr 3 type M.Map as >>

-- | One radar serie. Actually just data for echarts radar series
type RadarSerie =
  { name ∷ Maybe String
  , items ∷ String >> Number
  }

-- | All series that are drawn on one radar
type SeriesOnRadar =
  { name ∷ Maybe String
  , x ∷ Maybe Number
  , y ∷ Maybe Number
  , radius ∷ Maybe Number
  , series ∷ Array RadarSerie
  }

buildRadarData ∷ RadarR → JArray → Array SeriesOnRadar
buildRadarData r records = series
  where
  -- | maybe parallel >> maybe multiple series >> category name >> values
  dataMap ∷ Maybe String >> Maybe String >> String >> Array Number
  dataMap =
    foldl dataMapFoldFn M.empty records

  dataMapFoldFn
    ∷ Maybe String >> Maybe String >> String >> Array Number
    → Json
    → Maybe String >> Maybe String >> String >> Array Number
  dataMapFoldFn acc js =
    case toString =<< cursorGet r.category js of
      Nothing → acc
      Just categoryKey →
        let
          mbParallel = toString =<< flip cursorGet js =<< r.parallel
          mbMultiple = toString =<< flip cursorGet js =<< r.multiple
          values = foldMap A.singleton $ toNumber =<< cursorGet r.value js

          alterParallelFn
            ∷ Maybe (Maybe String >> String >> Array Number)
            → Maybe (Maybe String >> String >> Array Number)
          alterParallelFn Nothing =
            Just $ M.singleton mbMultiple $ M.singleton categoryKey values
          alterParallelFn (Just multiple) =
            Just $ M.alter alterMultipleFn mbMultiple multiple

          alterMultipleFn
            ∷ Maybe (String >> Array Number)
            → Maybe (String >> Array Number)
          alterMultipleFn Nothing =
            Just $ M.singleton categoryKey values
          alterMultipleFn (Just category) =
            Just $ M.alter alterCategoryFn categoryKey category

          alterCategoryFn
            ∷ Maybe (Array Number)
            → Maybe (Array Number)
          alterCategoryFn Nothing = Just values
          alterCategoryFn (Just arr) = Just $ arr ⊕ values
        in
         M.alter alterParallelFn mbParallel acc

  unpositionedSeries ∷ Array SeriesOnRadar
  unpositionedSeries =
    foldMap mkSeriesOnRadar $ M.toList dataMap

  mkSeriesOnRadar
    ∷ Maybe String × (Maybe String >> String >> Array Number)
    → Array SeriesOnRadar
  mkSeriesOnRadar (name × seriesData) =
    [{ name
     , x: Nothing
     , y: Nothing
     , radius: Nothing
     , series: foldMap mkMultipleSeries $ M.toList seriesData
     }]

  mkMultipleSeries
    ∷ Maybe String × (String >> Array Number)
    → Array RadarSerie
  mkMultipleSeries (name × itemsData) =
    [{ name
     , items: map (Ag.runAggregation r.valueAggregation) itemsData
     }]

  series ∷ Array SeriesOnRadar
  series =
    let
      len = A.length unpositionedSeries

      itemsInRow
        | len < 2 = 1
        | len < 5 = 2
        | len < 10 = 3
        | otherwise = 4

      numRows =
        Int.ceil $ Int.toNumber len / Int.toNumber itemsInRow

      topStep =
        90.0 / Int.toNumber numRows

      setPositions
        ∷ Array SeriesOnRadar
        → Int
        → Int
        → Int
        → Array SeriesOnRadar
        → Array SeriesOnRadar
      setPositions acc colIx rowIx inThisRow arr = case A.uncons arr of
        Nothing → acc
        Just {head, tail} →
          let
            top = topStep * ((Int.toNumber rowIx) + 0.5) + 5.0

            leftStep = 90.0 / Int.toNumber inThisRow

            left = leftStep * ((Int.toNumber colIx) + 0.5) + 5.0

            toPush =
              head { x = Just left
                   , y = Just top
                   , radius = Just $ 90.0 / Int.toNumber itemsInRow
                   }

            newAcc = A.snoc acc toPush

            newColIx
              | one + colIx < inThisRow = colIx + one
              | otherwise = zero

            newRowIx
              | newColIx ≡ zero = rowIx + one
              | otherwise = rowIx

            inNewRow
              | A.length tail > itemsInRow = itemsInRow
              | newColIx ≠ zero = itemsInRow
              | otherwise = A.length tail
          in
            setPositions newAcc newColIx newRowIx inNewRow tail
    in
      setPositions [ ] 0 0 itemsInRow unpositionedSeries


buildRadar ∷ RadarR → JArray → DSL OptionI
buildRadar r records = do
  E.tooltip do
    E.triggerItem
    E.textStyle do
      E.fontFamily "Ubuntu, sans"
      E.fontSize 12

  E.legend do
    E.items $ map ET.strItem serieNames
    E.topBottom
    E.textStyle do
      E.fontFamily "Ubuntu, sans"

  E.colors colors

  E.titles
    $ traverse_ E.title titles

  E.radars
    $ traverse_ E.radar radars

  E.series
    $ traverse_ E.radarSeries series

  where
  radarData ∷ Array SeriesOnRadar
  radarData = buildRadarData r records

  serieNames ∷ Array String
  serieNames =
    A.fromFoldable
      $ foldMap (_.series
                 ⋙ foldMap (_.name
                            ⋙ (foldMap Set.singleton)))
        radarData

  titles ∷ Array (DSL ETP.TitleI)
  titles = [ ]

  series ∷ Array (DSL ETP.RadarSeriesI)
  series = [ ]

  radars ∷ Array (DSL ETP.RadarI)
  radars = [ ]
