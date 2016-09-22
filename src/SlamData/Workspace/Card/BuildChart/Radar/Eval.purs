module SlamData.Workspace.Card.BuildChart.Radar.Eval
  ( eval
  , module SlamData.Workspace.Card.BuildChart.Radar.Model
  ) where

import SlamData.Prelude

import Data.Argonaut (JArray, Json, cursorGet, toNumber, toString)
import Data.Array as A
import Data.Foldable as F
import Data.Lens ((^?))
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
import SlamData.Workspace.Card.BuildChart.Common.Eval (type (>>))
import SlamData.Workspace.Card.BuildChart.Common.Eval as BCE
import SlamData.Workspace.Card.BuildChart.Common.Positioning
  (RadialPosition, adjustRadialPositions, radialTitles)
import SlamData.Workspace.Card.BuildChart.Radar.Model (Model, RadarR)
import SlamData.Workspace.Card.CardType.ChartType (ChartType(Radar))
import SlamData.Workspace.Card.Chart.Aggregation as Ag
import SlamData.Workspace.Card.Chart.BuildOptions.ColorScheme (colors)
import SlamData.Workspace.Card.Eval.CardEvalT as CET
import SlamData.Workspace.Card.Port as Port

import Utils.Array (enumerate)

eval
  ∷ ∀ m
  . (Monad m, QuasarDSL m)
  ⇒ Model
  → FilePath
  → CET.CardEvalT m Port.Port
eval Nothing _ =
  QE.throw "Please select axis to aggregate"
eval (Just conf) resource = do
  records ← BCE.records resource
  pure $ Port.ChartInstructions (buildRadar conf records) Radar

-- | One radar serie. Actually just data for echarts radar series
type RadarSerie =
  { name ∷ Maybe String
  , items ∷ String >> Number
  }

-- | All series that are drawn on one radar
type SeriesOnRadar =
  RadialPosition
    ( name ∷ Maybe String
    , series ∷ Array RadarSerie
    )

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
  series = adjustRadialPositions unpositionedSeries

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

  E.radars
    $ traverse_ E.radar radars

  E.series
    $ traverse_ E.radarSeries series

  radialTitles radarData

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

  series ∷ Array (DSL ETP.RadarSeriesI)
  series = (enumerate radarData) <#> \(ix × {series}) → do
    E.radarIndex $ Int.toNumber ix
    E.symbol ET.Circle
    let
      allKeys = foldMap (Set.fromFoldable ∘ M.keys ∘ _.items) series
    E.buildItems $ for_ series \serie → E.addItem do
      for_ serie.name E.name
      E.buildValues $ for_ allKeys \k →
        case M.lookup k serie.items of
          Nothing → E.missingValue
          Just val → E.addValue val

  minVal ∷ Maybe Number
  minVal =
    F.minimum
      $ map (fromMaybe zero
             ∘ F.minimum
             ∘ map (fromMaybe zero
                    ∘ F.minimum
                    ∘ M.values
                    ∘ _.items)
             ∘ _.series
            )
      radarData

  maxVal ∷ Maybe Number
  maxVal =
    F.maximum
      $ map (fromMaybe zero
             ∘ F.maximum
             ∘ map (fromMaybe zero
                    ∘ F.maximum
                    ∘ M.values
                    ∘ _.items)
             ∘ _.series
            )
      radarData

  radars ∷ Array (DSL ETP.RadarI)
  radars = radarData <#> \{name, series, x, y, radius} → do
    let
      allKeys = foldMap (Set.fromFoldable ∘ M.keys ∘ _.items) series

    E.indicators $ for_ allKeys \k → E.indicator do
      E.name k
      for_ minVal E.min
      for_ maxVal E.max

    E.nameGap 10.0

    E.buildCenter do
      traverse_ (E.setX ∘ E.percents) x
      traverse_ (E.setY ∘ E.percents) y

    traverse_
      (E.singleValueRadius
       ∘ ET.SingleValueRadius
       ∘ ET.Percent) radius
