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

module SlamData.Workspace.Card.Setups.Chart.Radar.Eval
  ( eval
  , module SlamData.Workspace.Card.Setups.Chart.Radar.Model
  ) where

import SlamData.Prelude

import Data.Argonaut (Json, decodeJson, (.?))
import Data.Array as A
import Data.Foldable as F
import Data.Lens ((^?), _Just)
import Data.List as L
import Data.Map as M
import Data.Set as Set
import Data.Int as Int

import ECharts.Monad (DSL)
import ECharts.Commands as E
import ECharts.Types as ET
import ECharts.Types.Phantom (OptionI)
import ECharts.Types.Phantom as ETP

import SlamData.Workspace.Card.CardType.ChartType (ChartType(Radar))
import SlamData.Workspace.Card.Port as Port
import SlamData.Workspace.Card.Setups.Axis (Axes)
import SlamData.Workspace.Card.Setups.Chart.ColorScheme (colors)
import SlamData.Workspace.Card.Setups.Chart.Common as SCC
import SlamData.Workspace.Card.Setups.Chart.Common.Positioning
  (RadialPosition, adjustRadialPositions, radialTitles)
import SlamData.Workspace.Card.Setups.Chart.Radar.Model (Model, ModelR)
import SlamData.Workspace.Card.Setups.Common.Eval (type (>>))
import SlamData.Workspace.Card.Setups.Common.Eval as BCE
import SlamData.Workspace.Card.Setups.Dimension as D
import SlamData.Workspace.Card.Setups.Semantics as Sem
import SlamData.Workspace.Card.Setups.Transform as T
import SlamData.Workspace.Card.Setups.Transform.Aggregation as Ag

import SqlSquare as Sql

import Utils.Array (enumerate)

eval ∷ ∀ m. BCE.ChartSetupEval ModelR m
eval = BCE.chartSetupEval (SCC.buildBasicSql buildProjections buildGroupBy) buildPort

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

type Item =
  { category ∷ String
  , measure ∷ Number
  , multiple ∷ Maybe String
  , parallel ∷ Maybe String
  }

decodeItem ∷ Json → Either String Item
decodeItem = decodeJson >=> \obj → do
  category ← Sem.requiredString "" <$> obj .? "category"
  measure ← Sem.requiredNumber zero <$> obj .? "measure"
  multiple ← Sem.maybeString <$> obj .? "multiple"
  parallel ← Sem.maybeString <$> obj .? "parallel"
  pure { category, measure, multiple, parallel }

buildProjections ∷ ModelR → L.List (Sql.Projection Sql.Sql)
buildProjections r = L.fromFoldable
  [ r.category # SCC.jcursorPrj # Sql.as "category"
  , r.value # SCC.jcursorPrj # Sql.as "measure" # SCC.applyTransform r.value
  , r.multiple # maybe SCC.nullPrj SCC.jcursorPrj # Sql.as "multiple"
  , r.parallel # maybe SCC.nullPrj SCC.jcursorPrj # Sql.as "parallel"
  ]

buildGroupBy ∷ ModelR → Maybe (Sql.GroupBy Sql.Sql)
buildGroupBy r =
  SCC.groupBy $ L.fromFoldable $ A.catMaybes
    [ r.parallel <#> SCC.jcursorSql
    , r.multiple <#> SCC.jcursorSql
    , Just r.category <#> SCC.jcursorSql
    ]

buildPort ∷ ModelR → Axes → Array Json → Port.Port
buildPort m _ jarr =
  Port.ChartInstructions
    { options: buildOptions m $ buildData m jarr
    , chartType: Radar
    }

buildData ∷ ModelR → Array Json → Array SeriesOnRadar
buildData r records = series
  where
  items ∷ Array Item
  items = foldMap (foldMap A.singleton ∘ decodeItem) records

  -- | maybe parallel >> maybe multiple series >> category name >> values
  dataMap ∷ Maybe String >> Maybe String >> String >> Array Number
  dataMap = foldl dataMapFoldFn M.empty items

  dataMapFoldFn
    ∷ Maybe String >> Maybe String >> String >> Array Number
    → Item
    → Maybe String >> Maybe String >> String >> Array Number
  dataMapFoldFn acc item = M.alter alterParallelFn item.parallel acc
    where
    values =
      pure item.measure

    alterParallelFn
      ∷ Maybe (Maybe String >> String >> Array Number)
      → Maybe (Maybe String >> String >> Array Number)
    alterParallelFn = case _ of
      Nothing → Just $ M.singleton item.multiple $ M.singleton item.category values
      Just multiple → Just $ M.alter alterMultipleFn item.multiple multiple

    alterMultipleFn
      ∷ Maybe (String >> Array Number)
      → Maybe (String >> Array Number)
    alterMultipleFn = case _ of
      Nothing → Just $ M.singleton item.category values
      Just category → Just $ M.alter alterCategoryFn item.category category

    alterCategoryFn
      ∷ Maybe (Array Number)
      → Maybe (Array Number)
    alterCategoryFn = case _ of
      Nothing → Just values
      Just arr → Just $ arr ⊕ values

  unpositionedSeries ∷ Array SeriesOnRadar
  unpositionedSeries = map mkSeriesOnRadar $ A.fromFoldable $ M.toList dataMap

  mkSeriesOnRadar
    ∷ Maybe String × (Maybe String >> String >> Array Number)
    → SeriesOnRadar
  mkSeriesOnRadar (name × seriesData) =
    { name
    , x: Nothing
    , y: Nothing
    , radius: Nothing
    , series: map mkMultipleSeries $ A.fromFoldable $ M.toList seriesData
    }

  mkMultipleSeries
    ∷ Maybe String × (String >> Array Number)
    → RadarSerie
  mkMultipleSeries (name × itemsData) =
    { name
    , items:
        flip map itemsData
        $ Ag.runAggregation
        $ fromMaybe Ag.Sum
        $ r.value ^? D._value ∘ D._transform ∘ _Just ∘ T._Aggregation
    }

  series ∷ Array SeriesOnRadar
  series = adjustRadialPositions unpositionedSeries

buildOptions ∷ ModelR → Array SeriesOnRadar → DSL OptionI
buildOptions r radarData = do
  E.tooltip do
    E.triggerItem
    E.textStyle do
      E.fontFamily "Ubuntu, sans"
      E.fontSize 12

  E.legend do
    E.items $ map ET.strItem serieNames
    E.textStyle do
      E.fontFamily "Ubuntu, sans"
      E.fontSize 12
    E.orient ET.Vertical
    E.leftLeft

  E.colors colors

  E.radars
    $ traverse_ E.radar radars

  E.series
    $ traverse_ E.radarSeries series

  radialTitles radarData

  where
  serieNames ∷ Array String
  serieNames =
    A.fromFoldable
      $ foldMap (_.series
                 ⋙ foldMap (_.name ⋙ Set.fromFoldable))
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
