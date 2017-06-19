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
import SlamData.Workspace.Card.Setups.Chart.Common.Positioning as BCP
import SlamData.Workspace.Card.Setups.Dimension as D
import SlamData.Workspace.Card.Setups.Chart.Radar.Model (Model, ModelR)
import SlamData.Workspace.Card.Setups.Common.Eval (type (>>))
import SlamData.Workspace.Card.Setups.Common.Eval as BCE
import SlamData.Workspace.Card.Setups.Semantics as Sem

import SqlSquared as Sql

import Utils.Array (enumerate)

eval ∷ ∀ m v. BCE.ChartSetupEval ModelR m v
eval = BCE.chartSetupEval (SCC.buildBasicSql buildProjections buildGroupBy) buildPort

-- | One radar serie. Actually just data for echarts radar series
type RadarSerie =
  { name ∷ Maybe String
  , items ∷ String >> Number
  }

-- | All series that are drawn on one radar
type SeriesOnRadar =
  BCP.RadialPosition
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

buildPort ∷ ModelR → Axes → Port.Port
buildPort m _ =
  Port.ChartInstructions
    { options: buildOptions m ∘ buildData m
    , chartType: Radar
    }

buildData ∷ ModelR → Array Json → Array SeriesOnRadar
buildData r =
  foldMap (foldMap A.singleton ∘ decodeItem)
    >>> series
    >>> BCP.adjustRadialPositions

  where
  series ∷ Array Item → Array SeriesOnRadar
  series =
    BCE.groupOn _.parallel
      >>> map \(name × items) →
            { name
            , x: Nothing
            , y: Nothing
            , radius: Nothing
            , series: multiples items
            }

  multiples ∷ Array Item → Array RadarSerie
  multiples =
    BCE.groupOn _.multiple
      >>> map \(name × items) →
            { name
            , items: M.fromFoldable $ toPoint <$> items
            }

  toPoint ∷ Item → Tuple String Number
  toPoint item = item.category × item.measure

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

  BCP.radialTitles radarData
    $ maybe "" D.jcursorLabel r.parallel

  where
  serieNames ∷ Array String
  serieNames =
    A.fromFoldable
      $ foldMap (_.series
                 ⋙ foldMap (_.name ⋙ Set.fromFoldable))
        radarData

  series ∷ Array (DSL ETP.RadarSeriesI)
  series = (enumerate radarData) <#> \(ix × {series: ss }) → do
    E.radarIndex $ Int.toNumber ix
    E.symbol ET.Circle
    let
      allKeys = foldMap (Set.fromFoldable ∘ M.keys ∘ _.items) ss
    E.buildItems $ for_ ss \serie → E.addItem do
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
  radars = radarData <#> \{name, series: ss, x, y, radius} → do
    let
      allKeys = foldMap (Set.fromFoldable ∘ M.keys ∘ _.items) ss

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
