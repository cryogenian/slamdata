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

module SlamData.Workspace.Card.Setups.Chart.Scatter.Eval
  ( eval
  , module SlamData.Workspace.Card.Setups.Chart.Scatter.Model
  ) where

import SlamData.Prelude

import Color as C

import Data.Argonaut (Json, decodeJson, (.?))
import Data.Array as A
import Data.Foldable as F
import Data.Int as Int
import Data.Lens ((.~))
import Data.List as L
import Data.Set as Set

import Global (infinity)

import ECharts.Monad (DSL)
import ECharts.Commands as E
import ECharts.Types as ET
import ECharts.Types.Phantom (OptionI)
import ECharts.Types.Phantom as ETP

import SlamData.Workspace.Card.CardType.ChartType (ChartType(Scatter))
import SlamData.Workspace.Card.Port as Port
import SlamData.Workspace.Card.Setups.Axis (Axes)
import SlamData.Workspace.Card.Setups.Chart.ColorScheme (colors, getTransparentColor)
import SlamData.Workspace.Card.Setups.Chart.Common as SCC
import SlamData.Workspace.Card.Setups.Chart.Common.Positioning as BCP
import SlamData.Workspace.Card.Setups.Chart.Common.Tooltip as CCT
import SlamData.Workspace.Card.Setups.Chart.Scatter.Model (Model, ModelR)
import SlamData.Workspace.Card.Setups.Common.Eval as BCE
import SlamData.Workspace.Card.Setups.Dimension as D
import SlamData.Workspace.Card.Setups.Semantics as Sem

import SqlSquared as Sql

import Utils.Foldable (enumeratedFor_)

eval ∷ ∀ m v. BCE.ChartSetupEval ModelR m v
eval =
  BCE.chartSetupEval
    (\a b → setDistinct $ SCC.buildBasicSql buildProjections buildGroupBy a b) buildPort
  where
  setDistinct = Sql._Select ∘ Sql._isDistinct .~ true

type ScatterSeries =
  { name ∷ Maybe String
  , w ∷ Maybe Number
  , h ∷ Maybe Number
  , x ∷ Maybe Number
  , y ∷ Maybe Number
  , fontSize ∷ Maybe Int
  , series ∷ Array OnOneGrid
  }

type ScatterItem =
  { x ∷ Number
  , y ∷ Number
  , r ∷ Number
  , size ∷ Int
  }

type OnOneGrid =
  { name ∷ Maybe String
  , items ∷ Array ScatterItem
  }

type Item =
  { abscissa ∷ Number
  , ordinate ∷ Number
  , size ∷ Number
  , parallel ∷ Maybe String
  , series ∷ Maybe String
  }

decodeItem ∷ Json → Either String Item
decodeItem = decodeJson >=> \obj → do
  abscissa ← Sem.requiredNumber zero <$> obj .? "abscissa"
  ordinate ← Sem.requiredNumber zero <$> obj .? "ordinate"
  size ← Sem.requiredNumber zero <$> obj .? "size"
  parallel ← Sem.maybeString <$> obj .? "parallel"
  series ← Sem.maybeString <$> obj .? "series"
  pure { abscissa, ordinate, size, parallel, series }

buildProjections ∷ ModelR → L.List (Sql.Projection Sql.Sql)
buildProjections r = L.fromFoldable
  [ r.abscissa # SCC.jcursorPrj # Sql.as "abscissa"
  , r.ordinate # SCC.jcursorPrj # Sql.as "ordinate" # SCC.applyTransform r.ordinate
  , sizeF
  , r.parallel # maybe SCC.nullPrj SCC.jcursorPrj # Sql.as "parallel"
  , r.series # maybe SCC.nullPrj SCC.jcursorPrj # Sql.as "series"
  ]
  where
  sizeF = case r.size of
    Just sv → sv # SCC.jcursorPrj # Sql.as "size" # SCC.applyTransform sv
    Nothing → SCC.nullPrj # Sql.as "size"

buildGroupBy ∷ ModelR → Maybe (Sql.GroupBy Sql.Sql)
buildGroupBy r =
  SCC.groupBy $ L.fromFoldable $ A.catMaybes
    [ r.parallel <#> SCC.jcursorSql
    , r.series <#> SCC.jcursorSql
    , Just $ r.abscissa # SCC.jcursorSql
    ]

buildPort ∷ ModelR → Axes → Port.Port
buildPort m _ =
  Port.ChartInstructions
    { options: buildOptions m ∘ buildData m
    , chartType: Scatter
    }

buildData ∷ ModelR → Array Json → Array ScatterSeries
buildData r =
  BCP.adjustRectangularPositions
  ∘ series
  ∘ foldMap (foldMap A.singleton ∘ decodeItem)
  where
  series ∷ Array Item → Array ScatterSeries
  series =
    BCE.groupOn _.parallel
      >>> map \(name × is) →
            { name
            , x: Nothing
            , y: Nothing
            , w: Nothing
            , h: Nothing
            , fontSize: Nothing
            , series: onOneGrids is
            }

  onOneGrids ∷ Array Item → Array OnOneGrid
  onOneGrids =
    BCE.groupOn _.series
      >>> map \(name × is) →
            { name
            , items: adjustSymbolSizes $ toPoint <$> is
            }

  toPoint ∷ Item → ScatterItem
  toPoint item =
    { x: item.abscissa
    , y: item.ordinate
    , r: item.size
    , size: zero
    }

  adjustSymbolSizes ∷ Array ScatterItem → Array ScatterItem
  adjustSymbolSizes is =
    let
      values =
        map _.r is
      minValue =
        fromMaybe zero $ F.minimum values
      maxValue =
        fromMaybe infinity $ F.maximum values
      distance =
        maxValue - minValue
      sizeDistance =
        r.maxSize - r.minSize

      relativeSize ∷ Number → Number
      relativeSize val
        | distance ≡ zero = val
        | val < 0.0 = 0.0
        | otherwise =
            r.maxSize - sizeDistance / distance * (maxValue - val)
    in
      map (\x → x{size = Int.floor $ relativeSize x.r}) is

buildOptions ∷ ModelR → Array ScatterSeries → DSL OptionI
buildOptions r scatterData = do
  let
    cols =
      [ { label: D.jcursorLabel r.abscissa, value: CCT.formatValueIx 0 }
      , { label: D.jcursorLabel r.ordinate, value: CCT.formatValueIx 1 }
      ]
    opts = A.catMaybes
      [ r.size <#> \dim → { label: D.jcursorLabel dim, value: CCT.formatValueIx 2 }
      , r.series <#> \dim → { label: D.jcursorLabel dim, value: _.seriesName }
      ]
  E.tooltip do
    E.formatterItem (CCT.tableFormatter (pure ∘ _.color) (cols <> opts) ∘ pure)
    E.triggerItem
    E.textStyle do
      E.fontFamily "Ubuntu, sans"
      E.fontSize 12
    E.axisPointer do
      E.crossAxisPointer
      E.crossStyle do
        E.color $ C.rgba 170 170 170 0.6
        E.widthNum 0.2
        E.solidLine
  E.colors colors

  BCP.rectangularGrids scatterData
  BCP.rectangularTitles scatterData
    $ maybe "" D.jcursorLabel r.parallel

  E.grid BCP.cartesian
  E.xAxes $ valueAxes E.addXAxis
  E.yAxes $ valueAxes E.addYAxis

  E.legend do
    E.topBottom
    E.textStyle $ E.fontFamily "Ubuntu, sans"
    E.items $ map ET.strItem seriesNames

  E.series series

  where
  valueAxes ∷ ∀ i a. (DSL (ETP.AxisI (gridIndex ∷ ETP.I|i)) → DSL a) → DSL a
  valueAxes addAxis = enumeratedFor_ scatterData \(ix × _) → addAxis do
    E.gridIndex ix
    E.axisType ET.Value
    E.axisLabel $ E.textStyle $ E.fontFamily "Ubuntu, sans"
    E.axisLine $ E.lineStyle do
      E.color $ C.rgba 184 184 184 1.0
      E.width 1
    E.splitLine $ E.lineStyle do
      E.color $ C.rgba 204 204 204 0.2
      E.width 1

  seriesNames ∷ Array String
  seriesNames =
    A.fromFoldable
    $ foldMap (_.series ⋙ foldMap (_.name ⋙ Set.fromFoldable)) scatterData

  series ∷ ∀ i. DSL (scatter ∷ ETP.I|i)
  series = enumeratedFor_ scatterData \(gridIx × onOneGrid) →
    enumeratedFor_ onOneGrid.series \(ix × serie) → E.scatter do
      E.xAxisIndex gridIx
      E.yAxisIndex gridIx
      for_ serie.name E.name
      for_ (A.index colors $ mod ix $ A.length colors) \color → do
        E.itemStyle $ E.normal $ E.color $ getTransparentColor color 0.5
      E.symbol ET.Circle
      E.buildItems $ for_ serie.items \item → E.addItem do
        E.buildValues do
          E.addValue item.x
          E.addValue item.y
          E.addValue item.r
        when (isJust r.size) $ E.symbolSize item.size
