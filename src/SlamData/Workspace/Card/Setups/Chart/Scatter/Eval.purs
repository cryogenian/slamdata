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
import Data.Lens ((^?), _Just)
import Data.List as L
import Data.Map as M
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
import SlamData.Workspace.Card.Setups.Common.Eval (type (>>))
import SlamData.Workspace.Card.Setups.Common.Eval as BCE
import SlamData.Workspace.Card.Setups.Dimension as D
import SlamData.Workspace.Card.Setups.Semantics as Sem
import SlamData.Workspace.Card.Setups.Transform as T
import SlamData.Workspace.Card.Setups.Transform.Aggregation as Ag

import SqlSquare as Sql

import Utils.Foldable (enumeratedFor_)

eval ∷ ∀ m. BCE.ChartSetupEval ModelR m
eval = BCE.chartSetupEval (SCC.buildBasicSql buildProjections buildGroupBy) buildScatter

type ScatterSeries =
  { name ∷ Maybe String
  , w ∷ Maybe Number
  , h ∷ Maybe Number
  , x ∷ Maybe Number
  , y ∷ Maybe Number
  , fontSize ∷ Maybe Int
  , series ∷ Array OnOneGrid
  }

type OnOneGrid =
  { name ∷ Maybe String
  , items ∷ Array {x ∷ Number, y ∷ Number, r ∷ Number}
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
    ]

buildScatter ∷ ModelR → Axes → Array Json → Port.Port
buildScatter m _ jarr =
  Port.ChartInstructions
    { options: scatterOptions m $ buildScatterData m jarr
    , chartType: Scatter
    }

buildScatterData ∷ ModelR → Array Json → Array ScatterSeries
buildScatterData r records = series
  where
  items ∷ Array Item
  items = foldMap (foldMap A.singleton ∘ decodeItem) records

  dataMap ∷ Maybe String >> Maybe String >> (Array Number × Array Number × Array Number)
  dataMap = foldl dataMapFoldFn M.empty items

  dataMapFoldFn
    ∷ Maybe String >> Maybe String >> (Array Number × Array Number × Array Number)
    → Item
    → Maybe String >> Maybe String >> (Array Number × Array Number × Array Number)
  dataMapFoldFn acc item = M.alter alterParallelFn item.parallel acc
    where
    xs = pure item.abscissa
    ys = pure item.ordinate
    rs = pure item.size

    alterParallelFn
      ∷ Maybe (Maybe String >> (Array Number × Array Number × Array Number))
      → Maybe (Maybe String >> (Array Number × Array Number × Array Number))
    alterParallelFn = case _ of
      Nothing → Just $ M.singleton item.series $ xs × ys × rs
      Just parallel → Just $ M.alter alterSeriesFn item.series parallel

    alterSeriesFn
      ∷ Maybe (Array Number × Array Number × Array Number)
      → Maybe (Array Number × Array Number × Array Number)
    alterSeriesFn = case _ of
      Nothing → Just $ xs × ys × rs
      Just (xxs × yys × rrs) → Just $ (xxs ⊕ xs) × (yys ⊕ ys) × (rrs ⊕ rs)

  rawSeries ∷ Array ScatterSeries
  rawSeries = foldMap (pure ∘ mkScatterSeries) $ M.toList dataMap

  series ∷ Array ScatterSeries
  series = BCP.adjustRectangularPositions rawSeries

  mkScatterSeries
    ∷ Maybe String × (Maybe String >> (Array Number × Array Number × Array Number))
    → ScatterSeries
  mkScatterSeries (name × mp) =
    { name
    , x: Nothing
    , y: Nothing
    , w: Nothing
    , h: Nothing
    , fontSize: Nothing
    , series: foldMap (pure ∘ mkOneGrid) $ M.toList mp
    }

  mkOneGrid
    ∷ Maybe String × (Array Number × Array Number × Array Number)
    → OnOneGrid
  mkOneGrid (name × is) =
    { name
    , items: adjustSymbolSizes $ mkScatterItem is
    }

  mkScatterItem
    ∷ Array Number × Array Number × Array Number
    → Array { x ∷ Number, y ∷ Number, r ∷ Number }
  mkScatterItem (xs × ys × rs)
    | A.null xs = []
    | A.null ys = []
    | otherwise =
      let
        len =
          max (A.length xs) $ max (A.length ys) (A.length rs)

        abscissas =
          case r.abscissa ^? D._value ∘ D._transform ∘ _Just ∘ T._Aggregation of
            Nothing → xs
            Just ag →
              let
                v = Ag.runAggregation ag xs
              in
                map (const v) $ A.range 0 $ len - 1
        ordinates =
          case r.ordinate ^? D._value ∘ D._transform ∘ _Just ∘ T._Aggregation of
            Nothing → ys
            Just ag →
              let
                v = Ag.runAggregation ag ys
              in
                map (const v) $ A.range 0 $ len - 1

        sizes =
          case r.size ^? _Just ∘ D._value ∘ D._transform ∘ _Just ∘ T._Aggregation of
            Just ag →
              let
                v = Ag.runAggregation ag rs
              in
                map (const v) $ A.range 0 $ len - 1
            Nothing
              | A.null rs →
                map (\_ → r.minSize) rs
            Nothing →
              rs
        zipped = A.zip abscissas $ A.zip ordinates sizes
      in
        zipped <#> \(x × y × r) → {x, y, r}

  adjustSymbolSizes
    ∷ Array {x ∷ Number, y ∷ Number, r ∷ Number}
    → Array {x ∷ Number, y ∷ Number, r ∷ Number}
  adjustSymbolSizes is =
    let
      minValue =
        fromMaybe (-1.0 * infinity) $ map _.r $ F.maximumBy (\a b → compare a.r b.r) is
      maxValue =
        fromMaybe infinity $ map _.r $ F.maximumBy (\a b → compare a.r b.r) is
      distance =
        maxValue - minValue
      sizeDistance =
        r.maxSize - r.minSize

      relativeSize ∷ Number → Number
      relativeSize val
        | distance ≡ zero = val
        | otherwise =
            r.maxSize - sizeDistance / distance * (maxValue - val)
    in
      map (\x → x{r = relativeSize x.r}) is

scatterOptions ∷ ModelR → Array ScatterSeries → DSL OptionI
scatterOptions r scatterData = do
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
    E.triggerAxis
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
      E.buildItems $ for_ serie.items \item → E.addItem $ E.buildValues do
        E.addValue item.x
        E.addValue item.y
        E.addValue item.r
