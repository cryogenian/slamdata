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

module SlamData.Workspace.Card.Setups.Chart.Pie.Eval
  ( eval
  , module SlamData.Workspace.Card.Setups.Chart.Pie.Model
  ) where

import SlamData.Prelude

import Data.Argonaut (Json, decodeJson, (.?))
import Data.Array as A
import Data.List as L
import Data.Map as M
import Data.Set as Set

import ECharts.Monad (DSL)
import ECharts.Commands as E
import ECharts.Types as ET
import ECharts.Types.Phantom (OptionI)
import ECharts.Types.Phantom as ETP

import SlamData.Workspace.Card.CardType.ChartType (ChartType(Pie))
import SlamData.Workspace.Card.Port as Port
import SlamData.Workspace.Card.Setups.Axis (Axes)
import SlamData.Workspace.Card.Setups.Chart.ColorScheme (colors)
import SlamData.Workspace.Card.Setups.Chart.Common as SCC
import SlamData.Workspace.Card.Setups.Chart.Common.Positioning (adjustRadialPositions, adjustDonutRadiuses, RadialPosition, WithDonutRadius, radialTitles)
import SlamData.Workspace.Card.Setups.Chart.Common.Tooltip as CCT
import SlamData.Workspace.Card.Setups.Chart.Pie.Model (Model, ModelR)
import SlamData.Workspace.Card.Setups.Common.Eval (type (>>))
import SlamData.Workspace.Card.Setups.Common.Eval as BCE
import SlamData.Workspace.Card.Setups.Dimension as D
import SlamData.Workspace.Card.Setups.Semantics as Sem

import SqlSquared as Sql

eval ∷ ∀ m v. BCE.ChartSetupEval ModelR m v
eval = BCE.chartSetupEval (SCC.buildBasicSql buildProjections buildGroupBy) buildPort

type OnePieSeries =
  RadialPosition
  ( series ∷ Array DonutSeries
  , name ∷ Maybe String
  )

type DonutSeries =
  WithDonutRadius
  ( name ∷ Maybe String
  , items ∷ String >> Number
  )

type Item =
  { category ∷ String
  , measure ∷ Number
  , donut ∷ Maybe String
  , parallel ∷ Maybe String
  }

decodeItem ∷ Json → Either String Item
decodeItem = decodeJson >=> \obj → do
  category ← Sem.requiredString "" <$> obj .? "category"
  measure ← Sem.requiredNumber zero <$> obj .? "measure"
  donut ← Sem.maybeString <$> obj .? "donut"
  parallel ← Sem.maybeString <$> obj .? "parallel"
  pure { category, measure, donut, parallel }

buildProjections ∷ ModelR → L.List (Sql.Projection Sql.Sql)
buildProjections r =
  L.fromFoldable
    [ r.category # SCC.jcursorPrj # Sql.as "category"
    , r.value # SCC.jcursorPrj # Sql.as "measure" # SCC.applyTransform r.value
    , r.donut # maybe SCC.nullPrj SCC.jcursorPrj # Sql.as "donut"
    , r.parallel # maybe SCC.nullPrj SCC.jcursorPrj # Sql.as "parallel"
    ]

buildGroupBy ∷ ModelR → Maybe (Sql.GroupBy Sql.Sql)
buildGroupBy r =
  SCC.groupBy $ L.fromFoldable $ A.catMaybes
    [ r.parallel <#> SCC.jcursorSql
    , r.donut <#> SCC.jcursorSql
    , Just r.category <#> SCC.jcursorSql
    ]

buildPort ∷ ModelR → Axes → Port.Port
buildPort m _ =
  Port.ChartInstructions
    { options: buildOptions m ∘ buildData m
    , chartType: Pie
    }

buildData ∷ ModelR → Array Json → Array OnePieSeries
buildData r =
  foldMap (foldMap A.singleton ∘ decodeItem)
    >>> onePies
    >>> adjustPositions

  where
  adjustPositions ∷ Array OnePieSeries → Array OnePieSeries
  adjustPositions =
    adjustRadialPositions
      >>> map \pie → pie { series = adjustDonutRadiuses pie.series }

  onePies ∷ Array Item → Array OnePieSeries
  onePies =
    BCE.groupOn _.parallel
      >>> map \(name × is) →
            { name
            , x: Nothing
            , y: Nothing
            , radius: Nothing
            , series: donuts is
            }

  donuts ∷ Array Item → Array DonutSeries
  donuts =
    BCE.groupOn _.donut
      >>> map \(name × is) →
            { name
            , radius: Nothing
            , items: M.fromFoldable (toPoint <$> is)
            }

  toPoint ∷ Item → Tuple String Number
  toPoint { category, measure } = category × measure

buildOptions ∷ ModelR → Array OnePieSeries → DSL OptionI
buildOptions r pieData = do
  let
    cols =
      [ { label: D.jcursorLabel r.category, value: CCT.formatAssocProp "key" }
      , { label: D.jcursorLabel r.value, value: CCT.formatAssocProp "value" }
      ]
    opts = A.catMaybes
      [ r.donut <#> \dim → { label: D.jcursorLabel dim, value: _.seriesName }
      ]

  E.tooltip do
    E.formatterItem (CCT.tableFormatter (Just ∘ _.color) (cols <> opts) ∘ pure)
    E.textStyle $ E.fontSize 12
    E.triggerItem

  E.colors colors

  E.legend do
    E.textStyle do
      E.fontSize 12
      E.fontFamily "Ubuntu, sans"
    E.items $ map ET.strItem legendNames
    E.orient ET.Vertical
    E.leftLeft

  E.series series

  radialTitles pieData
    $ maybe "" D.jcursorLabel r.parallel

  where
  itemNames ∷ Array String
  itemNames =
    A.fromFoldable
      $ foldMap (_.series
                 ⋙ foldMap (_.items
                            ⋙ M.keys
                            ⋙ Set.fromFoldable)
                )
        pieData

  seriesNames ∷ Array String
  seriesNames =
    A.fromFoldable
      $ foldMap (_.series ⋙ foldMap (_.name ⋙ Set.fromFoldable)) pieData

  legendNames ∷ Array String
  legendNames
    | A.null seriesNames = itemNames
    | otherwise = do
      s ← seriesNames
      i ← itemNames
      pure $ s ⊕ ":" ⊕ i

  series ∷ ∀ i. DSL (pie ∷ ETP.I|i)
  series = for_ pieData \{x, y, radius: parallelR, series: ss} →
    for_ ss \{radius, items, name} → E.pie do
      E.label do
        E.normal E.hidden
        E.emphasis E.hidden

      E.buildCenter do
        traverse_ (E.setX ∘ E.percents) x
        traverse_ (E.setY ∘ E.percents) y

      for_ parallelR \pR →
        for_ radius \{start, end} → E.buildRadius do
          E.setStart $ E.percents $ start * pR
          E.setEnd $ E.percents $ end * pR

      for_ name E.name

      E.buildItems $ for_ (asList $ M.toUnfoldable $ items) \(key × value) →
        E.addItem do
          E.value value
          E.name $ foldMap (flip append ":") name ⊕ key
          BCE.assoc { key, value }
