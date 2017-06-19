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

module SlamData.Workspace.Card.Setups.Chart.Gauge.Eval
  ( eval
  , module SlamData.Workspace.Card.Setups.Chart.Gauge.Model
  ) where

import SlamData.Prelude

import Data.Argonaut (Json, decodeJson, (.?))
import Data.Array as A
import Data.Foldable as F
import Data.List as L

import ECharts.Monad (DSL)
import ECharts.Commands as E
import ECharts.Types.Phantom (OptionI)

import SlamData.Workspace.Card.CardType.ChartType (ChartType(Gauge))
import SlamData.Workspace.Card.Port as Port
import SlamData.Workspace.Card.Setups.Axis as Ax
import SlamData.Workspace.Card.Setups.Chart.ColorScheme (colors)
import SlamData.Workspace.Card.Setups.Chart.Common as SCC
import SlamData.Workspace.Card.Setups.Chart.Common.Positioning as BCP
import SlamData.Workspace.Card.Setups.Chart.Common.Tooltip as CCT
import SlamData.Workspace.Card.Setups.Chart.Gauge.Model (Model, ModelR)
import SlamData.Workspace.Card.Setups.Common.Eval as BCE
import SlamData.Workspace.Card.Setups.Dimension as D
import SlamData.Workspace.Card.Setups.Semantics as Sem

import SqlSquared as Sql

eval ∷ ∀ m v. BCE.ChartSetupEval ModelR m v
eval = BCE.chartSetupEval (SCC.buildBasicSql buildProjections buildGroupBy) buildPort

type GaugeItem =
  { name ∷ Maybe String
  , value ∷ Number
  }

type GaugeSerie =
  BCP.RadialPosition
  ( name ∷ Maybe String
  , items ∷ Array GaugeItem
  )

type Item =
  { measure ∷ Number
  , parallel ∷ Maybe String
  , multiple ∷ Maybe String
  }

decodeItem ∷ Json → Either String Item
decodeItem = decodeJson >=> \obj → do
  parallel ← Sem.maybeString <$> obj .? "parallel"
  multiple ← Sem.maybeString <$> obj .? "multiple"
  measure ← Sem.requiredNumber zero <$> obj .? "measure"
  pure { measure, parallel, multiple }

buildProjections ∷ ModelR → L.List (Sql.Projection Sql.Sql)
buildProjections r = L.fromFoldable
  [ r.value # SCC.jcursorPrj # Sql.as "measure" # SCC.applyTransform r.value
  , r.multiple # maybe SCC.nullPrj SCC.jcursorPrj # Sql.as "multiple"
  , r.parallel # maybe SCC.nullPrj SCC.jcursorPrj # Sql.as "parallel"
  ]

buildGroupBy ∷ ModelR → Maybe (Sql.GroupBy Sql.Sql)
buildGroupBy r =
  SCC.groupBy $ L.fromFoldable $ A.catMaybes
    [ r.parallel <#> SCC.jcursorSql
    , r.multiple <#> SCC.jcursorSql
    ]

buildPort ∷ ModelR → Ax.Axes → Port.Port
buildPort m _ =
  Port.ChartInstructions
    { options: buildOptions m ∘ buildData m
    , chartType: Gauge
    }

buildData ∷ ModelR → Array Json → Array GaugeSerie
buildData r =
  foldMap (foldMap A.singleton ∘ decodeItem)
    >>> series
    >>> BCP.adjustRadialPositions

  where
  series ∷ Array Item → Array GaugeSerie
  series =
    BCE.groupOn _.parallel
      >>> map \(name × items) →
            { name
            , x: Nothing
            , y: Nothing
            , radius: Nothing
            , items: toPoint <$> items
            }

  toPoint ∷ Item → GaugeItem
  toPoint item = { name: item.multiple, value: item.measure }

buildOptions ∷ ModelR → Array GaugeSerie → DSL OptionI
buildOptions r series = do
  let
    cols =
      [ { label: D.jcursorLabel r.value, value: CCT.formatForeign ∘ _.value }
      ]
    opts = A.catMaybes
      [ r.parallel <#> \dim → { label: D.jcursorLabel dim, value: _.seriesName }
      , r.multiple <#> \dim → { label: D.jcursorLabel dim, value: _.name }
      ]

  E.tooltip do
    E.formatterItem (CCT.tableFormatter (const Nothing) (cols <> opts) ∘ pure)
    E.textStyle do
      E.fontFamily "Ubuntu, sans"
      E.fontSize 12

  E.colors colors

  E.series $ for_ series \serie → E.gauge do
    for_ serie.name E.name
    E.axisLine $ E.lineStyle $ E.setWidth $ E.pixels 10
    E.splitLine $ E.length 20
    traverse_ (E.buildGaugeRadius ∘ E.percents) serie.radius

    E.buildCenter do
      traverse_ (E.setX ∘ E.percents) serie.x
      traverse_ (E.setY ∘ E.percents) serie.y

    when (A.length serie.items > 1)
      $ E.title E.hidden

    E.detail do
      traverse_ E.formatterString serie.name
      when (A.length series < 2 ∧ A.length serie.items > 1) E.hidden
      E.buildOffsetCenter do
        E.setX $ E.percents zero
        E.setY $ E.percents 65.0
      E.textStyle do
        E.fontSize 16
        E.fontFamily "Ubuntu, sans"
        for_ (A.head colors) E.color


    if (A.length allValues > 1)
      then do
      for_ (F.minimum allValues) E.min
      for_ (F.maximum allValues) E.max
      else
      for_ (A.head allValues) \v → do
        E.min $ v / 2.0
        E.max $ v * 1.5

    E.buildItems
      $ for_ serie.items \item → E.addItem do
        E.value item.value
        traverse_ E.name item.name

  where
  allValues ∷ Array Number
  allValues = map _.value $ A.concatMap _.items series
