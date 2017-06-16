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

module SlamData.Workspace.Card.Setups.Chart.Funnel.Eval
  ( eval
  , module SlamData.Workspace.Card.Setups.Chart.Funnel.Model
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

import SlamData.Common.Sort (Sort(..))
import SlamData.Common.Align (Align(..))
import SlamData.Workspace.Card.Setups.Axis (Axes)
import SlamData.Workspace.Card.Setups.Common.Eval (type (>>))
import SlamData.Workspace.Card.Setups.Common.Eval as BCE
import SlamData.Workspace.Card.Setups.Chart.Common as SCC
import SlamData.Workspace.Card.Setups.Chart.Funnel.Model (Model, ModelR)
import SlamData.Workspace.Card.CardType.ChartType (ChartType(Funnel))
import SlamData.Workspace.Card.Setups.Chart.ColorScheme (colors)
import SlamData.Workspace.Card.Setups.Chart.Common.Positioning as BCP
import SlamData.Workspace.Card.Setups.Chart.Common.Tooltip as CCT
import SlamData.Workspace.Card.Setups.Dimension as D
import SlamData.Workspace.Card.Setups.Semantics as Sem
import SlamData.Workspace.Card.Port as Port

import SqlSquared as Sql

eval ∷ ∀ m v. BCE.ChartSetupEval ModelR m v
eval = BCE.chartSetupEval (SCC.buildBasicSql buildProjections buildGroupBy) buildPort

type FunnelSeries =
  { name ∷ Maybe String
  , items ∷ String >> Number
  , x ∷ Maybe Number
  , y ∷ Maybe Number
  , w ∷ Maybe Number
  , h ∷ Maybe Number
  , fontSize ∷ Maybe Int
  }

type Item =
  { category ∷ String
  , measure ∷ Number
  , series ∷ Maybe String
  }

decodeItem ∷ Json → Either String Item
decodeItem = decodeJson >=> \obj → do
  category ← Sem.requiredString "" <$> obj .? "category"
  measure ← Sem.requiredNumber zero <$> obj .? "measure"
  series ← Sem.maybeString <$> obj .? "series"
  pure { category, measure, series }

buildProjections ∷ ModelR → L.List (Sql.Projection Sql.Sql)
buildProjections r = L.fromFoldable
  [ r.category # SCC.jcursorPrj # Sql.as "category"
  , r.value # SCC.jcursorPrj # Sql.as "measure" # SCC.applyTransform r.value
  , r.series # maybe SCC.nullPrj SCC.jcursorPrj # Sql.as "series"
  ]

buildGroupBy ∷ ModelR → Maybe (Sql.GroupBy Sql.Sql)
buildGroupBy r =
  SCC.groupBy $ L.fromFoldable $ A.catMaybes
    [ r.series <#> SCC.jcursorSql
    , Just r.category <#> SCC.jcursorSql
    ]

buildPort ∷ ModelR → Axes → Port.Port
buildPort m _ =
  Port.ChartInstructions
    { options: buildOptions m ∘ buildData m
    , chartType: Funnel
    }

buildData ∷ ModelR → Array Json → Array FunnelSeries
buildData r =
  foldMap (foldMap A.singleton ∘ decodeItem)
    >>> series
    >>> BCP.adjustRectangularPositions

  where
  series ∷ Array Item → Array FunnelSeries
  series =
    BCE.groupOn _.series
      >>> map \(name × items) →
            { name
            , x: Nothing
            , y: Nothing
            , w: Nothing
            , h: Nothing
            , fontSize: Nothing
            , items: M.fromFoldable $ toPoint <$> items
            }

  toPoint ∷ Item → String × Number
  toPoint item = item.category × item.measure

buildOptions ∷ ModelR → Array FunnelSeries → DSL OptionI
buildOptions r funnelData = do
  let
    cols =
      [ { label: D.jcursorLabel r.category, value: _.name }
      , { label: D.jcursorLabel r.value, value: CCT.formatForeign ∘ _.value }
      ]
    opts = flip foldMap r.series \dim →
      [ { label: D.jcursorLabel dim, value: _.seriesName } ]

  E.tooltip do
    E.formatterItem (CCT.tableFormatter (pure ∘ _.color) (cols <> opts) ∘ pure)
    E.triggerItem
    E.textStyle do
      E.fontFamily "Ubuntu, sans"
      E.fontSize 12

  E.legend do
    E.items $ map ET.strItem legendNames
    E.topBottom
    E.textStyle do
      E.fontFamily "Ubuntu, sans"

  E.colors colors

  BCP.rectangularTitles funnelData
    $ maybe "" D.jcursorLabel r.series
  E.series series

  where
  legendNames ∷ Array String
  legendNames =
    A.fromFoldable
      $ foldMap (_.items ⋙ M.keys ⋙ Set.fromFoldable) funnelData

  series ∷ ∀ i. DSL (funnel ∷ ETP.I|i)
  series = for_ funnelData \{x, y, w, h, items, name: serieName} → E.funnel do
    traverse_ E.widthPct w
    traverse_ E.heightPct h
    for_ serieName E.name
    case r.order of
      Asc → E.ascending
      Desc → E.descending
    case r.align of
      LeftAlign → E.funnelLeft
      RightAlign → E.funnelRight
      CenterAlign → E.funnelCenter
    E.label do
      E.normal do
        E.textStyle $ E.fontFamily "Ubuntu, sans"
        E.positionInside
      E.emphasis do
        E.textStyle $ E.fontFamily "Ubuntu, sans"
        E.positionInside
    E.buildItems $ for_ (asList $ M.toUnfoldable items) \(name × value) → E.addItem do
      E.name name
      E.value value
    traverse_ (E.top ∘ ET.Percent) y
    traverse_ (E.left ∘ ET.Percent) x
