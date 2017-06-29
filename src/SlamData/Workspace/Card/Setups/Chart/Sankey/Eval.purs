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

module SlamData.Workspace.Card.Setups.Chart.Sankey.Eval
  ( eval
  , module SlamData.Workspace.Card.Setups.Chart.Sankey.Model
  ) where

import SlamData.Prelude

import Data.Argonaut (JArray, Json, decodeJson, (.?))
import Data.Array as A
import Data.List as L

import ECharts.Monad (DSL)
import ECharts.Commands as E
import ECharts.Types.Phantom (OptionI)
import ECharts.Types.Phantom as ETP

import SlamData.Workspace.Card.CardType.ChartType (ChartType(Sankey))
import SlamData.Workspace.Card.Port as Port
import SlamData.Workspace.Card.Setups.Axis (Axes)
import SlamData.Workspace.Card.Setups.Chart.ColorScheme (colors)
import SlamData.Workspace.Card.Setups.Chart.Common as SCC
import SlamData.Workspace.Card.Setups.Chart.Common.Tooltip as CCT
import SlamData.Workspace.Card.Setups.Chart.Sankey.Model (Model, ModelR)
import SlamData.Workspace.Card.Setups.Common.Eval as BCE
import SlamData.Workspace.Card.Setups.Dimension as D
import SlamData.Workspace.Card.Setups.Semantics as Sem

import SqlSquared as Sql

type Item =
  { source ∷ String
  , target ∷ String
  , weight ∷ Number
  }

decodeItem ∷ Json → String ⊹ Item
decodeItem = decodeJson >=> \obj → do
  source ← map (fromMaybe "" ∘ Sem.maybeString) $ obj .? "source"
  target ← map (fromMaybe "" ∘ Sem.maybeString) $ obj .? "target"
  weight ← map (fromMaybe zero ∘ Sem.maybeNumber) $ obj .? "weight"
  pure { source, target, weight }

eval ∷ ∀ m v. BCE.ChartSetupEval ModelR m v
eval = BCE.chartSetupEval (SCC.buildBasicSql buildProjections buildGroupBy) buildSankey

buildProjections ∷ ModelR → L.List (Sql.Projection Sql.Sql)
buildProjections r = L.fromFoldable
  [ r.source # SCC.jcursorPrj # Sql.as "source"
  , r.target # SCC.jcursorPrj # Sql.as "target"
  , r.value # SCC.jcursorPrj # Sql.as "weight"
  ]

buildGroupBy ∷ ModelR → Maybe (Sql.GroupBy Sql.Sql)
buildGroupBy r =
  SCC.groupBy $ L.fromFoldable
    [ r.source # SCC.jcursorSql
    , r.target # SCC.jcursorSql
    ]

buildSankey ∷ ModelR → Axes → Port.Port
buildSankey m _ =
  Port.ChartInstructions
    { options: sankeyOptions m ∘ buildSankeyData
    , chartType: Sankey
    }

buildSankeyData ∷ JArray → Array Item
buildSankeyData =
  foldMap $ foldMap A.singleton ∘ decodeItem

sankeyOptions ∷ ModelR → Array Item → DSL OptionI
sankeyOptions r sankeyData = do
  let
    cols =
      [ { label: D.jcursorLabel r.source, value: CCT.formatDataProp "source" }
      , { label: D.jcursorLabel r.target, value: CCT.formatDataProp "target" }
      , { label: D.jcursorLabel r.value, value: CCT.formatDataProp "value" }
      ]
  E.tooltip do
    E.formatterItem (CCT.tableFormatter (const Nothing) cols ∘ pure)
    E.triggerItem
    E.textStyle do
      E.fontFamily "Ubuntu, sans"
      E.fontSize 12

  E.colors colors

  E.series $ E.sankey do
    E.buildItems items
    E.buildLinks links

    E.lineStyle $ E.normal $ E.curveness 0.3
  where
  links ∷ DSL ETP.LinksI
  links = for_ sankeyData \item → E.addLink do
    E.sourceName item.source
    E.targetName item.target
    E.value item.weight

  items ∷ DSL ETP.ItemsI
  items =
    for_
      (A.nub $ (_.source <$> sankeyData) ⊕ (_.target <$> sankeyData))
      (E.addItem ∘ E.name)
