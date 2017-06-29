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

module SlamData.Workspace.Card.Setups.Chart.Graph.Eval
  ( eval
  , module SlamData.Workspace.Card.Setups.Chart.Graph.Model
  ) where

import SlamData.Prelude

import Data.Argonaut (JArray, Json, decodeJson, (.?))
import Data.Array as A
import Data.Foldable as F
import Data.Foreign as FR
import Data.Function (on)
import Data.Int as Int
import Data.List as L

import ECharts.Monad (DSL)
import ECharts.Commands as E
import ECharts.Types.Phantom (OptionI)
import ECharts.Types as ET
import ECharts.Types.Phantom as ETP

import SlamData.Workspace.Card.CardType.ChartType (ChartType(Graph))
import SlamData.Workspace.Card.Port as Port
import SlamData.Workspace.Card.Setups.Chart.ColorScheme (colors)
import SlamData.Workspace.Card.Setups.Chart.Common as SCC
import SlamData.Workspace.Card.Setups.Chart.Graph.Model (Model, ModelR)
import SlamData.Workspace.Card.Setups.Common.Eval as BCE
import SlamData.Workspace.Card.Setups.Axis (Axes)
import SlamData.Workspace.Card.Setups.Dimension as D
import SlamData.Workspace.Card.Setups.Semantics as Sem

import SqlSquared as Sql

import Utils (hush')

type Item =
  { source ∷ String
  , target ∷ String
  , size ∷ Maybe Number
  , color ∷ Maybe String
  }

type Node =
  { name ∷ String
  , value ∷ Number
  , size ∷ Number
  , category ∷ Int
  }

type GraphData = Array Item × Array Node

decodeItem ∷ Json → String ⊹ Item
decodeItem = decodeJson >=> \obj → do
  source ← map (fromMaybe "" ∘ Sem.maybeString) $ obj .? "source"
  target ← map (fromMaybe "" ∘ Sem.maybeString) $ obj .? "target"
  size ← map Sem.maybeNumber $ obj .? "size"
  color ← map Sem.maybeString $ obj .? "color"
  pure { source, target, size, color }

eval ∷ ∀ m v. BCE.ChartSetupEval ModelR m v
eval = BCE.chartSetupEval (SCC.buildBasicSql buildProjections buildGroupBy) buildGraph

buildProjections ∷ ModelR → L.List (Sql.Projection Sql.Sql)
buildProjections r = L.fromFoldable
  [ r.source # SCC.jcursorPrj # Sql.as "source"
  , r.target # SCC.jcursorPrj # Sql.as "target"
  , r.size # maybe SCC.nullPrj (\s → SCC.applyTransform s $ SCC.jcursorPrj s) # Sql.as "size"
  , r.color # maybe SCC.nullPrj SCC.jcursorPrj # Sql.as "color"
  ]

buildGroupBy ∷ ModelR → Maybe (Sql.GroupBy Sql.Sql)
buildGroupBy r =
  SCC.groupBy $ L.fromFoldable $ A.catMaybes
    [ Just $ r.source # SCC.jcursorSql
    , Just $ r.target # SCC.jcursorSql
    , r.color <#> SCC.jcursorSql
    ]

buildGraph ∷ ModelR → Axes → Port.Port
buildGraph m axes =
  Port.ChartInstructions
    { options: graphOptions axes m ∘ buildGraphData m
    , chartType: Graph
    }

buildGraphData ∷ ModelR → JArray → GraphData
buildGraphData r jarr =
  let
    items ∷ Array Item
    items = foldMap (foldMap A.singleton ∘ decodeItem) jarr

    colors ∷ Array String
    colors = A.nub $ A.mapMaybe _.color items

    colorFn
      ∷ Array Item
      → Array { size ∷ Maybe Number, source ∷ String, target ∷ String, index ∷ Int }
    colorFn = map \item →
      let index = fromMaybe zero $ item.color >>= flip A.elemIndex colors
      in { size: item.size, source: item.source, target: item.target, index }

    sizes ∷ Array Number
    sizes = A.mapMaybe _.size items

    minSize ∷ Number
    minSize = fromMaybe zero $ F.minimum sizes

    maxSize ∷ Number
    maxSize = fromMaybe zero $ F.maximum sizes

    distance ∷ Number
    distance = maxSize - minSize

    sizeDistance ∷ Number
    sizeDistance = r.maxSize - r.minSize

    relativeSize ∷ Number → Number
    relativeSize val
      | distance ≡ zero = val
      | val < 0.0 = 0.0
      | otherwise =
        r.maxSize - sizeDistance / distance * (maxSize - val)

    itemToNodes
      ∷ { size ∷ Maybe Number, source ∷ String, target ∷ String, index ∷ Int }
      → Array Node
    itemToNodes { index, source, target, size } =
      [ { name: source
        , value: fromMaybe zero size
        , size: relativeSize $ fromMaybe zero size
        , category: index
        }
      , { name: target
        , value: fromMaybe zero size
        , size: relativeSize $ fromMaybe zero size
        , category: index
        } ]

  in
    items × (A.nubBy (eq `on` _.name) $ foldMap itemToNodes $ colorFn items)

graphOptions ∷ Axes → ModelR → GraphData → DSL OptionI
graphOptions axes r (links × nodes) = do
  E.tooltip do
    E.triggerItem
    E.textStyle do
      E.fontFamily "Ubuntu, sans"
      E.fontSize 12
    E.formatterItem \{name, value, "data": item, dataType} →
      if dataType ≡ "edge" then ""
      else
        let
          mbVal = map show $ hush' $ FR.readNumber value
          sourceLabel = D.jcursorLabel r.source
          sourceTag
            | sourceLabel ≠ "" = sourceLabel
            | otherwise = "name"
          measureLabel = foldMap D.jcursorLabel r.size
          measureTag
            | measureLabel ≠ "" = measureLabel
            | otherwise = "value"
        in
         (sourceTag ⊕ ": " ⊕ name ⊕ "<br />")
         ⊕ (foldMap (\x → measureTag ⊕ ": " ⊕ x) mbVal)

  E.legend do
    E.orient ET.Vertical
    E.leftLeft
    E.topTop
    E.textStyle $ E.fontFamily "Ubuntu, sans"
    E.items
      $ map ET.strItem
      $ A.nub
      $ A.mapMaybe _.color links

  E.colors colors

  E.series $ E.graph do
    if r.circular
      then E.layoutCircular
      else E.layoutForce

    E.force do
      E.edgeLength 120.0
      E.layoutAnimation true


    E.buildItems items
    E.links $ links <#> \{source, target} → {source, target}

    E.buildCategories
      $ for_ (A.nub $ A.mapMaybe _.color links)
      $ E.addCategory ∘ E.name

    E.lineStyle $ E.normal $ E.colorSource

  where
  items ∷ DSL ETP.ItemsI
  items = for_ nodes \node → E.addItem do
    E.name node.name
    E.itemStyle $ E.normal do
      E.borderWidth 1
    E.label do
      E.normal E.hidden
      E.emphasis E.hidden
    when (isJust r.color)
      $ E.category node.category
    when (isJust r.size) do
      E.symbolSize $ Int.floor node.size
      E.value node.value
