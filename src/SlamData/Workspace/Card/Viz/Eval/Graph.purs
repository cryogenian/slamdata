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

module SlamData.Workspace.Card.Viz.Eval.Graph where

import SlamData.Prelude

import Data.Argonaut (JArray, Json, decodeJson, (.?))
import Data.Array as A
import Data.Foldable as F
import Data.Foreign as FR
import Data.Function (on)
import Data.Int as Int
import Data.Lens ((?~))
import Data.Variant as V
import ECharts.Commands as E
import ECharts.Monad (DSL)
import ECharts.Types as ET
import ECharts.Types.Phantom (OptionI)
import ECharts.Types.Phantom as ETP
import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.Error as CE
import SlamData.Workspace.Card.Eval.Common as CEC
import SlamData.Workspace.Card.Eval.Monad as CEM
import SlamData.Workspace.Card.Port as Port
import SlamData.Workspace.Card.Setups.Auxiliary.Graph as Graph
import SlamData.Workspace.Card.Setups.Axis (Axes)
import SlamData.Workspace.Card.Setups.ColorScheme (colors)
import SlamData.Workspace.Card.Setups.Common.Tooltip as CCT
import SlamData.Workspace.Card.Setups.Dimension as D
import SlamData.Workspace.Card.Setups.DimensionMap.Projection as P
import SlamData.Workspace.Card.Setups.Semantics as Sem
import SlamData.Workspace.Card.Setups.Viz.Eval.Common (VizEval)
import SlamData.Workspace.Card.Viz.Model as M
import SqlSquared as Sql
import Utils (hush')
import Utils.SqlSquared (all, asRel, variRelation)

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


eval
  ∷ ∀ m
  . VizEval m
  ( M.ChartModel
  → Port.ChartInstructionsPort
  → m ( Port.Resource × DSL OptionI )
  )
eval m { chartType, dimMap, aux, axes } = do
  var × resource ← CEM.extractResourcePair Port.Initial

  aux' ←
    maybe
      (CE.throw "Missing or incorrect auxiliary model, please contact support")
      pure
      $ V.prj CT._graph =<< aux

  let
    sql = buildSql (M.getEvents m) var

  CEM.CardEnv { path, varMap } ← ask

  outResource ←
    CE.liftQ $ CEC.localEvalResource (Sql.Query empty sql) varMap
  records ←
    CE.liftQ $ CEC.sampleResource path outResource Nothing

  let
    items = buildData aux' records
    options = buildOptions dimMap axes aux' items
  pure $ outResource × options

buildSql ∷ Array M.FilteredEvent → Port.Var → Sql.Sql
buildSql es var =
  Sql.buildSelect
  $ all
  ∘ (Sql._relations
     ?~ (variRelation (unwrap var) # asRel "res"))

buildData ∷ Graph.State → JArray → GraphData
buildData r jarr =
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

    rMinSize = r.size.min
    rMaxSize = r.size.max

    sizeDistance ∷ Number
    sizeDistance = rMaxSize - rMinSize

    relativeSize ∷ Number → Number
    relativeSize val
      | distance ≡ zero = val
      | val < 0.0 = 0.0
      | otherwise =
        rMaxSize - sizeDistance / distance * (maxSize - val)

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

buildOptions ∷ P.DimMap → Axes → Graph.State → GraphData → DSL OptionI
buildOptions dimMap axes r (links × nodes) = do
  CCT.tooltip do
    E.triggerItem
    E.formatterItem \{name, value, "data": item, dataType} →
      if dataType ≡ "edge" then ""
      else
        let
          mbVal = map show $ hush' $ FR.readNumber value
          sourceLabel = foldMap D.jcursorLabel $ P.lookup P.source dimMap
          sourceTag
            | sourceLabel ≠ "" = sourceLabel
            | otherwise = "name"
          measureLabel = foldMap D.jcursorLabel $ P.lookup P.size dimMap
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
    when (P.member P.color dimMap)
      $ E.category node.category
    when (P.member P.size dimMap) do
      E.symbolSize $ Int.floor node.size
      E.value node.value
