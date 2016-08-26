module SlamData.Workspace.Card.Chart.BuildOptions.Graph where

import SlamData.Prelude

import Data.Argonaut (JArray, JCursor)
import Data.Array as A
import Data.Int as Int
import Data.Map as M

import ECharts.Monad (DSL)
import ECharts.Commands as E
import ECharts.Types as ET
import ECharts.Types.Phantom (OptionI)
import ECharts.Types.Phantom as ETP

import SlamData.Workspace.Card.Chart.Aggregation (Aggregation)
import SlamData.Workspace.Card.Chart.Axis (Axis, Axes, analyzeJArray)
import SlamData.Workspace.Card.Chart.BuildOptions.ColorScheme (colors)

type GraphR =
  { source ∷ JCursor
  , target ∷ JCursor
  , size ∷ Maybe JCursor
  , sizeAggregation ∷ Maybe Aggregation
  , color ∷ Maybe JCursor
  , minSize ∷ Number
  , maxSize ∷ Number
  , circular ∷ Boolean
  , axes ∷ Axes
  }


type EdgeItem =
  (Number × Number) ⊹ (String × String)

type GraphItem =
  { size ∷ Number
  , category ∷ String
  , name ∷ String
  }

type GraphData = Array GraphItem × Array EdgeItem

buildGraphData ∷ M.Map JCursor Axis → GraphR → GraphData
buildGraphData _ _ = [ ] × [ ]

buildGraph ∷ GraphR → JArray → DSL OptionI
buildGraph r records = do
  E.tooltip do
    E.triggerItem
    E.textStyle do
      E.fontFamily "Ubuntu, sans"
      E.fontSize 12

  E.legend do
    E.orient ET.Vertical
    E.textStyle $ E.fontFamily "Ubuntu, sans"
    E.items $ map ET.strItem legendNames

  E.colors colors

  E.series $ E.graph do
    if r.circular
      then E.layoutCircular
      else E.layoutForce

    E.force do
      E.edgeLength 50.0
      E.repulsion 100.0
      E.gravity 0.1
      E.layoutAnimation true

    E.circular do
      E.rotateLabel true

    E.buildItems items
    E.buildLinks links

  where
  axisMap ∷ M.Map JCursor Axis
  axisMap = analyzeJArray records

  graphData ∷ GraphData
  graphData = buildGraphData axisMap r

  legendNames ∷ Array String
  legendNames = A.nub $ map _.category $ fst graphData

  links ∷ DSL ETP.LinksI
  links = for_ (snd graphData) case _ of
    Left (sIx × tIx) → E.addLink do
      traverse_ E.sourceIx $ Int.fromNumber sIx
      traverse_ E.targetIx $ Int.fromNumber tIx
    Right (sName × tName) → E.addLink do
      E.sourceName sName
      E.targetName tName

  items ∷ DSL ETP.ItemsI
  items = for_ (fst graphData) \item → E.addItem do
    for_ (A.elemIndex item.category legendNames) E.category
    E.symbolSize $ Int.floor item.size
    E.value item.size
    E.name item.name
