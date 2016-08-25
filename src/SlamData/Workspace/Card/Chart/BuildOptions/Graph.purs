module SlamData.Workspace.Card.Chart.BuildOptions.Graph where

import SlamData.Prelude

import Data.Argonaut (JArray, JCursor)

import ECharts.Monad (DSL)
import ECharts.Commands as E
import ECharts.Types as ET
import ECharts.Types.Phantom (OptionI)
--import ECharts.Types.Phantom as ETP

import SlamData.Workspace.Card.Chart.Aggregation (Aggregation)
import SlamData.Workspace.Card.Chart.VisualMapColor (VisualMapColor)
import SlamData.Workspace.Card.Chart.Axis (analyzeJArray)
import SlamData.Workspace.Card.Chart.BuildOptions.ColorScheme (colors)

type GraphR =
  { source ∷ JCursor
  , target ∷ JCursor
  , size ∷ Maybe JCursor
  , color ∷ Maybe JCursor
  , sizeAggregation ∷ Maybe Aggregation
  , colorAggregation ∷ Maybe Aggregation
  , vmStart ∷ Maybe VisualMapColor
  , vmEnd ∷ Maybe VisualMapColor
  , minSize ∷ Number
  , maxSize ∷ Number
  }


buildGraph ∷ GraphR → JArray → DSL OptionI
buildGraph r records = do
  let axisMap = analyzeJArray records

  E.tooltip do
    E.triggerItem
    E.textStyle do
      E.fontFamily "Ubuntu, sans"
      E.fontSize 12

  E.legend do
    E.orient ET.Vertical
    E.textStyle $ E.fontFamily "Ubuntu, sans"
    E.items $ map ET.strItem legendNames

  E.title $ E.text "Graph"

  E.colors colors

  E.series $ E.graph do
    pure unit

  where
  legendNames ∷ Array String
  legendNames = [ ]
