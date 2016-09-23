module SlamData.Workspace.Card.BuildChart.Line.Component.State where

import SlamData.Prelude

import Halogen (ParentState)

import SlamData.Monad (Slam)
import SlamData.Workspace.LevelOfDetails (LevelOfDetails(..))
import SlamData.Workspace.Card.BuildChart.Line.Component.ChildSlot as CS
import SlamData.Workspace.Card.BuildChart.Line.Component.Query (QueryC)
import SlamData.Workspace.Card.BuildChart.Axis (Axes, initialAxes)

type State =
   { axes ∷ Axes
   , levelOfDetails ∷ LevelOfDetails
   , axisLabelAngle ∷ Number
   , axisLabelFontSize ∷ Int
   , minSize ∷ Number
   , maxSize ∷ Number
   }

initialState ∷ State
initialState =
  { axes: initialAxes
  , levelOfDetails: High
  , axisLabelAngle: zero
  , axisLabelFontSize: zero
  , minSize: 2.0
  , maxSize: 20.0
  }

type StateP =
  ParentState State CS.ChildState QueryC CS.ChildQuery Slam CS.ChildSlot
