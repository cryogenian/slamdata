module SlamData.Workspace.Card.BuildChart.Area.Component.State where

import SlamData.Prelude

import Halogen (ParentState)

import SlamData.Monad (Slam)
import SlamData.Workspace.LevelOfDetails (LevelOfDetails(..))
import SlamData.Workspace.Card.BuildChart.Area.Component.ChildSlot as CS
import SlamData.Workspace.Card.BuildChart.Area.Component.Query (QueryC)
import SlamData.Workspace.Card.BuildChart.Axis (Axes, initialAxes)

type State =
   { axes ∷ Axes
   , levelOfDetails ∷ LevelOfDetails
   , axisLabelAngle ∷ Number
   , axisLabelFontSize ∷ Int
   , isStacked ∷ Boolean
   , isSmooth ∷ Boolean
   }

initialState ∷ State
initialState =
  { axes: initialAxes
  , levelOfDetails: High
  , axisLabelAngle: zero
  , axisLabelFontSize: zero
  , isStacked: false
  , isSmooth: false
  }

type StateP =
  ParentState State CS.ChildState QueryC CS.ChildQuery Slam CS.ChildSlot
