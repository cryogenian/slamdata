module SlamData.Workspace.Card.BuildChart.Heatmap.Component.State where

import SlamData.Prelude

import Halogen (ParentState)

import SlamData.Monad (Slam)
import SlamData.Workspace.LevelOfDetails (LevelOfDetails(..))
import SlamData.Workspace.Card.BuildChart.Heatmap.Component.ChildSlot as CS
import SlamData.Workspace.Card.BuildChart.Heatmap.Component.Query (QueryC)
import SlamData.Workspace.Card.Chart.Axis (Axes, initialAxes)

type State =
   { axes ∷ Axes
   , levelOfDetails ∷ LevelOfDetails
   , minValue ∷ Number
   , maxValue ∷ Number
   , isSchemeReversed ∷ Boolean
   }

initialState ∷ State
initialState =
  { axes: initialAxes
  , levelOfDetails: High
  , minValue: zero
  , maxValue: one
  , isSchemeReversed: false
  }

type StateP =
  ParentState State CS.ChildState QueryC CS.ChildQuery Slam CS.ChildSlot
