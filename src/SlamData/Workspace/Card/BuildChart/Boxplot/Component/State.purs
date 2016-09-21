module SlamData.Workspace.Card.BuildChart.Boxplot.Component.State where

import Halogen (ParentState)

import SlamData.Monad (Slam)
import SlamData.Workspace.LevelOfDetails (LevelOfDetails(..))
import SlamData.Workspace.Card.BuildChart.Boxplot.Component.ChildSlot as CS
import SlamData.Workspace.Card.BuildChart.Boxplot.Component.Query (QueryC)
import SlamData.Workspace.Card.Chart.Axis (Axes, initialAxes)

type State =
   { axes ∷ Axes
   , levelOfDetails ∷ LevelOfDetails
   }

initialState ∷ State
initialState =
  { axes: initialAxes
  , levelOfDetails: High
  }

type StateP =
  ParentState State CS.ChildState QueryC CS.ChildQuery Slam CS.ChildSlot
