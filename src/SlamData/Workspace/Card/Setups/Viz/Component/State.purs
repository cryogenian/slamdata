module SlamData.Workspace.Card.Setups.Viz.Component.State where

import SlamData.Prelude

import SlamData.Workspace.Card.CardType.VizType as VT
import SlamData.Workspace.Card.Setups.Axis as Ax

type State =
  { vizType ∷ VT.VizType
  , vizTypePickerExpanded ∷ Boolean
  , axes ∷ Maybe Ax.Axes
  }


initialState ∷ State
initialState =
  { vizType: VT.Chart VT.Pie
  , vizTypePickerExpanded: false
  , axes: Nothing
  }
