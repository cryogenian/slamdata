module SlamData.Workspace.Card.Setups.Viz.Component.State where

import SlamData.Prelude

import Data.Map as Map

import SlamData.Workspace.Card.CardType.VizType as VT
import SlamData.Workspace.Card.Setups.Axis as Ax
import SlamData.Workspace.Card.Setups.Package.Types as T

type State =
  { vizType ∷ VT.VizType
  , vizTypePickerExpanded ∷ Boolean
  , axes ∷ Maybe Ax.Axes
  , dimMaps ∷ Map.Map VT.VizType T.DimensionMap
  }


initialState ∷ State
initialState =
  { vizType: VT.Chart VT.Pie
  , vizTypePickerExpanded: false
  , axes: Nothing
  , dimMaps: Map.fromFoldable $ map (_ × T.emptyDimMap) VT.all
  }
