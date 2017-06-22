module SlamData.Workspace.Card.Setups.Viz.Component.Query where

import SlamData.Workspace.Card.Setups.Viz.VizTypePicker as VT
import SlamData.Workspace.Card.Setups.DimMap.Component.Query as Q
import SlamData.Workspace.Card.Setups.Viz.Auxiliary as Aux

data Query a
  = HandlePicker VT.Message a
  | HandleDims Q.Message a
  | HandleAux Aux.State a
  | ToggleVizPicker a
