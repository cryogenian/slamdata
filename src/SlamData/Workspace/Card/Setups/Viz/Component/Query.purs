module SlamData.Workspace.Card.Setups.Viz.Component.Query where

import SlamData.Workspace.Card.Setups.VizPicker.Component as VT
import SlamData.Workspace.Card.Setups.DimensionMap.Component.Query as Q
import SlamData.Workspace.Card.Setups.Auxiliary as Aux

data Query a
  = HandlePicker VT.Message a
  | HandleDims Q.Message a
  | HandleAux Aux.State a
  | ToggleVizPicker a
