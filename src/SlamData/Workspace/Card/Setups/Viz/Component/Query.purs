module SlamData.Workspace.Card.Setups.Viz.Component.Query where

import SlamData.Workspace.Card.Setups.Viz.VizTypePicker as VT
import SlamData.Workspace.Card.Setups.DimensionMap.Component.Query as Q

data Query a
  = HandlePicker VT.Message a
  | HandleDims Q.Message a
  | ToggleVizPicker a
