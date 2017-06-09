module SlamData.Workspace.Card.Setups.Viz.Component.Query where

import SlamData.Workspace.Card.Setups.Viz.VizTypePicker as VT

data Query a
  = HandlePicker VT.Message a
