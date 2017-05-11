module SlamData.Workspace.Card.Setups.Geo.Marker.Component.Query where

import SlamData.Workspace.Card.Setups.DimensionMap.Component.Query as Q

data Query a
  = HandleDims Q.Message a
  | SetMaxSymbolSize String a
  | SetMinSymbolSize String a
