module SlamData.Workspace.Card.Setups.Geo.Heatmap.Component.Query where

import SlamData.Workspace.Card.Setups.DimensionMap.Component.Query as Q

data Query a = HandleDims Q.Message a
