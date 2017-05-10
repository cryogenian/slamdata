module SlamData.Workspace.Card.Geo.Component.Query where

import Halogen.Leaflet as HL

data Query a
  = HandleMessage HL.Message a
