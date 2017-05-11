module SlamData.Workspace.Card.Setups.Geo.Marker.Component.State where

type State =
  { minSize ∷ Number
  , maxSize ∷ Number
  }

initialState ∷ State
initialState =
  { minSize: 10.0
  , maxSize: 50.0
  }
