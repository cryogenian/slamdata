module SlamData.Workspace.Card.Save.Component.State where

import Data.Lens (LensP, lens)

type State =
  {
    pathString ∷ String
  }

initialState ∷ State
initialState =
  {
    pathString: ""
  }

_pathString ∷ ∀ a r. LensP {pathString ∷ a|r} a
_pathString = lens _.pathString (_{pathString = _})
