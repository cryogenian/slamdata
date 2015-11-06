module Dashboard.Component.State where

import Prelude

import Data.Lens (LensP(), lens)

type StateRec =
  { editable :: Boolean
  }

newtype State = State StateRec

initialState :: State
initialState =
  State { editable: true
        }


_State :: LensP State StateRec
_State = lens (\(State obj) -> obj) (const State)

_editable :: LensP State Boolean
_editable = _State <<< lens _.editable _{editable = _}
