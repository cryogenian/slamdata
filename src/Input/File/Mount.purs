module Input.File.Mount
  ( MountInput(..)
  , inputMount
  ) where

import Data.Array (filter)
import Model.File.Dialog (Dialog(MountDialog))
import Model.File.Dialog.Mount (MountDialogRec(), MountHostRec(), MountPropRec(), initialMountHost, initialMountProp)

data MountInput = ValueChanged (MountDialogRec -> MountDialogRec)

inputMount :: Dialog -> MountInput -> Dialog
inputMount (MountDialog d) (ValueChanged fn) =
  let d' = fn d
  in MountDialog d' { hosts = (filter (not <<< isEmptyHost) d'.hosts) ++ [initialMountHost]
                    , props = (filter (not <<< isEmptyProp) d'.props) ++ [initialMountProp]
                    }
inputMount dialog _ = dialog

isEmptyHost :: MountHostRec -> Boolean
isEmptyHost h = h.host == "" && h.port == ""

isEmptyProp :: MountPropRec -> Boolean
isEmptyProp p = p.name == "" && p.value == ""
