module Input.File.Rename
  ( RenameInput(..)
  , inputRename
  ) where

import Data.Array (sort, nub, sortBy)
import Data.String (length)
import Model.File.Dialog (Dialog(RenameDialog))
import Model.Resource

data RenameInput
  = RenameChanged String
  | RenameError String
  | RenameIncorrect Boolean
  | RenameSelectedContent [Resource]
  | SetRenameSelected String
  | AddRenameDirs [Resource]

inputRename :: Dialog -> RenameInput -> Dialog
inputRename (RenameDialog d) input = RenameDialog $ case input of

  RenameChanged newVal ->
    d { target = newVal }

  RenameError err ->
    _{error = err} $ if length err /= 0
                     then d {incorrect = true}
                     else d 

  RenameSelectedContent cont ->
    d { selectedContent = cont }

  SetRenameSelected toSelect ->
    d { selected = toSelect
      , showList = false
      , error = ""
      }
  AddRenameDirs dirs ->
    d { dirs = sort $ nub $ (d.dirs <> dirs) }
  RenameIncorrect incorrect ->
    d { incorrect = incorrect }

inputRename dialog _ = dialog
