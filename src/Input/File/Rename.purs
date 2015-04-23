module Input.File.Rename where

import Data.Array (sort, nub)
import Data.String (length)
import Model.DialogResume

data RenameInput
  = RenameChanged String
  | RenameError String
  | RenameIncorrect Boolean
  | RenameSelectedContent [String]
  | SetRenameSelected String
  | AddRenameDirs [String]

inputRename :: DialogResume -> RenameInput -> DialogResume
inputRename (RenameDialog d) input = RenameDialog $ case input of

  RenameChanged newVal ->
    d { target = newVal }

  RenameError err ->
    d { error = err
      , incorrect = length err /= 0
      }

  RenameSelectedContent cont ->
    d { selectedContent = cont }

  SetRenameSelected toSelect ->
    d { selected = toSelect
      , showList = false
      , error = ""
      }
  AddRenameDirs dirs ->
    d { dirs = sort $ nub $ d.dirs <> dirs }

inputRename dialog _ = dialog
