module Input.File.Rename
  ( RenameInput(..)
  , inputRename
  ) where

import Data.Array (sort, nub, sortBy)
import Data.String (length)
import Model.File.Dialog (Dialog(RenameDialog))
import Model.Resource
import Optic.Core

data RenameInput
  = RenameError String
  | RenameIncorrect Boolean
  | SetResource Resource
  | SetDir Resource
  | SetSiblings [Resource]
  | AddDirs [Resource]
    
inputRename :: Dialog -> RenameInput -> Dialog
inputRename (RenameDialog d) input = RenameDialog $ case input of
  RenameIncorrect incorrect ->
    d { incorrect = incorrect }

  RenameError err ->
    _{error = err} $ if length err /= 0
                     then d {incorrect = true}
                     else d 

  SetResource newRes ->
    d { resource = newRes}

  SetDir toSelect ->
    d { dir = toSelect
      , showList = false
      , error = ""
      }

  AddDirs dirs ->
    d { dirs = sort $ nub $ (d.dirs <> dirs) }


  SetSiblings ss ->
    d { siblings = ss }

inputRename dialog _ = dialog
