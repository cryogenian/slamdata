module Model.File.Dialog where

import Model.File.Dialog.Mount
import Model.File.Dialog.Rename

data Dialog
  = RenameDialog RenameDialogRec
  | MountDialog MountDialogRec
  | ShareDialog String
  | ErrorDialog String
