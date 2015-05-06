module Model.File.Dialog where

import Model.File.Dialog.Mount
import Model.File.Dialog.Rename

data Dialog
  = RenameDialog RenameDialogRec
  | MountDialog MountDialogRec
  | ShareDialog String

instance eqDialog :: Eq Dialog where
  (==) (RenameDialog r) (RenameDialog r') = eqRenameDialog r r'
  (==) (MountDialog m) (MountDialog m') = eqMountDialog m m'
  (==) (ShareDialog s) (ShareDialog s') = s == s'
  (==) _ _ = false
  (/=) a b = not $ a == b
