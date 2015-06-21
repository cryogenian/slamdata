module Model.File.Dialog where

import Data.Maybe (Maybe(..))
import Model.File.Dialog.Download (DownloadDialogRec())
import Model.File.Dialog.Mount (MountDialogRec())
import Model.File.Dialog.Rename (RenameDialogRec())
import Optic.Core (prism', PrismP())

data Dialog
  = RenameDialog RenameDialogRec
  | MountDialog MountDialogRec
  | ShareDialog String
  | ErrorDialog String
  | DownloadDialog DownloadDialogRec

_RenameDialog :: PrismP Dialog RenameDialogRec
_RenameDialog = prism' RenameDialog $ \s -> case s of
  RenameDialog r -> Just r
  _ -> Nothing

_MountDialog :: PrismP Dialog MountDialogRec
_MountDialog = prism' MountDialog $ \s -> case s of
  MountDialog r -> Just r
  _ -> Nothing

_DownloadDialog :: PrismP Dialog DownloadDialogRec
_DownloadDialog = prism' DownloadDialog $ \s -> case s of
  DownloadDialog r -> Just r
  _ -> Nothing
