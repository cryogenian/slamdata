{-
Copyright 2015 SlamData, Inc.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
-}

module Model.File.Dialog where

import Prelude (($))
import Data.Maybe (Maybe(..))
import Model.File.Dialog.Download (DownloadDialogRec())
import Model.File.Dialog.Mount (MountDialogRec())
import Model.File.Dialog.Rename (RenameDialogRec())
import Optic.Core 

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
