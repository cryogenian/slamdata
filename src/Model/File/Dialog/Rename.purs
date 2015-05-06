module Model.File.Dialog.Rename where

import Model.Resource
import Data.Path.Pathy

type RenameDialogRec =
  { showList :: Boolean
  , initial :: Resource
  , resource :: Resource
  , dirs :: [Resource]
  , dir :: Resource
  , siblings :: [Resource]
  , error :: String
  , incorrect :: Boolean
  }

initialRenameDialog :: Resource -> RenameDialogRec
initialRenameDialog resource  =
  { showList: false
  , initial: resource
  , resource: resource
  , dir: parent resource
  , siblings: []
  , dirs: []
  , incorrect: true
  , error: ""
  }
