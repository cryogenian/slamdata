module Model.File.Dialog.Rename where

import Model.Resource
import Data.Path.Pathy

type RenameDialogRec =
  { showList :: Boolean
  , resource :: Resource
  , dirs :: [Resource]
  , selected :: String
  , target :: String
  , error :: String
  , incorrect :: Boolean
  , selectedContent :: [Resource]
  , dirView :: String
  }

initialRenameDialog :: Resource -> RenameDialogRec
initialRenameDialog resource =
  { showList: false
  , resource: resource
  , selected: printPath $ resourceDir resource
  , target: resourceName resource
  , dirs: []
  , incorrect: true
  , error: ""
  , selectedContent: []
  , dirView: printPath $ resourceDir resource
  }
