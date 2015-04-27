module Model.File.Dialog.Rename where

import Model.File.Item

type RenameDialogRec =
  { showList :: Boolean
  , item :: Item
  , dirs :: [String]
  , selected :: String
  , target :: String
  , error :: String
  , incorrect :: Boolean
  , selectedContent :: [String]
  }

initialRenameDialog :: Item -> RenameDialogRec
initialRenameDialog item =
  { showList: false
  , item: item
  , selected: item.root
  , target: item.name
  , dirs: []
  , incorrect: true
  , error: ""
  , selectedContent: []
  }

eqRenameDialog :: RenameDialogRec -> RenameDialogRec -> Boolean
eqRenameDialog r r' = r.showList == r'.showList
                   && r.item.name == r'.item.name
                   && r.item.root == r'.item.root
                   && r.item.phantom == r'.item.phantom
                   && r.dirs == r'.dirs
