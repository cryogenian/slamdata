-- | Input, output messages and state for file component
module Model.DialogResume where

import Model.Item

type RenameDialogRec = {
  showList :: Boolean,
  item :: Item,
  dirs :: [String],
  selected :: String,
  target :: String,
  error :: String,
  incorrect :: Boolean,
  selectedContent :: [String]
  }

initialRenameDialog :: Item -> RenameDialogRec
initialRenameDialog item = {
  showList: false,
  item: item,
  selected: item.root,
  target: item.name,
  dirs: [],
  incorrect: true,
  error: "",
  selectedContent: []}

data DialogResume
  = RenameDialog RenameDialogRec
  | ConfigureDialog
  | MountDialog
  | ShareDialog String

instance eqDialogResume :: Eq DialogResume where
  (==) (RenameDialog r) (RenameDialog r') =
    r.showList == r'.showList &&
    r.item.name == r'.item.name &&
    r.item.root == r'.item.root &&
    r.item.phantom == r'.item.phantom &&
    r.dirs == r'.dirs

  (==) ConfigureDialog ConfigureDialog = true
  (==) MountDialog MountDialog = true
  (==) (ShareDialog s) (ShareDialog s') = s == s'
  (==) _ _ = false
  (/=) a b = not $ a == b
