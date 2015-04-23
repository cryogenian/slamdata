-- | Input, output messages and state for file component
module Model.File where

import Control.Monad.Aff
import Control.Timer (Timeout())
import Data.Maybe
import Data.Foreign

import Data.Either
import DOM


import Model.Sort
import Model.Resource
import Model.Item
import Model.Search
import Model.Breadcrumb
import Input.File.Search (SearchInput())

type Input = Either Input1 SearchInput

-- | Input messages
data Input1
  = Sorting Sort
  | ItemsUpdate [Item] Sort
  | ItemHover Number Boolean
  | ItemSelect Number Boolean
  | ItemAdd Item
  | SetPath String
  | Resort
  | Remove Item
  | Loading Boolean
  | Focus Boolean
  | SetSearching Boolean
  | SetDialog (Maybe DialogResume)
  | AddRenameDirs [String]
  | SetRenameSelected String
  | RenameChanged String
  | RenameError String
  | RenameIncorrect Boolean
  | RenameSelectedContent [String]

-- | Request Messages
data Request
  = GoToRoute String
  | SetSort Sort
  | SearchChange Search String String
  | SearchClear Boolean Search
  | Breadcrumb Breadcrumb
  | SearchSubmit Search String
  | Open Item
  | Delete Item
  | Share Item
  | Move Item
  | Configure Item
  | CreateNotebook State
  | MountDatabase State
  | CreateFolder State
  | UploadFile Node State
  | FileListChanged Node State

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


-- | Application state
type State = {
  search :: Search,
  sort :: Sort,
  items :: [Item],
  breadcrumbs :: [Breadcrumb],
  path :: String,
  searching :: Boolean,
  dialog :: Maybe DialogResume
  }

initialState :: State
initialState = {
  search : initialSearch,
  sort : Asc,
  items : [],
  breadcrumbs : [],
  path: "/",
  searching: false,
  dialog: Nothing
  }


