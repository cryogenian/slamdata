-- | Input, output messages and state for file component
module Model.File where

import Control.Monad.Aff
import Control.Timer (Timeout())
import Data.Either
import Data.Foreign
import Data.Maybe
import DOM
import Input.File.Search (SearchInput())
import Input.File.Rename (RenameInput())
import Model.Breadcrumb
import Model.DialogResume
import Model.Item
import Model.Resource
import Model.Search
import Model.Sort

type Input = Either Input1 (Either SearchInput RenameInput)

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


