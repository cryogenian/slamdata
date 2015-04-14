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

-- | Input messages 
data Input
  = Sorting Sort
  | ItemsUpdate [Item] Sort 
  | ItemHover Number Boolean
  | ItemSelect Number Boolean
  | ItemAdd Item
  | SearchValidation Boolean
  | SearchSet String
  | SearchTimeout Timeout
  | SearchNextValue String
  | SetPath String
  | Resort
  | Remove Item
  | Loading Boolean
  | Focus Boolean
  | SetSearching Boolean
    
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
  searching :: Boolean
  }

initialState :: State
initialState = {
  search : initialSearch,
  sort : Asc,
  items : [],
  breadcrumbs : [],
  path: "/",
  searching: false
  }
