-- | Input, output messages and state for file component
module Model.File where

import Control.Monad.Aff
import Control.Timer (Timeout())
import Data.Either
import Data.Foreign
import Data.Maybe
import DOM
import Input.File.Item (ItemInput())
import Input.File.Rename (RenameInput())
import Input.File.Search (SearchInput())
import Model.Breadcrumb
import Model.DialogResume
import Model.Item
import Model.Resource
import Model.Search
import Model.Sort

type Input = Either ItemInput (Either Input1 (Either SearchInput RenameInput))

-- | Input messages
data Input1
  = Sorting Sort
  | SetPath String
  | ItemsUpdate [Item] Sort
  | Loading Boolean
  | Focus Boolean
  | SetSearching Boolean
  | SetDialog (Maybe DialogResume)

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
