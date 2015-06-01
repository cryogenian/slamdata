-- | Input, output messages and state for file component
module Model.File where

import Data.Either (Either())
import Data.Maybe (Maybe(..))
import Data.Path.Pathy (rootDir)
import Model.Breadcrumb (Breadcrumb())
import Model.File.Dialog (Dialog())
import Model.File.Item (Item(), wrap)
import Model.File.Search (Search(), initialSearch)
import Model.Path (DirPath())
import Model.Salt (Salt(..))
import Model.Sort (Sort(..))

-- | Application state
type State =
  { search :: Search
  , sort :: Sort
  , items :: [Item]
  , breadcrumbs :: [Breadcrumb]
  , path :: DirPath
  , searching :: Boolean
  , dialog :: Maybe Dialog
  , salt :: Salt
  , hasMountRoot :: Boolean
  }

initialState :: State
initialState =
  { search : initialSearch
  , sort : Asc
  , items : []
  , breadcrumbs : []
  , path: rootDir
  , searching: false
  , dialog: Nothing
  , salt: Salt ""
  , hasMountRoot: false
  }
