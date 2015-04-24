-- | Input, output messages and state for file component
module Model.File where

import Data.Either (Either())
import Data.Maybe (Maybe(..))
import Model.Breadcrumb (Breadcrumb())
import Model.File.Dialog (Dialog())
import Model.File.Item (Item())
import Model.Search (Search(), initialSearch)
import Model.Sort (Sort(..))

-- | Application state
type State =
  { search :: Search
  , sort :: Sort
  , items :: [Item]
  , breadcrumbs :: [Breadcrumb]
  , path :: String
  , searching :: Boolean
  , dialog :: Maybe Dialog
  }

initialState :: State
initialState =
  { search : initialSearch
  , sort : Asc
  , items : []
  , breadcrumbs : []
  , path: "/"
  , searching: false
  , dialog: Nothing
  }
