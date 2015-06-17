-- | Input, output messages and state for file component
module Model.File
  ( State()
  , StateRec()
  , initialState
  , _search
  , _sort
  , _items
  , _breadcrumbs
  , _path
  , _dialog
  , _salt
  , _isMount
  , _showHiddenFiles
  , isSearching
  ) where

import Data.Either (Either())
import Data.Maybe (Maybe(..), isJust)
import Data.Path.Pathy (rootDir)
import Data.These (theseRight)
import Model.File.Breadcrumb (Breadcrumb())
import Model.File.Dialog (Dialog())
import Model.File.Item (Item())
import Model.File.Salt (Salt(..))
import Model.File.Search (Search(), initialSearch, _value)
import Model.File.Sort (Sort(..))
import Model.Path (DirPath())
import Optic.Core (LensP(), lens, (..), (^.))

-- | Application state
newtype State = State StateRec

type StateRec =
  { search :: Search
  , sort :: Sort
  , items :: [Item]
  , breadcrumbs :: [Breadcrumb]
  , path :: DirPath
  , dialog :: Maybe Dialog
  , salt :: Salt
  , isMount :: Boolean
  , showHiddenFiles :: Boolean
  }

initialState :: State
initialState = State
  { search: initialSearch ""
  , sort: Asc
  , items: []
  , breadcrumbs: []
  , path: rootDir
  , dialog: Nothing
  , salt: Salt ""
  , isMount: false
  , showHiddenFiles: false
  }

_State :: LensP State StateRec
_State  = lens (\(State obj) -> obj) (const State)

_search :: LensP State Search
_search = _State .. lens _.search _{search = _}

_sort :: LensP State Sort
_sort = _State .. lens _.sort _{sort = _}

_items :: LensP State [Item]
_items = _State .. lens _.items _{items = _}

_breadcrumbs :: LensP State [Breadcrumb]
_breadcrumbs = _State .. lens _.breadcrumbs _{breadcrumbs = _}

_path :: LensP State DirPath
_path = _State .. lens _.path _{path = _}

_dialog :: LensP State (Maybe Dialog)
_dialog = _State .. lens _.dialog _{dialog = _}

_salt :: LensP State Salt
_salt = _State .. lens _.salt _{salt = _}

_isMount :: LensP State Boolean
_isMount = _State .. lens _.isMount _{isMount = _}

_showHiddenFiles :: LensP State Boolean
_showHiddenFiles = _State .. lens _.showHiddenFiles _{showHiddenFiles = _}

isSearching :: State -> Boolean
isSearching state = isJust $ theseRight $ state ^. _search .. _value
