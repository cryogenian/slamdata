module Model.Notebook where

import Data.Date (Date())
import Data.Maybe (Maybe(..))
import Data.Path.Pathy
import Data.Platform (Platform(..))
import Model.Notebook.Domain (Notebook(), emptyNotebook)
import Model.Notebook.Menu (DropdownItem(), initialDropdowns)
import Model.Path
import Optic.Core (LensP(), lens, (^.))

type State =
  { dropdowns :: [DropdownItem]
  , loaded :: Boolean
  , error :: Maybe String
  , editable :: Boolean
  , modalError :: String
  , addingCell :: Boolean
  , notebook :: Notebook
  , tickDate :: Maybe Date
  , platform :: Platform
  }

_dropdowns :: LensP State [DropdownItem]
_dropdowns = lens _.dropdowns _{dropdowns = _}

_loaded :: LensP State Boolean
_loaded = lens _.loaded _{loaded = _}

_error :: LensP State (Maybe String)
_error = lens _.error _{error = _}

_editable :: LensP State Boolean
_editable = lens _.editable _{editable = _}

_modalError :: LensP State String
_modalError = lens _.modalError _{modalError = _}

_addingCell :: LensP State Boolean
_addingCell = lens _.addingCell _{addingCell = _}

_notebook :: LensP State Notebook
_notebook = lens _.notebook _{notebook = _}

_tickDate :: LensP State (Maybe Date)
_tickDate = lens _.tickDate _{tickDate = _}

_platform :: LensP State Platform
_platform = lens _.platform _{platform = _}

initialState :: State
initialState =
  { dropdowns: initialDropdowns
  , loaded: false
  , error: Nothing
  , editable: true
  , modalError: ""
  , addingCell: false
  , notebook: emptyNotebook
  , tickDate: Nothing
  , platform: Other
  }
