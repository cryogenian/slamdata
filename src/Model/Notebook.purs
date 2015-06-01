module Model.Notebook where

import Data.Date (Date())
import Data.Maybe (Maybe(..))
import Data.Path.Pathy
import Data.Platform (Platform(..))
import Model.Notebook.Cell (CellId())
import Model.Notebook.Dialog
import Model.Notebook.Domain (Notebook(), emptyNotebook)
import Model.Notebook.Menu (DropdownItem(), initialDropdowns)
import Model.Path
import Optic.Core (LensP(), lens, (^.))

type State =
  { dropdowns :: [DropdownItem]
  , loaded :: Boolean
  , error :: Maybe String
  , editable :: Boolean
  , notebook :: Notebook
  , viewingCell :: Maybe CellId
  , tickDate :: Maybe Date
  , platform :: Platform
  , dialog :: Maybe Dialog
  }

_dropdowns :: LensP State [DropdownItem]
_dropdowns = lens _.dropdowns _{dropdowns = _}

_loaded :: LensP State Boolean
_loaded = lens _.loaded _{loaded = _}

_error :: LensP State (Maybe String)
_error = lens _.error _{error = _}

_editable :: LensP State Boolean
_editable = lens _.editable _{editable = _}

_viewingCell :: LensP State (Maybe CellId)
_viewingCell = lens _.viewingCell _{viewingCell = _}

_notebook :: LensP State Notebook
_notebook = lens _.notebook _{notebook = _}

_tickDate :: LensP State (Maybe Date)
_tickDate = lens _.tickDate _{tickDate = _}

_platform :: LensP State Platform
_platform = lens _.platform _{platform = _}

_dialog :: LensP State (Maybe Dialog)
_dialog = lens _.dialog _{dialog = _}

initialState :: State
initialState =
  { dropdowns: initialDropdowns
  , loaded: false
  , error: Nothing
  , editable: true
  , notebook: emptyNotebook
  , viewingCell: Nothing
  , tickDate: Nothing
  , platform: Other
  , dialog: Nothing
  }
