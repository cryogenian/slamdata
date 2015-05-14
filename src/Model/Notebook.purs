module Model.Notebook where

import Data.Maybe
import Data.Date (Date())
import Data.Inject1 (prj, inj)
import Data.Array ((!!))
import Control.Timer (Timeout())
import Optic.Core (lens, LensP(), (..), (^.))
import EffectTypes (NotebookAppEff())
import Model.Notebook.Menu (initialDropdowns, DropdownItem())
import Model.Notebook.Cell (CellId(), Cell())
import Model.Resource (Resource(), newNotebook)
import qualified Model.Notebook.Domain as D 


type State =
  { dropdowns :: [DropdownItem]
  , resource :: Resource
  , siblings :: [Resource]
  , loaded :: Boolean
  , error :: String
  , editable :: Boolean
  , modalError :: String
  , addingCell :: Boolean
  , notebook :: D.Notebook
  , initialName :: String 
  , tickDate :: Maybe Date
  }

_dropdowns :: LensP State [DropdownItem]
_dropdowns = lens _.dropdowns _{dropdowns = _}

_resource :: LensP State Resource
_resource = lens _.resource _{resource = _}

_notebook :: LensP State D.Notebook
_notebook = lens _.notebook _{notebook = _}

_activeCellId :: LensP State CellId
_activeCellId = _notebook .. D._activeCellId

_siblings :: LensP State [Resource]
_siblings = lens _.siblings _{siblings = _}

_loaded :: LensP State Boolean
_loaded = lens _.loaded _{loaded = _}

_error :: LensP State String
_error = lens _.error _{error = _}

_editable :: LensP State Boolean
_editable = lens _.editable _{editable = _}

_modalError :: LensP State String
_modalError = lens _.modalError _{modalError = _}

_addingCell :: LensP State Boolean
_addingCell = lens _.addingCell _{addingCell = _}

_initialName :: LensP State String
_initialName = lens _.initialName _{initialName = _}

_tickDate :: LensP State (Maybe Date)
_tickDate = lens _.tickDate _{tickDate = _}

activeCell :: State -> Maybe Cell
activeCell state = (state ^. _notebook .. D._notebookCells) !! (state ^. _activeCellId)

initialState :: State
initialState =
  { dropdowns: initialDropdowns
  , resource: newNotebook
  , siblings: []
  , loaded: false
  , error: ""
  , editable: true
  , modalError: ""
  , addingCell: false
  , notebook: D.emptyNotebook
  , initialName: ""
  , tickDate: Nothing
  }
