module Model.Notebook where

import Data.Date (Date())
import Data.Maybe
import Data.Inject1 (prj, inj)
import Control.Timer (Timeout())
import Optic.Core (lens, LensP(), (..))
import EffectTypes (NotebookAppEff())
import Model.Notebook.Menu (initialDropdowns, DropdownItem())
import Model.Notebook.Cell (CellId())
import Model.Resource (Resource(), newNotebook)
import qualified Model.Notebook.Domain as D 

type State =
  { dropdowns :: [DropdownItem]
  , timeout :: Maybe Timeout
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

_notebook :: LensP State D.Notebook
_notebook = lens _.notebook _{notebook = _}

_activeCellId :: LensP State CellId
_activeCellId = _notebook .. D._activeCellId

initialState :: State
initialState =
  { dropdowns: initialDropdowns
  , timeout: Nothing
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
