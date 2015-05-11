module Model.Notebook where

import Data.Maybe
import Data.Inject1 (prj, inj)
import Control.Timer (Timeout())
import Optic.Core (lens, LensP())
import EffectTypes (NotebookAppEff())
import Model.Notebook.Menu (initialDropdowns, DropdownItem())
import Model.Notebook.Cell (CellId())
import Model.Notebook.Domain (emptyNotebook, Notebook(), activeCellIdL)
import Model.Resource (Resource(), newNotebook)

type State =
  { dropdowns :: [DropdownItem]
  , timeout :: Maybe Timeout
  , resource :: Resource
  , name :: String
  , siblings :: [Resource]
  , loaded :: Boolean
  , error :: String
  , editable :: Boolean
  , modalError :: String
  , addingCell :: Boolean
  , notebook :: Notebook
  }

dropdowns :: LensP State [DropdownItem]
dropdowns = lens _.dropdowns _{dropdowns = _}

notebook :: LensP State Notebook
notebook = lens _.notebook _{notebook = _}

activeCellId :: LensP State CellId
activeCellId = notebook <<< activeCellIdL

initialState :: State
initialState =
  { dropdowns: initialDropdowns
  , timeout: Nothing
  , resource: newNotebook
  , name: ""
  , siblings: []
  , loaded: false
  , error: ""
  , editable: true
  , modalError: ""
  , addingCell: false
  , notebook: emptyNotebook
  }

