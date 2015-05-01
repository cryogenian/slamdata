module Model.Notebook where

import Data.Either
import Data.Maybe
import Data.Inject1 (prj, inj)
import Halogen.HTML.Events.Monad (Event())
import Optic.Core (lens, LensP())
import Control.Timer (Timeout())
import Model.Path (Path(), emptyPath)
import Model.File.Item (Item())
import EffectTypes (NotebookAppEff())
import Model.Notebook.Menu (initialDropdowns, DropdownItem())
import qualified App.Notebook.Ace as NA

import Data.Argonaut.Combinators
import qualified Data.Argonaut.Core as Ac
import qualified Data.Argonaut.Decode as Ad
import qualified Data.Argonaut.Encode as Ae
import qualified Data.Argonaut.Printer as Ap
import qualified Network.HTTP.Affjax.Request as Ar
import qualified Data.StrMap as M

type I e = Event (NotebookAppEff e) Input

type State =
  { dropdowns :: [DropdownItem]
  , timeout :: Maybe Timeout
  , path :: Path
  , name :: String
  , items :: [Item]
  , loaded :: Boolean
  , error :: String
  , editable :: Boolean
  , modalError :: String
  , addingCell :: Boolean
  , notebook :: Notebook
  , nextCellId :: Number
  }

dropdowns :: LensP State [DropdownItem]
dropdowns = lens _.dropdowns _{dropdowns = _}

notebook :: LensP State Notebook
notebook = lens _.notebook _{notebook = _}

nextCellId :: LensP State Number
nextCellId = lens _.nextCellId _{nextCellId = _}

initialState :: State
initialState =
  { dropdowns: initialDropdowns
  , timeout: Nothing
  , path: emptyPath
  , name: ""
  , items: []
  , loaded: false
  , error: ""
  , editable: true
  , modalError: ""
  , addingCell: false
  , notebook: newNotebook
  -- TODO: We use CellId = String below - how do we gen one?
  , nextCellId: 0
  }

data Input
  = Dropdown Number
  | CloseDropdowns
  | SetTimeout (Maybe Timeout)
  | SetName String
  | SetPath Path
  | SetItems [Item]
  | SetLoaded Boolean
  | SetError String
  | SetEditable Boolean
  | SetModalError String
  | SetAddingCell Boolean
  | AddCell CellType
  | ToggleEditorCell CellId
  | TrashCell CellId
  | AceInput NA.AceEvent


type CellId = String


string2cellId :: String -> Either String CellId
string2cellId "" = Left "incorrect cellid"
string2cellId a = Right a

data CellType
  = Evaluate
  | Explore
  | Search
  | Query
  | Visualize
  | Markdown

newtype Cell = Cell {
  cellId :: CellId,
  input :: String,
  output :: String,
  cellType :: CellType,
  metadata :: String,
  hiddenEditor :: Boolean
  }

newCell :: CellId -> CellType -> Cell
newCell cellId cellType =
  Cell { cellId: cellId
       , input: ""
       , output: ""
       , cellType: cellType
       , metadata: ""
       , hiddenEditor: false
       }

newtype Notebook = Notebook {
  metadata :: String,
  cells :: [Cell]
  }

newNotebook :: Notebook
newNotebook = Notebook {
  metadata: "",
  cells: []
  }

notebookLens :: LensP Notebook _
notebookLens = lens (\(Notebook obj) -> obj) (const Notebook)

notebookCells :: LensP Notebook [Cell]
notebookCells = notebookLens <<< lens _.cells (_ { cells = _ })

instance cellTypeEncode :: Ae.EncodeJson CellType where
  encodeJson cell = Ac.fromString $ case cell of
    Evaluate -> "evaluate"
    Explore -> "explore"
    Search -> "search"
    Query -> "query"
    Visualize -> "visualize"
    Markdown -> "markdown"

instance cellEncode :: Ae.EncodeJson Cell where
  encodeJson (Cell cell)
    =  "input" := cell.input
    ~> "output" := cell.output
    ~> "cellType" := cell.cellType
    ~> "metadata" := cell.metadata
    ~> Ac.jsonEmptyObject

instance notebookEncode :: Ae.EncodeJson Notebook where
  encodeJson (Notebook {metadata: metadata, cells: cells})
    =  "metadata" := metadata
    ~> "cells" := cells
    ~> Ac.jsonEmptyObject


instance notebookRequestable :: Ar.Requestable Notebook where
  toRequest notebook =
    let str = Ap.printJson (Ae.encodeJson notebook) :: String
    in Ar.toRequest str
