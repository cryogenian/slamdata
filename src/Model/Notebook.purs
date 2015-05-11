module Model.Notebook where

import Data.Either
import Data.Maybe
import Data.Foreign (toForeign)
import Data.Foreign.Class (read)
import Data.Inject1 (prj, inj)
import Data.Bifunctor (lmap)
import Halogen.HTML.Events.Monad (Event())
import Optic.Core (lens, LensP())
import Control.Timer (Timeout())
import EffectTypes (NotebookAppEff())
import Model.Notebook.Menu (initialDropdowns, DropdownItem())

import Data.Argonaut.Combinators
import qualified Data.Argonaut.Core as Ac
import qualified Data.Argonaut.Decode as Ad
import qualified Data.Argonaut.Encode as Ae
import qualified Data.Argonaut.Printer as Ap
import qualified Network.HTTP.Affjax.Request as Ar
import qualified Data.StrMap as M
import Model.Resource (Resource(), newNotebook)
import Global


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
  , nextCellId :: CellId
  , activeCellId :: CellId 
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
  , resource: newNotebook
  , name: ""
  , siblings: []
  , loaded: false
  , error: ""
  , editable: true
  , modalError: ""
  , addingCell: false
  , notebook: emptyNotebook
  , nextCellId: 0
  , activeCellId: 0
  }

type CellId = Number

string2cellId :: String -> Either String CellId
string2cellId str =
  if isNaN int then Left "incorrect cell id" else Right int
  where int = readFloat str 


data CellType
  = Evaluate
  | Explore
  | Search
  | Query
  | Visualize
  | Markdown

celltype2str :: CellType -> String
celltype2str Evaluate = "evaluate"
celltype2str Explore = "explore"
celltype2str Search = "search"
celltype2str Query = "query"
celltype2str Visualize = "visualize"
celltype2str Markdown = "markdown"

-- input and output are ports for cells i.e data uri,
-- content is cell content i.e. markdown
newtype Cell = Cell {
  cellId :: CellId,
  input :: String,
  output :: String,
  content :: String,
  cellType :: CellType,
  metadata :: String,
  hiddenEditor :: Boolean,
  isRunning :: Boolean
  }

newCell :: CellId -> CellType -> Cell
newCell cellId cellType =
  Cell { cellId: cellId
       , input: ""
       , output: ""
       , content: ""
       , cellType: cellType
       , metadata: ""
       , hiddenEditor: false
       , isRunning: false
       }

newtype Notebook = Notebook {
  metadata :: String,
  cells :: [Cell]
  }

emptyNotebook :: Notebook
emptyNotebook = Notebook
  { metadata: ""
  , cells: [] }

notebookLens :: LensP Notebook _
notebookLens = lens (\(Notebook obj) -> obj) (const Notebook)

notebookCells :: LensP Notebook [Cell]
notebookCells = notebookLens <<< lens _.cells (_ { cells = _ })

instance cellTypeEncode :: Ae.EncodeJson CellType where
  encodeJson = Ac.fromString <<< celltype2str 

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
