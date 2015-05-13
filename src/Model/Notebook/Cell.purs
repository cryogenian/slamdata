module Model.Notebook.Cell where

import Data.Date (Date())
import Data.Time (Milliseconds())
import Data.Argonaut.Combinators
import Data.Either
import Global
import Model.Notebook.Port
import Optic.Core (lens, LensP())
import qualified Data.Argonaut.Core as Ac
import qualified Data.Argonaut.Encode as Ae

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
newtype Cell =
  Cell { cellId :: CellId
       , input :: Port
       , output :: Port
       , content :: String
       , cellType :: CellType
       , metadata :: String
       , hiddenEditor :: Boolean
       , runState :: RunState
       }

data RunState = RunInitial
              | RunningSince Date
              | RunFinished Milliseconds

newCell :: CellId -> CellType -> Cell
newCell cellId cellType =
  Cell { cellId: cellId
       , input: Closed
       , output: Closed
       , content: ""
       , cellType: cellType
       , metadata: ""
       , hiddenEditor: false
       , runState: RunInitial
       }

instance cellEq :: Eq Cell where
  (==) (Cell c) (Cell c') =
    c.cellId == c'.cellId
  (/=) a b = not $ a == b

instance cellOrd :: Ord Cell where
  compare (Cell c) (Cell c') = compare c.cellId c'.cellId

instance cellTypeEncode :: Ae.EncodeJson CellType where
  encodeJson = Ac.fromString <<< celltype2str

instance cellEncode :: Ae.EncodeJson Cell where
  encodeJson (Cell cell)
    =  "input" := cell.input
    ~> "output" := cell.output
    ~> "cellType" := cell.cellType
    ~> "metadata" := cell.metadata
    ~> Ac.jsonEmptyObject

_cell :: LensP Cell _
_cell = lens (\(Cell obj) -> obj) (const Cell)

_cellId :: LensP Cell CellId
_cellId = _cell <<< lens _.cellId (_ { cellId = _ })

_input :: LensP Cell Port
_input = _cell <<< lens _.input (_ { input = _ })

_output :: LensP Cell Port
_output = _cell <<< lens _.output (_ { output = _ })

_content :: LensP Cell String
_content = _cell <<< lens _.content (_ { content = _ })

_cellType :: LensP Cell CellType
_cellType = _cell <<< lens _.cellType (_ { cellType = _ })

_metadata :: LensP Cell String
_metadata = _cell <<< lens _.metadata (_ { metadata = _ })

_hiddenEditor :: LensP Cell Boolean
_hiddenEditor = _cell <<< lens _.hiddenEditor (_ { hiddenEditor = _ })

_runState :: LensP Cell RunState
_runState = _cell <<< lens _.runState (_ { runState = _ })
