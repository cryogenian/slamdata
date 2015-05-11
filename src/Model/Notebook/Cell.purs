module Model.Notebook.Cell where

import Data.Either
import Global
import Model.Notebook.Port
import Data.Argonaut.Combinators
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
       , isRunning :: Boolean
       }

newCell :: CellId -> CellType -> Cell
newCell cellId cellType =
  Cell { cellId: cellId
       , input: Closed 
       , output: Closed
       , content: ""
       , cellType: cellType
       , metadata: ""
       , hiddenEditor: false
       , isRunning: false
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
