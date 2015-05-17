module Model.Notebook.Cell where

import Data.Date (Date())
import Data.Time (Milliseconds())
import Data.Argonaut.Combinators
import Data.Either
import Data.Maybe (Maybe(..))
import Global
import Model.Notebook.Port
import Optic.Core (lens, LensP(), prism', PrismP(), (..))
import Model.Notebook.Cell.FileInput
import Model.Notebook.Cell.JTableContent
import Optic.Extended (TraversalP())
import qualified Model.Notebook.Cell.Explore as Ex
import qualified Model.Notebook.Cell.Search as Sr
import qualified Model.Notebook.Cell.Common as Cm
import qualified Data.Argonaut.Core as Ac
import qualified Data.Argonaut.Encode as Ae

type CellId = Number

string2cellId :: String -> Either String CellId
string2cellId str =
  if isNaN int then Left "incorrect cell id" else Right int
  where int = readFloat str

newtype Cell =
  Cell { cellId :: CellId
       , input :: Port
       , output :: Port
       , content :: CellContent
       , expandedStatus :: Boolean
       , failures :: [FailureMessage]
       , metadata :: String
       , hiddenEditor :: Boolean
       , runState :: RunState
       }

data CellContent
  = Evaluate String
  | Search Sr.SearchRec
  | Explore Ex.ExploreRec
  | Query String
  | Visualize String
  | Markdown String

cellContentType :: CellContent -> String
cellContentType (Evaluate _) = "evaluate"
cellContentType (Explore _) = "explore"
cellContentType (Search _) = "search"
cellContentType (Query _) = "query"
cellContentType (Visualize _) = "visualize"
cellContentType (Markdown _) = "markdown"

_Evaluate :: PrismP CellContent String
_Evaluate = prism' Evaluate $ \s -> case s of
  Evaluate s -> Just s
  _ -> Nothing

_Explore :: PrismP CellContent Ex.ExploreRec
_Explore = prism' Explore $ \s -> case s of
  Explore r -> Just r
  _ -> Nothing

_Search :: PrismP CellContent Sr.SearchRec
_Search = prism' Search $ \s -> case s of
  Search s -> Just s
  _ -> Nothing

_Query :: PrismP CellContent String
_Query = prism' Query $ \s -> case s of
  Query s -> Just s
  _ -> Nothing

_Visualize :: PrismP CellContent String
_Visualize = prism' Visualize $ \s -> case s of
  Visualize s -> Just s
  _ -> Nothing

_Markdown :: PrismP CellContent String
_Markdown = prism' Markdown $ \s -> case s of
  Markdown s -> Just s
  _ -> Nothing

_FileInput :: TraversalP CellContent FileInput
_FileInput f s = case s of
  Explore _ -> (_Explore .. Ex._input) f s
  Search _ -> (_Search .. Cm._input) f s
  _  -> _const f s

_JTableContent :: TraversalP CellContent JTableContent
_JTableContent f s = case s of
  Explore _ -> (_Explore .. Ex._table) f s
  Search _ -> (_Search .. Cm._table) f s 
  _  -> _const f s

_AceContent :: TraversalP CellContent String
_AceContent f s = case s of
  Evaluate _ -> _Evaluate f s
  Search _ -> (_Search .. Sr._buffer) f s
  Query _ -> _Query f s
  Visualize _ -> _Visualize f s
  Markdown _ -> _Markdown f s
  _ -> _const f s

_const :: forall a b. TraversalP a b
_const _ s = pure s

type FailureMessage = String

data RunState = RunInitial
              | RunningSince Date
              | RunFinished Milliseconds

_RunInitial :: PrismP RunState Unit
_RunInitial = prism' (const RunInitial) $ \r -> case r of
  RunInitial -> Just unit
  _ -> Nothing

_RunningSince :: PrismP RunState Date
_RunningSince = prism' RunningSince $ \r -> case r of
  RunningSince d -> Just d
  _ -> Nothing

_RunFinished :: PrismP RunState Milliseconds
_RunFinished = prism' RunFinished $ \r -> case r of
  RunFinished m -> Just m
  _ -> Nothing

newCell :: CellId -> CellContent -> Cell
newCell cellId content =
  Cell { cellId: cellId
       , input: Closed
       , output: Closed
       , content: content
       , expandedStatus: false
       , failures: []
       , metadata: ""
       , hiddenEditor: false
       , runState: RunInitial
       }

instance cellEq :: Eq Cell where
  (==) (Cell c) (Cell c') = c.cellId == c'.cellId
  (/=) a b = not $ a == b

instance cellOrd :: Ord Cell where
  compare (Cell c) (Cell c') = compare c.cellId c'.cellId

-- instance cellTypeEncode :: Ae.EncodeJson CellType where
--   encodeJson = Ac.fromString <<< celltype2str

instance cellEncode :: Ae.EncodeJson Cell where
  encodeJson (Cell cell)
    =  "input" := cell.input
    ~> "output" := cell.output
    -- ~> "cellType" := cell.cellType
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

_content :: LensP Cell CellContent
_content = _cell <<< lens _.content (_ { content = _ })

_metadata :: LensP Cell String
_metadata = _cell <<< lens _.metadata (_ { metadata = _ })

_hiddenEditor :: LensP Cell Boolean
_hiddenEditor = _cell <<< lens _.hiddenEditor (_ { hiddenEditor = _ })

_runState :: LensP Cell RunState
_runState = _cell <<< lens _.runState (_ { runState = _ })

_expandedStatus :: LensP Cell Boolean
_expandedStatus = _cell <<< lens _.expandedStatus (_ { expandedStatus = _ })

_failures :: LensP Cell [FailureMessage]
_failures = _cell <<< lens _.failures (_ { failures = _ })
