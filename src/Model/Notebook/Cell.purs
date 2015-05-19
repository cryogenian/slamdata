module Model.Notebook.Cell where

import Data.Argonaut.Combinators ((~>), (:=))
import Data.Argonaut.Core (jsonEmptyObject)
import Data.Argonaut.Encode (EncodeJson, encodeJson)
import Data.Date (Date())
import Data.Either
import Data.Maybe (Maybe(..))
import Data.Time (Milliseconds())
import Global (readInt, isNaN)
import Model.Notebook.Cell.FileInput
import Model.Notebook.Cell.JTableContent
import Model.Notebook.Port
import Optic.Core (lens, LensP(), prism', PrismP(), (..))
import Optic.Extended (TraversalP())

import qualified Model.Notebook.Cell.Common as Cm
import qualified Model.Notebook.Cell.Explore as Ex
import qualified Model.Notebook.Cell.Query as Qu
import qualified Model.Notebook.Cell.Search as Sr

type CellId = Number

string2cellId :: String -> Either String CellId
string2cellId str =
  let int = readInt 10 str
  in if isNaN int then Left "incorrect cell id" else Right int

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
       , addingNextCell :: Boolean 
       }

data CellContent
  = Evaluate String
  | Search Sr.SearchRec
  | Explore Ex.ExploreRec
  | Query Qu.QueryRec
  | Visualize String
  | Markdown String

cellContentType :: CellContent -> String
cellContentType (Evaluate _) = "evaluate"
cellContentType (Explore _) = "explore"
cellContentType (Search _) = "search"
cellContentType (Query _) = "query"
cellContentType (Visualize _) = "visualize"
cellContentType (Markdown _) = "markdown"

newEvaluateContent :: CellContent
newEvaluateContent = Evaluate ""

newSearchContent :: CellContent
newSearchContent = Search Sr.initialSearchRec

newExploreContent :: CellContent
newExploreContent = Explore Ex.initialExploreRec

newQueryContent :: CellContent
newQueryContent = Query Qu.initialQueryRec

newVisualizeContent :: CellContent
newVisualizeContent = Visualize ""

newMarkdownContent :: CellContent
newMarkdownContent = Markdown "" 

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

_Query :: PrismP CellContent Qu.QueryRec
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
  Query _ -> (_Query .. Cm._table) f s
  _  -> _const f s

_AceContent :: TraversalP CellContent String
_AceContent f s = case s of
  Evaluate _ -> _Evaluate f s
  Search _ -> (_Search .. Sr._buffer) f s
  Query _ -> (_Query .. Qu._input) f s
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
       , addingNextCell: false 
       }

instance eqCell :: Eq Cell where
  (==) (Cell c) (Cell c') = c.cellId == c'.cellId
  (/=) a b = not $ a == b

instance ordCell :: Ord Cell where
  compare (Cell c) (Cell c') = compare c.cellId c'.cellId

instance encodeJsonCell :: EncodeJson Cell where
  encodeJson (Cell cell)
    =  "input" := cell.input
    ~> "output" := cell.output
    ~> "content" := cell.content
    ~> "metadata" := cell.metadata
    ~> jsonEmptyObject

instance encodeJsonCellContent :: EncodeJson CellContent where
  encodeJson cc
    =  "type" := cellContentType cc
    ~> case cc of
      Explore rec -> encodeJson rec
      _ -> jsonEmptyObject



_cell :: LensP Cell _
_cell = lens (\(Cell obj) -> obj) (const Cell)

_addingNextCell :: LensP Cell Boolean
_addingNextCell = _cell <<< lens _.addingNextCell _{addingNextCell = _ }

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
