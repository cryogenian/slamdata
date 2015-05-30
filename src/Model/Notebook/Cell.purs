module Model.Notebook.Cell where

import Data.Argonaut.Combinators ((~>), (:=), (.?))
import Data.Argonaut.Core (jsonEmptyObject)
import Data.Argonaut.Decode (DecodeJson, decodeJson)
import Data.Argonaut.Encode (EncodeJson, encodeJson)
import Data.Date (Date())
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Time (Milliseconds())
import Global (readInt, isNaN)
import Model.Notebook.Cell.FileInput
import Model.Notebook.Cell.JTableContent
import Model.Notebook.Port
import Model.Resource
import Optic.Core (lens, LensP(), prism', PrismP(), (..), (.~))
import Optic.Extended (TraversalP())

import qualified Model.Notebook.Cell.Common as Cm
import qualified Model.Notebook.Cell.Explore as Ex
import qualified Model.Notebook.Cell.Query as Qu
import qualified Model.Notebook.Cell.Search as Sr
import qualified Model.Notebook.Cell.Viz as Vz

type CellId = Number
type FailureMessage = String

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
       , message :: String
       , failures :: [FailureMessage]
       , hiddenEditor :: Boolean
       , runState :: RunState
       }

newCell :: CellId -> CellContent -> Cell
newCell cellId content =
  Cell { cellId: cellId
       , input: Closed
       , output: Closed
       , content: content
       , expandedStatus: false
       , failures: []
       , message: ""
       , hiddenEditor: false
       , runState: RunInitial
       }

_Cell :: LensP Cell _
_Cell = lens (\(Cell obj) -> obj) (const Cell)

_cellId :: LensP Cell CellId
_cellId = _Cell <<< lens _.cellId (_ { cellId = _ })

_input :: LensP Cell Port
_input = _Cell <<< lens _.input (_ { input = _ })

_output :: LensP Cell Port
_output = _Cell <<< lens _.output (_ { output = _ })

_content :: LensP Cell CellContent
_content = _Cell <<< lens _.content (_ { content = _ })

_hiddenEditor :: LensP Cell Boolean
_hiddenEditor = _Cell <<< lens _.hiddenEditor (_ { hiddenEditor = _ })

_runState :: LensP Cell RunState
_runState = _Cell <<< lens _.runState (_ { runState = _ })

_expandedStatus :: LensP Cell Boolean
_expandedStatus = _Cell <<< lens _.expandedStatus (_ { expandedStatus = _ })

_failures :: LensP Cell [FailureMessage]
_failures = _Cell <<< lens _.failures (_ { failures = _ })

_message :: LensP Cell String
_message = _Cell <<< lens _.message _{message = _}

instance eqCell :: Eq Cell where
  (==) (Cell c) (Cell c') = c.cellId == c'.cellId
  (/=) a b = not $ a == b

instance ordCell :: Ord Cell where
  compare (Cell c) (Cell c') = compare c.cellId c'.cellId

instance encodeJsonCell :: EncodeJson Cell where
  encodeJson (Cell cell)
    =  "cellId" := cell.cellId
    ~> "input" := cell.input
    ~> "output" := cell.output
    ~> "content" := cell.content
    ~> jsonEmptyObject

instance decodeJsonCell :: DecodeJson Cell where
  decodeJson json = do
    obj <- decodeJson json
    cell <- newCell <$> obj .? "cellId"
                    <*> obj .? "content"
    input <- obj .? "input"
    output <- obj .? "output"
    pure (cell # (_input .~ input)
              .. (_output .~ output))

data CellContent
  = Search Sr.SearchRec
  | Explore Ex.ExploreRec
  | Visualize Vz.VizRec
  | Query Qu.QueryRec
  | Markdown String

cellContentType :: CellContent -> String
cellContentType (Explore _) = "explore"
cellContentType (Search _) = "search"
cellContentType (Query _) = "query"
cellContentType (Visualize _) = "visualize"
cellContentType (Markdown _) = "markdown"

newSearchContent :: Resource -> CellContent
newSearchContent res = Search (Sr.initialSearchRec # Sr._input .. _file .~ Right res)

newExploreContent :: CellContent
newExploreContent = Explore Ex.initialExploreRec

newQueryContent :: Resource -> CellContent
newQueryContent res = Query (Qu.initialQueryRec # Qu._input .~ "SELECT * FROM \"" ++ resourcePath res ++ "\"")

newVisualizeContent :: CellContent
newVisualizeContent = Visualize Vz.initialVizRec

newMarkdownContent :: CellContent
newMarkdownContent = Markdown ""

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

_Visualize :: PrismP CellContent Vz.VizRec
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
  Search _ -> (_Search .. Sr._input) f s
  _  -> _const f s

_JTableContent :: TraversalP CellContent JTableContent
_JTableContent f s = case s of
  Explore _ -> (_Explore .. Ex._table) f s
  Search _ -> (_Search .. Sr._table) f s
  Query _ -> (_Query .. Qu._table) f s
  _  -> _const f s

_AceContent :: TraversalP CellContent String
_AceContent f s = case s of
  Search _ -> (_Search .. Sr._buffer) f s
  Query _ -> (_Query .. Qu._input) f s
  Markdown _ -> _Markdown f s
  _ -> _const f s

_const :: forall a b. TraversalP a b
_const _ s = pure s

instance encodeJsonCellContent :: EncodeJson CellContent where
  encodeJson cc
    =  "type" := cellContentType cc
    ~> case cc of
      Search rec -> encodeJson rec
      Explore rec -> encodeJson rec
      Query rec -> encodeJson rec
      Markdown s -> "value" := s ~> jsonEmptyObject
      -- TODO: Visualize
      _ -> jsonEmptyObject

instance decodeJsonExploreRec :: DecodeJson CellContent where
  decodeJson json = do
    obj <- decodeJson json
    cellType <- obj .? "type"
    case cellType of
      "search" -> Search <$> decodeJson json
      "explore" -> Explore <$> decodeJson json
      "query" -> Query <$> decodeJson json
      "markdown" -> Markdown <$> obj .? "value"
      -- TODO: Visualize
      _ -> Left $ "Unknown CellContent type: '" ++ cellType ++ "'"

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
