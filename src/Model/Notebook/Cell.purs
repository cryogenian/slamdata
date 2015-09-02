{-
Copyright 2015 SlamData, Inc.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
-}

module Model.Notebook.Cell where

import Prelude
import Control.Alt ((<|>))
import Data.Argonaut.Combinators ((~>), (:=), (.?))
import Data.Argonaut.Core (jsonEmptyObject)
import Data.Argonaut.Decode (DecodeJson, decodeJson)
import Data.Argonaut.Encode (EncodeJson, encodeJson)
import Data.Date (Date())
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..), maybe)
import Data.Time (Milliseconds(..))
import Global (readInt, isNaN)
import Model.Notebook.Cell.FileInput
import Model.Notebook.Cell.JTableContent
import Model.Notebook.Port
import Model.Resource
import Optic.Core
import Optic.Extended (TraversalP())
import Data.Path.Pathy (rootDir, parseAbsDir, sandbox, (</>), printPath, file)
import Model.Path (DirPath(), phantomNotebookPath)
import Utils (s2i)

import qualified Model.Notebook.Cell.Common as Cm
import qualified Model.Notebook.Cell.Explore as Ex
import qualified Model.Notebook.Cell.Markdown as Ma
import qualified Model.Notebook.Cell.Query as Qu
import qualified Model.Notebook.Cell.Search as Sr
import qualified Model.Notebook.Cell.Viz as Vz

type CellId = Int

type FailureMessage = String

string2cellId :: String -> Either String CellId
string2cellId str = maybe (Left "incorrect cell id") Right $ s2i str


newtype Cell =
  Cell { cellId :: CellId
       , parent :: Maybe CellId
       , input :: Port
       , output :: Port
       , content :: CellContent
       , expandedStatus :: Boolean
       , message :: String
       , failures :: Array FailureMessage
       , hiddenEditor :: Boolean
       , embedHovered :: Boolean
       , runState :: RunState
       , hasRun :: Boolean
       , pathToNotebook :: DirPath
       }

newCell :: CellId -> CellContent -> Cell
newCell cellId content =
  Cell { cellId: cellId
       , parent: Nothing
       , input: Closed
       , output: Closed
       , content: content
       , expandedStatus: false
       , failures: []
       , message: ""
       , hiddenEditor: false
       , runState: RunInitial
       , embedHovered: false
       , hasRun: false
       , pathToNotebook: rootDir
       }

_Cell :: LensP Cell _
_Cell = lens (\(Cell obj) -> obj) (const Cell)

_cellId :: LensP Cell CellId
_cellId = _Cell <<< lens _.cellId (_ { cellId = _ })

_parent :: LensP Cell (Maybe CellId)
_parent = _Cell <<< lens _.parent (_ { parent = _ })

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

_failures :: LensP Cell (Array FailureMessage)
_failures = _Cell <<< lens _.failures (_ { failures = _ })

_message :: LensP Cell String
_message = _Cell <<< lens _.message _{message = _}

_hasRun :: LensP Cell Boolean
_hasRun = _Cell <<< lens _.hasRun _{hasRun = _}

_pathToNotebook :: LensP Cell DirPath
_pathToNotebook = _Cell <<< lens _.pathToNotebook _{pathToNotebook = _}

_embedHovered :: LensP Cell Boolean
_embedHovered = _Cell <<< lens _.embedHovered _{embedHovered = _}


instance eqCell :: Eq Cell where
  eq (Cell c) (Cell c') = c.cellId == c'.cellId

instance ordCell :: Ord Cell where
  compare (Cell c) (Cell c') = compare c.cellId c'.cellId

instance encodeJsonCell :: EncodeJson Cell where
  encodeJson (Cell cell)
    =  "cellId" := cell.cellId
    ~> "parent" := cell.parent
    ~> "input" := cell.input
    ~> "output" := cell.output
    ~> "content" := cell.content
    ~> "hasRun" := cell.hasRun
    ~> "runState" := cell.runState
    ~> jsonEmptyObject

instance decodeJsonCell :: DecodeJson Cell where
  decodeJson json = do
    obj <- decodeJson json
    cell <- newCell <$> obj .? "cellId"
                    <*> obj .? "content"
    parent <- obj .? "parent"
    input <- obj .? "input"
    output <- obj .? "output"
    runState <- obj .? "runState" <|> pure RunInitial
    let hasRun = either (const false) id (obj .? "hasRun")
    pure (cell # (_parent .~ parent)
              .. (_input  .~ input)
              .. (_output .~ output)
              .. (_hasRun .~ hasRun)
              .. (_runState .~ runState)
              .. (_pathToNotebook .~ phantomNotebookPath))

data CellContent
  = Search Sr.SearchRec
  | Explore Ex.ExploreRec
  | Visualize Vz.VizRec
  | Query Qu.QueryRec
  | Markdown Ma.MarkdownRec

cellContentType :: CellContent -> String
cellContentType (Explore _) = "explore"
cellContentType (Search _) = "search"
cellContentType (Query _) = "query"
cellContentType (Visualize _) = "visualize"
cellContentType (Markdown _) = "markdown"

newSearchContent :: Resource -> CellContent
newSearchContent res = Search (Sr.initialSearchRec # Sr._input .. _file .~ Right res)

newQueryContent :: Resource -> CellContent
newQueryContent res = Query (Qu.initialQueryRec # Qu._input .~ "SELECT * FROM \"" ++ resString ++ "\"")
  where resString = if isTempFile res
                    then resourceName res
                    else resourcePath res

newVisualizeContent :: CellContent
newVisualizeContent = Visualize Vz.initialVizRec

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

_Markdown :: PrismP CellContent Ma.MarkdownRec
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
  Markdown _ -> (_Markdown .. Ma._input) f s
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
      Markdown rec -> encodeJson rec
      Visualize rec -> encodeJson rec
      _ -> jsonEmptyObject

instance decodeJsonExploreRec :: DecodeJson CellContent where
  decodeJson json = do
    obj <- decodeJson json
    cellType <- obj .? "type"
    case cellType of
      "search" -> Search <$> decodeJson json
      "explore" -> Explore <$> decodeJson json
      "query" -> Query <$> decodeJson json
      "markdown" -> Markdown <$> decodeJson json
      "visualize" -> Visualize <$> decodeJson json
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


instance encodeRunState :: EncodeJson RunState where
  encodeJson RunInitial = encodeJson "run-initial"
  encodeJson (RunningSince _) = encodeJson ""
  encodeJson (RunFinished (Milliseconds n)) = encodeJson n

instance decodeRunState :: DecodeJson RunState where
  decodeJson json =
    ( do r <- decodeJson json
         case r of
           "run-initial" -> pure RunInitial
           _ -> Left "incorrect RunState"
    ) <|>
    ( RunFinished <$> (Milliseconds <$> decodeJson json))

outFile :: Cell -> Resource
outFile cell = mkFile $ Left $ (cell ^._pathToNotebook) </> file ("out" <> show (cell ^._cellId))
