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

module Input.Notebook
  ( CellResultContent(..)
  , Input(..)
  , updateState
  , cellContent
  ) where

import Prelude
import Control.Alt ((<|>))
import Control.Apply ((*>))
import Control.Bind (join)
import Data.Array (filter, modifyAt, (!!), elemIndex, (\\))
import Data.Date (Date(), toEpochMilliseconds)
import Data.Either (Either(..), isRight, either)
import Data.Foldable (intercalate, foldl, elem)
import Data.Function (on)

import Data.Maybe (Maybe(..), fromMaybe, maybe, isJust)
import Data.String (indexOf)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), fst, snd)
import Model.Notebook
import Model.Resource (Resource())
import Model.Notebook.Cell
import Model.Notebook.Cell.FileInput (_showFiles)
import Model.Notebook.Domain (_cells, addCell, insertCell, _dependencies, Notebook(), trash, ancestors)
import Model.Notebook.Port (Port(..), VarMapValue(), _PortResource, _VarMap)
import Optic.Core
import Optic.Fold ((^?))
import Optic.Setter (mapped)
import Text.Markdown.SlamDown (SlamDown(..), Block(..), Expr(..), Inline(..), FormField(..), TextBoxType(..), everything)
import Text.Markdown.SlamDown.Html (FormFieldValue(..), SlamDownEvent(..), SlamDownState(..), applySlamDownEvent, initSlamDownState)
import Text.Markdown.SlamDown.Parser (parseMd)

import qualified Data.Array.NonEmpty as NEL
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.String.Regex as Rx
import qualified Data.StrMap as SM
import qualified ECharts.Options as EC
import qualified Model.Notebook.Cell.JTableContent as JTC
import qualified Model.Notebook.Cell.Markdown as Ma
import qualified Text.Parsing.StringParser as SP
import qualified Text.Parsing.StringParser.Combinators as SP
import qualified Text.Parsing.StringParser.String as SP
import Model.Path (FilePath(), AnyPath())

data CellResultContent
  = AceContent String
  | JTableContent JTC.JTableContent
  | MarkdownContent SlamDown

data Input
  = WithState (State -> State)
  | Dropdown Int
  | CloseDropdowns
  | AddCell CellContent
  | TrashCell CellId

  | RequestCellContent Cell
  | ReceiveCellContent Cell
  | ViewCellContent Cell
  | RefreshCell Cell

  | StartRunCell CellId Date
  | StopCell CellId
  | CellResult CellId Date (Either (NEL.NonEmpty FailureMessage) CellResultContent)

  | UpdateCell CellId (Cell -> Cell)
  | CellSlamDownEvent CellId SlamDownEvent
  | InsertCell Cell CellContent
  | SetEChartsOption String EC.Option
  | ResizeECharts String

  | UpdatedOutput CellId Port
  | ForceSave


updateState :: State -> Input -> State

updateState state (WithState f) =
  f state

updateState state (UpdateCell cellId fn) =
  state # _notebook.._cells..mapped %~ onCell cellId fn

updateState state (Dropdown i) =
  let visSet = maybe true not (_.visible <$> state.dropdowns !! i) in
  state # _dropdowns %~
  ((fromMaybe (state ^. _dropdowns)) <<<
   (modifyAt i _{visible = visSet}) <<<
   (_{visible = false} <$>))

updateState state CloseDropdowns =
  state # (_dropdowns %~ (_{visible = false} <$>))
       .. (_notebook .. _cells .. mapped %~ _content .. _FileInput .. _showFiles .~ false)

updateState state (AddCell content) =
  state # _notebook %~ (fst <<< addCell content)

updateState state (InsertCell parent content) =
  state # _notebook %~ (fst <<< insertCell parent content)

updateState state (TrashCell cellId) =
  state # _notebook.._cells %~ filter (not <<< depends (state ^._notebook) cellId)
        # _notebook.._cells %~ filter (not <<< isCell cellId)
        # _notebook.._dependencies %~ trash cellId

updateState state (CellResult cellId date content) =
  let finished d = RunFinished $ on (-) toEpochMilliseconds date d

      fromState (RunningSince d) = cellContent content <<< setRunState (finished d)
      fromState a = setRunState a -- In bad state or cancelled. Leave content.

      f cell = fromState (cell ^. _runState) cell # (_hasRun .~ true)

  in state # _notebook.._cells..mapped %~ onCell cellId f
           # _notebook.._cells %~ syncParents

updateState state (ReceiveCellContent cell) =
  state # _notebook.._cells..mapped %~ onCell (cell ^. _cellId) (const $ setSlamDownStatus cell)

updateState state (StartRunCell cellId date) =
  state # _notebook.._cells..mapped %~ onCell cellId (setRunState (RunningSince date) <<< (_expandedStatus .~ false))

updateState state (StopCell cellId) =
  state # _notebook.._cells..mapped %~ onCell cellId (setRunState RunInitial)

updateState state (CellSlamDownEvent cellId event) =
  state # _notebook.._cells..mapped %~ onCell cellId (cellUpdateVarMap <<< runSlamDownEvent event)
        # _notebook.._cells %~ syncParents
        # _requesting <>~ [cellId]

updateState state (UpdatedOutput cid newInput) =
  state # _notebook.._cells..mapped %~ changed
  where
  changed cell = if not $ elem (cell ^. _cellId) depIds
                 then cell
                 else cell # _input .~ newInput
  depIds =
    fst <$>
    L.filter (\x -> snd x == cid) (M.toList (state ^. _notebook .. _dependencies))


updateState state (RefreshCell cell) =
  let requesting = state ^. _requesting
      cid = cell ^. _cellId
  in if elem cid requesting
     then state
     else state # _refreshing <>~ [cell ^. _cellId]

updateState state i = state


_SlamDownState :: LensP SlamDownState (SM.StrMap FormFieldValue)
_SlamDownState = lens (\(SlamDownState m) -> m) (const SlamDownState)

-- TODO: We need a much better solution to this.
syncParents :: Array Cell -> Array Cell
syncParents cells = updateCell <$> cells
  where updateCell cell = fromMaybe cell $ fromParent cell
        fromParent cell = do
          pid <- cell ^. _parent
          parent <- M.lookup pid m
          return (cell # _input .~ (parent ^. _output))
        m = M.fromList <<< L.toList $ pair <$> cells
        pair cell = Tuple (cell ^. _cellId) cell


-- | Renders the cell's `SlamDownState` into a string map of `VarMapValue`s and
-- | appends it to the cell's output `VarMap`.
cellUpdateVarMap :: Cell -> Cell
cellUpdateVarMap cell =
  cell # _output .. _VarMap  %~ SM.union (renderFormFieldValue <$> slamDownState)
  where
    slamDownState :: SM.StrMap FormFieldValue
    slamDownState = cell ^? _content .. _Markdown .. Ma._state .. _SlamDownState # fromMaybe SM.empty

    renderFormFieldValue :: FormFieldValue -> VarMapValue
    renderFormFieldValue formFieldValue =
      case formFieldValue of
        SingleValue ty s ->
          case ty of
            PlainText -> quoteString s
            Numeric | isSQLNum s -> s
                    | otherwise -> quoteString s
            Date -> "DATE '" ++ s ++ "'"
            Time -> "TIME '" ++ s ++ "'"
            DateTime -> "TIMESTAMP '" ++ s ++ ":00Z'"
        MultipleValues s ->
          "[" <> intercalate ", " (quoteString <$> S.toList s) <> "]"
          -- TODO: is this anything like we want for multi-values?

    quoteString :: String -> String
    quoteString s = "'" ++ Rx.replace rxQuot "''" s ++ "'"
      where
        rxQuot :: Rx.Regex
        rxQuot = Rx.regex "'" Rx.noFlags { global = true }

    isSQLNum :: String -> Boolean
    isSQLNum s = isRight $ flip SP.runParser s $ do
      SP.many1 SP.anyDigit
      SP.optional $ SP.string "." *> SP.many SP.anyDigit
      SP.optional $ (SP.string "e" <|> SP.string "E")
                 *> (SP.string "-" <|> SP.string "+")
                 *> SP.many SP.anyDigit
      SP.eof

slamDownFields :: SlamDown -> L.List (Tuple String FormField)
slamDownFields = everything (const L.Nil) go
  where
  go (FormField s _ ff) = L.singleton (Tuple s ff)
  go _ = L.Nil

setSlamDownStatus :: Cell -> Cell
setSlamDownStatus cell =
  let input' = cell ^? _content.._Markdown..Ma._input
      message = ("Exported fields: " <>) <<< intercalate ", " <<< (fst <$>)
      initial md =
        let fields = slamDownFields md
            updateSlamDownState (SlamDownState s) = case initSlamDownState md of
              SlamDownState s' ->
                let prunedSM = foldl (flip SM.delete) s $ keysToPrune s $ SM.fromList fields
                    mergedSM = prunedSM `SM.union` s'
                in SlamDownState mergedSM
        in cellUpdateVarMap (cell # _message .~ message fields
                                  # _content .. _Markdown .. Ma._state %~ updateSlamDownState)
  in maybe cell (initial <<< parseMd) input'

  where
    -- | Returns the keys that are either not present in the new state, or have had their types changed.
    keysToPrune :: SM.StrMap FormFieldValue -> SM.StrMap FormField -> Array _
    keysToPrune oldState newState =
      SM.foldMap
        (\key oldVal ->
            case Tuple oldVal (SM.lookup key newState) of
               Tuple (SingleValue tbt1 _) (Just (TextBox tbt2 _)) -> if tbt1 == tbt2 then [] else [key]
               Tuple _ Nothing -> [key]
               _ -> [])
        oldState

runSlamDownEvent :: SlamDownEvent -> Cell -> Cell
runSlamDownEvent event cell =
  cell # _content.._Markdown..Ma._state %~ flip applySlamDownEvent event

cellContent :: Either (NEL.NonEmpty FailureMessage) CellResultContent -> Cell -> Cell
cellContent = either (setFailures <<< NEL.toArray) success
  where
  success :: CellResultContent -> Cell -> Cell
  success (AceContent content) =
    setFailures [ ]
    <<< (_content.._AceContent .~ content)
  success (JTableContent content) =
    setFailures [ ]
    <<< (_content.._JTableContent .~ content)
  success (MarkdownContent content) =
    setFailures [ ]
    <<< (_content.._Markdown..Ma._evaluated ?~ content)

onCell :: CellId -> (Cell -> Cell) -> Cell -> Cell
onCell ci f c = if isCell ci c then f c else c

setFailures :: Array FailureMessage -> Cell -> Cell
setFailures fs (Cell o) = Cell $ o { failures = fs }

setRunState :: RunState -> Cell -> Cell
setRunState = modifyRunState <<< const

modifyRunState :: (RunState -> RunState) -> Cell -> Cell
modifyRunState f (Cell o) = Cell $ o { runState = f o.runState }

isCell :: CellId -> Cell -> Boolean
isCell ci (Cell { cellId = ci' }) = ci == ci'

depends :: Notebook -> CellId -> Cell -> Boolean
depends notebook cellId cell =
  isJust $ elemIndex cellId (ancestors (cell ^._cellId) (notebook ^._dependencies))
