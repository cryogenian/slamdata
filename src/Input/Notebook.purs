module Input.Notebook
  ( CellResultContent(..)
  , Input(..)
  , updateState
  , cellContent
  ) where

import Control.Bind (join)
import Data.Array (filter, modifyAt, (!!))
import Data.Date (Date(), toEpochMilliseconds)
import Data.Either (Either(..), either)
import Data.Foldable (intercalate)
import Data.Function (on)
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (fst)
import Model.Notebook
import Model.Notebook.Cell
import Model.Notebook.Cell.FileInput (_showFiles)
import Model.Notebook.Domain (_cells, addCell, insertCell)
import Model.Notebook.Port (Port(..), VarMapValue())
import Optic.Core (LensP(), (..), (<>~), (%~), (+~), (.~), (^.), (?~), lens)
import Optic.Fold ((^?))
import Optic.Setter (mapped)
import Text.Markdown.SlamDown (SlamDown(..), Block(..), Inline(..), FormField(..))
import Text.Markdown.SlamDown.Html (FormFieldValue(..), SlamDownEvent(..), SlamDownState(..), applySlamDownEvent, emptySlamDownState)
import Text.Markdown.SlamDown.Parser (parseMd)

import qualified Data.Array.NonEmpty as NEL
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.StrMap as SM
import qualified ECharts.Options as EC
import qualified Model.Notebook.Cell.JTableContent as JTC
import qualified Model.Notebook.Cell.Markdown as Ma

data CellResultContent
  = AceContent String
  | JTableContent JTC.JTableContent
  | MarkdownContent

data Input
  = WithState (State -> State)
  | Dropdown Number
  | CloseDropdowns
  | AddCell CellContent
  | TrashCell CellId

  | RequestCellContent Cell
  | ReceiveCellContent Cell

  | StartRunCell CellId Date
  | StopCell CellId
  | CellResult CellId Date (Either (NEL.NonEmpty FailureMessage) CellResultContent)

  | UpdateCell CellId (Cell -> Cell)
  | CellSlamDownEvent CellId SlamDownEvent
  | InsertCell Cell CellContent
  | SetEChartsOption String EC.Option

updateState :: State -> Input -> State

updateState state (WithState f) =
  f state

updateState state (UpdateCell cellId fn) =
  state # _notebook.._cells..mapped %~ onCell cellId fn

updateState state (Dropdown i) =
  let visSet = maybe true not (_.visible <$> state.dropdowns !! i) in
  state # _dropdowns %~
  (modifyAt i _{visible = visSet}) <<< (_{visible = false} <$>)

updateState state CloseDropdowns =
  state # (_dropdowns %~ (_{visible = false} <$>))
       .. (_notebook .. _cells .. mapped %~ _content .. _FileInput .. _showFiles .~ false)

updateState state (AddCell content) =
  state # _notebook %~ (fst <<< addCell content)

updateState state (InsertCell parent content) =
  state # _notebook %~ (fst <<< insertCell parent content)

updateState state (TrashCell cellId) =
  state # _notebook.._cells %~ filter (not <<< isCell cellId)

updateState state (CellResult cellId date content) =
  let finished d = RunFinished $ on (-) toEpochMilliseconds date d

      fromState (RunningSince d) = cellContent content <<< setRunState (finished d)
      fromState a = setRunState a -- In bad state or cancelled. Leave content.

      f cell = fromState (cell ^. _runState) cell # (_hasRun .~ true)

  in state # _notebook.._cells..mapped %~ onCell cellId f

updateState state (ReceiveCellContent cell) =
  state # _notebook.._cells..mapped %~ onCell (cell ^. _cellId) (const $  setSlamDownStatus cell)

updateState state (StartRunCell cellId date) =
  state # _notebook.._cells..mapped %~ onCell cellId (setRunState (RunningSince date) <<< (_expandedStatus .~ false))

updateState state (StopCell cellId) =
  state # _notebook.._cells..mapped %~ onCell cellId (setRunState RunInitial)

updateState state (CellSlamDownEvent cellId event) =
  state # _notebook.._cells..mapped %~ onCell cellId (slamDownOutput <<< runSlamDownEvent event)

updateState state i = state

toStrMap :: forall a. M.Map String a -> SM.StrMap a
toStrMap = SM.fromList <<< M.toList

slamDownStateMap :: LensP SlamDownState (M.Map String FormFieldValue)
slamDownStateMap = lens (\(SlamDownState m) -> m) (const SlamDownState)

slamDownOutput :: Cell -> Cell
slamDownOutput cell =
  cell # _output .~ VarMap m
  where
    m = maybe SM.empty ((fromFormValue <$>) <<< toStrMap) state
    fromFormValue (SingleValue s) = s
    fromFormValue (MultipleValues s) = "[" <> intercalate ", " (S.toList s) <> "]" -- TODO: is this anything like we want for multi-values?
    state = cell ^? _content.._Markdown..Ma._state..slamDownStateMap

-- TODO: This should probably live in purescript-markdown
slamDownFields :: SlamDown -> [String]
slamDownFields (SlamDown bs) = bs >>= block
  where
    block (Paragraph is) = is >>= inline
    block (Header _ is) = is >>= inline
    block (Blockquote bs) = bs >>= block
    block (List _ bss) = join bss >>= block
    block _ = []
    inline (Emph is) = is >>= inline
    inline (Strong is) = is >>= inline
    inline (Link is _) = is >>= inline
    inline (Image is _) = is >>= inline
    inline (FormField s _ _) = [s]
    inline _ = []

setSlamDownStatus :: Cell -> Cell
setSlamDownStatus cell =
  let input' = cell ^? _content.._Markdown..Ma._input
      message = ("Exported fields: "<>) <<< intercalate ", " <<< slamDownFields <<< parseMd
      changeMessage m = cell # _message .~ message m
  in maybe cell changeMessage input'

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
    <<< (_content.._Markdown..Ma._state .~ emptySlamDownState)
  success (JTableContent content) =
    setFailures [ ]
    <<< (_content.._JTableContent .~ content)
  success MarkdownContent = setFailures [ ]

onCell :: CellId -> (Cell -> Cell) -> Cell -> Cell
onCell ci f c = if isCell ci c then f c else c

setFailures :: [FailureMessage] -> Cell -> Cell
setFailures fs (Cell o) = Cell $ o { failures = fs }

setRunState :: RunState -> Cell -> Cell
setRunState = modifyRunState <<< const

modifyRunState :: (RunState -> RunState) -> Cell -> Cell
modifyRunState f (Cell o) = Cell $ o { runState = f o.runState }

isCell :: CellId -> Cell -> Boolean
isCell ci (Cell { cellId = ci' }) = ci == ci'
