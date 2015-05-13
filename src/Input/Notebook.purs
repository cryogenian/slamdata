module Input.Notebook
  ( Input()
  , NotebookInput(..)
  , runCellEvent
  , updateState
  , CellResultContent(..)
  ) where

import Control.Monad.Eff.Class (liftEff)
import Control.Timer (Timeout())
import Data.Array (filter, modifyAt, (!!))
import Data.Date (Date(), Now(), now, toEpochMilliseconds)
import Data.Either (Either(), either)
import Data.Function (on)
import Data.Inject1 (inj, prj)
import Data.Maybe (maybe, Maybe(..))
import Data.Maybe.Unsafe (fromJust)
import Data.Tuple (fst)
import Halogen.HTML.Events.Monad (Event(), async)
import Model.Notebook
import Model.Notebook.Cell
import Model.Notebook.Domain (_notebookCells, addCell)
import Model.Resource (_name, Resource())
import Optic.Core ((..), (<>~), (%~), (+~), (.~), (^.))
import Optic.Setter (mapped, over)
import qualified Data.Array.NonEmpty as NEL

data CellResultContent = AceContent String

type Input = Either NotebookInput Unit

data NotebookInput
  = Dropdown Number
  | CloseDropdowns
  | SetTimeout (Maybe Timeout)
  | SetName String
  | SetResource Resource
  | SetSiblings [Resource]
  | SetLoaded Boolean
  | SetError String
  | SetEditable Boolean
  | SetModalError String
  | SetAddingCell Boolean
  | AddCell CellContent
  | ToggleEditorCell CellId
  | ToggleFailuresCell CellId
  | TrashCell CellId
  | RunCell CellId Date
  | StopCell CellId
  | CellResult CellId Date (Either (NEL.NonEmpty FailureMessage) CellResultContent)
  | SetActiveCell CellId
  | SecondTick Date

runCellEvent :: forall eff. CellId -> Event (now :: Now | eff) Input
runCellEvent cid = async $ inj <<< RunCell cid <$> liftEff now

updateState :: State -> Input -> State
updateState state input =
  fromJust $ (inputNotebook state <$> prj input)

inputNotebook :: State -> NotebookInput -> State

inputNotebook state (Dropdown i) =
  let visSet = maybe true not (_.visible <$> state.dropdowns !! i) in
  state # _dropdowns %~
  (modifyAt i _{visible = visSet}) <<<  (_{visible = false} <$>)

inputNotebook state CloseDropdowns =
  state{addingCell = false} # _dropdowns %~ (_{visible = false} <$>)

inputNotebook state (SetTimeout mbTm) =
  state{timeout = mbTm}

inputNotebook state (SetResource res) =
  state{resource = res, initialName = res ^. _name}

inputNotebook state (SetName name) =
  state{resource = state.resource # _name .~ name}

inputNotebook state (SetSiblings ss) =
  state{siblings = ss}

inputNotebook state (SetLoaded loaded) =
  state{loaded = loaded}

inputNotebook state (SetError error) =
  state{error = error}

inputNotebook state (SetEditable editable) =
  state{editable = editable}

inputNotebook state (SetModalError error) =
  state{modalError = error}

inputNotebook state (SetAddingCell adding) =
  state{addingCell = adding}

inputNotebook state (AddCell cellType) =
  state # _notebook %~ (addCell cellType Nothing >>> fst)

inputNotebook state (ToggleEditorCell cellId) =
  state # _notebook.._notebookCells..mapped %~ onCell cellId toggleEditor

inputNotebook state (TrashCell cellId) =
  state # _notebook.._notebookCells %~ filter (not <<< isCell cellId)

inputNotebook state (CellResult cellId date content) =
  let f (RunningSince d) = RunFinished $ on (-) toEpochMilliseconds date d
      f _                = RunInitial -- TODO: Cell in bad state
  in state # _notebook.._notebookCells..mapped %~ onCell cellId (modifyRunState f <<< cellContent content)

inputNotebook state (SetActiveCell cellId) =
  state # _activeCellId .~ cellId

inputNotebook state (RunCell cellId date) =
  state # _notebook.._notebookCells..mapped %~ onCell cellId (modifyRunState <<< const $ RunningSince date)

inputNotebook state (SecondTick date) =
  state{tickDate = Just date}

inputNotebook state i = state

cellContent :: Either (NEL.NonEmpty FailureMessage) CellResultContent -> Cell -> Cell
cellContent = either (setFailures <<< NEL.toArray) success
  where success (AceContent content) = setFailures [ ] <<< setContent content

onCell :: CellId -> (Cell -> Cell) -> Cell -> Cell
onCell ci f c = if isCell ci c then f c else c

toggleEditor :: Cell -> Cell
toggleEditor (Cell o) = Cell $ o { hiddenEditor = not o.hiddenEditor }

toggleFailures :: Cell -> Cell
toggleFailures (Cell o) = Cell $ o { expandedStatus = not o.expandedStatus }

setFailures :: [FailureMessage] -> Cell -> Cell
setFailures fs (Cell o) = Cell $ o { failures = fs }

setContent :: String -> Cell -> Cell
setContent content (Cell o) =
  let content' = case o.content of
                  Evaluate _ -> Evaluate content
                  Search _ -> Search content
                  Query _ -> Query content
                  Visualize _ -> Visualize content
                  Markdown _ -> Markdown content
                  other -> other
  in Cell $ o { content = content' }

modifyRunState :: (RunState -> RunState) -> Cell -> Cell
modifyRunState f (Cell o) = Cell $ o { runState = f o.runState }

isCell :: CellId -> Cell -> Boolean
isCell ci (Cell { cellId = ci' }) = ci == ci'
