module Input.Notebook
  ( Input(..)
  , updateState
  , CellResultContent(..)
  ) where

import Control.Monad.Eff.Class (liftEff)
import Control.Timer (Timeout())
import Control.Alt ((<|>))
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
import Text.Markdown.SlamDown.Html (SlamDownEvent())
import Control.Timer (Timeout())
import qualified Data.Array.NonEmpty as NEL

data CellResultContent = AceContent String

data Input
  = WithState (State -> State)
  | Dropdown Number
  | CloseDropdowns
  | AddCell CellContent
  | TrashCell CellId
  | RunCell CellId Date
  | StopCell CellId
  | CellResult CellId Date (Either (NEL.NonEmpty FailureMessage) CellResultContent)
  | UpdateCell CellId (Cell -> Cell)
  | CellSlamDownEvent CellId SlamDownEvent

updateState state (WithState f) =
  f state

updateState state (UpdateCell cellId fn) = 
  state # _notebook.._notebookCells..mapped %~ onCell cellId fn

updateState state (Dropdown i) =
  let visSet = maybe true not (_.visible <$> state.dropdowns !! i) in
  state # _dropdowns %~
  (modifyAt i _{visible = visSet}) <<<  (_{visible = false} <$>)

updateState state CloseDropdowns =
  state{addingCell = false} # _dropdowns %~ (_{visible = false} <$>)

updateState state (AddCell cellType) =
  state # _notebook %~ (addCell cellType Nothing >>> fst)

updateState state (TrashCell cellId) =
  state # _notebook.._notebookCells %~ filter (not <<< isCell cellId)

updateState state (CellResult cellId date content) =
  let f (RunningSince d) = RunFinished $ on (-) toEpochMilliseconds date d
      f _                = RunInitial -- TODO: Cell in bad state
  in state # _notebook.._notebookCells..mapped %~ onCell cellId (modifyRunState f <<< cellContent content)


updateState state (RunCell cellId date) =
  state # _notebook.._notebookCells..mapped %~ onCell cellId (modifyRunState <<< const $ RunningSince date)

updateState state i = state

cellContent :: Either (NEL.NonEmpty FailureMessage) CellResultContent -> Cell -> Cell
cellContent = either (setFailures <<< NEL.toArray) success
  where success (AceContent content) = setFailures [ ] <<< setContent content

onCell :: CellId -> (Cell -> Cell) -> Cell -> Cell
onCell ci f c = if isCell ci c then f c else c

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
