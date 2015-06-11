module Driver.Notebook where

import Api.Fs (loadNotebook, saveNotebook)
import Control.Monad (when, unless)
import Control.Monad.Aff (Aff(), runAff, attempt)
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Exception (Error(), message)
import Control.Monad.Eff.Ref (RefVal(), readRef, modifyRef, writeRef)
import Control.Plus (empty)
import Control.Timer (Timeout(), timeout, clearTimeout, interval)
import Controller.Notebook (handleMenuSignal)
import Controller.Notebook.Cell (requestCellContent)
import Controller.Notebook.Cell.Explore (runExplore)
import Controller.Notebook.Common (run)
import Data.Date (now)
import Data.DOM.Simple.Document
import Data.DOM.Simple.Events (KeyboardEventType(KeydownEvent), metaKey, shiftKey, ctrlKey, keyCode, addKeyboardEventListener, preventDefault)
import Data.DOM.Simple.Types (DOMEvent())
import Data.DOM.Simple.Window (globalWindow, document)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Inject1 (inj)
import Data.Maybe (Maybe(..), isJust, isNothing)
import Data.These (These(..), theseRight, theseLeft)
import Data.Tuple (Tuple(..), fst)
import Driver.Notebook.Routing (Routes(..), routing)
import EffectTypes (NotebookComponentEff(), NotebookAppEff())
import Halogen (Driver())
import Halogen.HTML.Events.Monad (runEvent, andThen)
import Input.Notebook (Input(..))
import Model.Action (Action(Edit), isEdit)
import Model.Notebook
import Model.Notebook.Cell (Cell(), CellId(), CellContent(..), _cellId, _hasRun, _content, _output)
import Model.Notebook.Cell.Explore (initialExploreRec, _input)
import Model.Notebook.Cell.FileInput
import Model.Notebook.Domain (addCell, emptyNotebook, _activeCellId, _path, _name, _cells, notebookURL, cellOut, replacePendingPorts)
import Model.Notebook.Menu
import Model.Notebook.Port
import Model.Path (decodeURIPath, dropNotebookExt)
import Model.Resource (Resource(), resourceName, resourceDir)
import Optic.Core ((.~), (^.), (..), (?~), (%~))
import Routing (matches')
import Utils (log, replaceLocation)

import qualified Data.Maybe.Unsafe as U

tickDriver :: forall e. Driver Input (NotebookComponentEff e)
                     -> Eff (NotebookAppEff e) Unit
tickDriver k = void $ interval 1000 $ now >>= k <<< WithState <<< (_tickDate .~) <<< Just

driver :: forall e. RefVal State
                 -> Driver Input (NotebookComponentEff e)
                 -> Eff (NotebookAppEff e) Unit
driver ref k =
  matches' decodeURIPath routing \old new -> do
    case new of
      CellRoute res cellId editable -> notebook res editable $ Just cellId
      NotebookRoute res editable -> notebook res editable Nothing
      ExploreRoute res -> do
        let newNotebook = emptyNotebook # _path .~ resourceDir res
            newCell = Explore (initialExploreRec # _input .. _file .~ Right res)
        case addCell newCell newNotebook of
          Tuple notebook cell -> do
            update $ (_editable .~ true)
                  .. (_loaded .~ true)
                  .. (_error .~ Nothing)
                  .. (_notebook .~ notebook)
            runEvent (runError "runExplore") k $ run cell `andThen` \_ -> runExplore cell

  where

  update :: (State -> State) -> Eff (NotebookAppEff e) Unit
  update = k <<< WithState

  cellUpdate :: Cell -> (Cell -> Cell) -> Eff (NotebookAppEff e) Unit
  cellUpdate cell = k <<< (UpdateCell (cell ^._cellId))

  runError :: String -> Error -> Eff (NotebookAppEff e) Unit
  runError from err = log $ "Error for " ++ from ++ " in driver: " ++ show err

  notebook :: Resource -> Action -> Maybe CellId -> Eff (NotebookAppEff e) Unit
  notebook res editable viewing = do
    state <- readRef ref
    let name = dropNotebookExt (resourceName res)
        path = resourceDir res
        oldNotebook = state ^. _notebook
        pathChanged = oldNotebook ^. _path /= path
        oldName = theseLeft (oldNotebook ^. _name)
        nameChanged = oldName /= Just name
    when (pathChanged || nameChanged) do
      runAff' (loadNotebook res) \result -> do
        update $ (_loaded .~ true)
              .. (_notebook .. _name .~ This "")
              .. (_editable .~ isEdit editable)
              .. (_viewingCell .~ viewing)
        case result of
          Left err -> update (_error ?~ message err)
          Right nb -> do
            update (_notebook .~ nb)
            for_ (nb ^. _cells) \cell -> do
              case Tuple (cell ^. _content) (cell ^._output) of
                Tuple (Search _) Closed ->
                  cellUpdate cell
                  (_output .~ cellOut (cell ^._content)
                   nb (cell ^._cellId))
                _ -> pure unit
              if ((cell ^. _hasRun) && (isNothing viewing || viewing == Just (cell ^. _cellId)))
                then if isEdit editable
                       then runEvent (runError "requestCellContent") k $ requestCellContent cell
                       else k (ViewCellContent cell)
                else pure unit

handleShortcuts :: forall e. RefVal State
                          -> Driver Input (NotebookComponentEff e)
                          -> Eff (NotebookAppEff e) Unit
handleShortcuts ref k =
  document globalWindow >>= addKeyboardEventListener KeydownEvent (handler ref)
  where
  handler :: RefVal State -> DOMEvent -> _
  handler ref e = void do
    meta <- (||) <$> ctrlKey e <*> metaKey e
    shift <- shiftKey e
    code <- keyCode e
    state <- readRef ref
    when (state ^. _editable) $ do
      let handle signal = do
            preventDefault e
            pure $ handleMenuSignal state signal
      event <- case code of
        83 | meta && shift -> handle $ inj $ RenameNotebook
        80 | meta -> handle $ inj $ PublishNotebook
        49 | meta -> handle $ inj $ QueryInsert
        50 | meta -> handle $ inj $ MarkdownInsert
        51 | meta -> handle $ inj $ ExploreInsert
        52 | meta -> handle $ inj $ SearchInsert
        13 | meta -> handle $ inj $ EvaluateCell
        _ -> pure empty
      runEvent (\_ -> log "Error in handleShortcuts") k event

notebookAutosave :: forall e. RefVal State
                           -> RefVal Timeout
                           -> Input
                           -> Driver Input (NotebookComponentEff e)
                           -> Eff (NotebookAppEff e) Unit
notebookAutosave refState refTimer input k = do
  state <- readRef refState
  let notebook = state ^. _notebook
      hasBeenSaved = isJust $ theseRight (notebook ^. _name)
      save = do
        t <- readRef refTimer
        clearTimeout t
        t' <- timeout 1000 $ runAff' (saveNotebook notebook) \result -> do
          case result of
            Left err -> k $ WithState (_error ?~ message err)
            Right nb -> do
              let update = (_notebook %~ replacePendingPorts)
                       <<< (_notebook .. _name .~ (nb ^. _name))
              k $ WithState update
              -- This crime is to update the state so that when NotebookRoute is
              -- triggered in the router, the state will have the saved-name for
              -- the notebook, which will prevent it from re-loading it. We need
              -- to do this now because the stateRef is updated in postRender,
              -- but changing the URL happens immediately.
              modifyRef refState update
              unless hasBeenSaved $ replaceLocation $ U.fromJust $ notebookURL nb Edit
        writeRef refTimer t'
  case input of
    AddCell _ -> save
    InsertCell _ _ -> save
    UpdateCell _ _ | hasBeenSaved -> save
    CellResult _ _ _ | hasBeenSaved -> save
    TrashCell _ | hasBeenSaved -> save
    _ -> pure unit


runAff' :: forall e a. Aff e a -> (Either Error a -> Eff e Unit) -> Eff e Unit
runAff' aff f = runAff (const $ pure unit) f (attempt aff)
