module Driver.Notebook where

import Api.Fs (loadNotebook)
import Control.Monad (when)
import Control.Monad.Aff (runAff, attempt)
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Exception (message)
import Control.Monad.Eff.Ref (RefVal(), readRef)
import Control.Plus (empty)
import Control.Timer (interval)
import Controller.Notebook (handleMenuSignal)
import Controller.Notebook.Cell.Explore (runExplore)
import Controller.Notebook.Common (run)
import Data.Date (now)
import Data.DOM.Simple.Document
import Data.DOM.Simple.Events (KeyboardEventType(KeydownEvent), metaKey, shiftKey, ctrlKey, keyCode, addKeyboardEventListener, preventDefault)
import Data.DOM.Simple.Types (DOMEvent())
import Data.DOM.Simple.Window (globalWindow, document)
import Data.Either (Either(..))
import Data.Inject1 (inj)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), fst)
import Data.These (theseLeft)
import Driver.Notebook.Routing (Routes(..), routing)
import EffectTypes (NotebookComponentEff(), NotebookAppEff())
import Halogen (Driver())
import Halogen.HTML.Events.Monad (runEvent, andThen)
import Input.Notebook (Input(..))
import Model.Action (isEdit)
import Model.Notebook
import Model.Notebook.Cell (CellContent(..), _cellId)
import Model.Notebook.Cell.Explore (initialExploreRec, _input)
import Model.Notebook.Cell.FileInput
import Model.Notebook.Domain (addCell, emptyNotebook, _activeCellId, _path, _name)
import Model.Notebook.Menu
import Model.Path (decodeURIPath, dropNotebookExt)
import Model.Resource (resourceName, resourceDir)
import Optic.Core ((.~), (^.), (..), (?~))
import Routing (matches')
import Utils (log)

tickDriver :: forall e. Driver Input (NotebookComponentEff e)
                     -> Eff (NotebookAppEff e) Unit
tickDriver k = void $ interval 1000 $ now >>= k <<< WithState <<< (_tickDate .~) <<< Just

driver :: forall e. RefVal State
                 -> Driver Input (NotebookComponentEff e)
                 -> Eff (NotebookAppEff e) Unit
driver ref k =
  matches' decodeURIPath routing \old new -> do
    case new of
      NotebookRoute res editable -> do
        state <- readRef ref
        let name = dropNotebookExt (resourceName res)
            path = resourceDir res
            oldNotebook = state ^. _notebook
            pathChanged = oldNotebook ^. _path /= path
            oldName = theseLeft (oldNotebook ^. _name)
            nameChanged = oldName /= Nothing && oldName /= Just name
        when (pathChanged || nameChanged) do
          flip (runAff (const $ pure unit)) (attempt $ loadNotebook res) \result -> do
            update $ (_loaded .~ true)
                  .. (_editable .~ isEdit editable)
            update case result of
              Left err -> _error ?~ message err
              Right nb -> _notebook .~ nb
        handleShortcuts ref k
      ExploreRoute res ->
        let newNotebook = emptyNotebook # _path .~ resourceDir res
            newCell = Explore (initialExploreRec # _input .. _file .~ Right res)
        in case addCell newCell newNotebook of
          Tuple notebook cell -> do
            update $ (_editable .~ true)
                  .. (_loaded .~ true)
                  .. (_error .~ Nothing)
                  .. (_notebook .~ notebook)
            runEvent (\_ -> log "Error in runExplore in driver") k $
              run cell `andThen` \_ -> runExplore cell
      _ -> update (_error ?~ "Incorrect path")
   where update = k <<< WithState

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
