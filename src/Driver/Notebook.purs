module Driver.Notebook where

import Api.Fs (children)
import App.Notebook.Ace (AceKnot())
import Control.Monad.Aff (runAff, attempt)
import Control.Monad.Eff
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Ref (RefVal(), readRef)
import Control.Plus (empty)
import Control.Timer (interval)
import Controller.Notebook (handleMenuSignal)
import Controller.Notebook.Cell.Explore (runExplore)
import Data.Array (elemIndex)
import Data.Date (now)
import Data.DOM.Simple.Document
import Data.DOM.Simple.Events (KeyboardEventType(KeydownEvent), metaKey, shiftKey, ctrlKey, keyCode, addKeyboardEventListener, preventDefault)
import Data.DOM.Simple.Types (DOMEvent())
import Data.DOM.Simple.Window (globalWindow, document)
import Data.Either
import Data.Inject1 (inj)
import Data.Maybe
import Data.Tuple (Tuple(..), fst, snd)
import DOM (DOM())
import Driver.Notebook.Routing (routing, Routes(..))
import EffectTypes
import Halogen.HTML.Events.Monad (runEvent, andThen)
import Input.Notebook (Input(..))
import Model.Action (isEdit)
import Model.Notebook
import Model.Notebook.Cell (CellContent(..), _cellId)
import Model.Notebook.Cell.Explore (initialExploreRec, _input)
import Model.Notebook.Cell.FileInput
import Model.Notebook.Menu
import Model.Path (decodeURIPath)
import Model.Resource (resourcePath, parent, _name, _root)
import qualified Model.Notebook.Domain as D 
import Optic.Core ((.~), (^.), (..), (?~))
import Utils (log)

import qualified Halogen as H
import qualified Routing as R
import qualified Routing.Hash as R
import qualified Routing.Match as R
import qualified Routing.Match.Class as R

tickDriver :: forall e. H.Driver Input (NotebookComponentEff e) -> Eff (NotebookAppEff e) Unit
tickDriver k = void $ interval 1000 $ now >>= k <<< WithState <<< (_tickDate .~) <<< Just

driver :: forall e. RefVal AceKnot ->
          H.Driver Input (NotebookComponentEff e) ->
          Eff (NotebookAppEff e) Unit
driver ref k =
  R.matches' decodeURIPath routing \old new -> do
    case new of
      NotebookRoute res editable -> do
        update (_editable .~ isEdit editable)
        flip (runAff (const $ pure unit)) (attempt $ children (parent res)) $ \ei -> do
          update (_loaded .~ true)
          update case ei of
            Left _ -> _error .~ "Incorrect path"
            Right siblings ->
              if elemIndex res siblings == -1
              then _error .~ ("There is no notebook at " <> resourcePath res)
              else _siblings .~ siblings
        update $ (_resource .~ res)
              .. (_initialName .~ (res ^. _name))
        handleShortcuts ref k
      ExploreRoute res ->
        -- TODO: what to do about the resource here? it doesn't exist yet, 
        -- and we can't really invent a name for it until we actually need to
        -- save it
        let newNote = D.emptyNotebook # D._resource.._root .~ (res ^._root)
            newCell = Explore (initialExploreRec # _input .. _file .~ Right res)
        in case D.addCell newCell newNote of 

          Tuple notebook cell -> do
            update $ (_editable .~ true)
                  .. (_loaded .~ true)
                  .. (_error .~ "")
                  .. (_initialName .~ "")
                  .. (_notebook .~ notebook)
            runEvent (\_ -> log "Error in runExplore in driver") k $
              ((RunCell (cell ^. _cellId)) <$> liftEff now) `andThen` \_ -> runExplore cell
      _ -> update (_error .~ "Incorrect path")
   where update = k <<< WithState

handleShortcuts :: forall e. RefVal AceKnot ->
                   (Input -> Eff (NotebookAppEff e) Unit) ->
                   Eff (NotebookAppEff e) Unit
handleShortcuts ref k =
  document globalWindow >>=
  addKeyboardEventListener KeydownEvent (handler ref)
  where
  handler :: RefVal AceKnot -> DOMEvent -> _
  handler ref e = void do
    meta <- (||) <$> ctrlKey e <*> metaKey e
    shift <- shiftKey e
    code <- keyCode e
    Tuple cid _ <- readRef ref
    let handle signal = do
          preventDefault e
          pure $ handleMenuSignal (initialState # _activeCellId .~ cid) signal
    event <- case code of
      83 | meta && shift -> do
        handle $ inj $ RenameNotebook
      80 | meta ->
        handle $ inj $ PublishNotebook
      49 | meta ->
        handle $ inj $ QueryInsert
      50 | meta ->
        handle $ inj $ MarkdownInsert
      51 | meta ->
        handle $ inj $ ExploreInsert
      52 | meta ->
        handle $ inj $ SearchInsert
      13 | meta ->
        handle $ inj $ EvaluateCell
      _ -> pure empty
    runEvent (\_ -> log "Error in handleShortcuts") k event


