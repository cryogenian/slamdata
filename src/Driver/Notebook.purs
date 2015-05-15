module Driver.Notebook where

import Data.Either
import Data.Maybe
import Data.Tuple (Tuple(..))
import Control.Monad.Eff
import Control.Monad.Eff.Ref (RefVal(), readRef)
import Control.Monad.Aff (runAff, attempt)
import Data.Array (elemIndex)
import Data.DOM.Simple.Types (DOMEvent())
import Data.DOM.Simple.Document
import Data.DOM.Simple.Events (
  KeyboardEventType(KeydownEvent),
  metaKey, shiftKey, ctrlKey, keyCode,
  addKeyboardEventListener,
  preventDefault)
import Data.DOM.Simple.Window (
  globalWindow,
  document)
import DOM (DOM())
import Utils (log)
import Model.Path (decodeURIPath)
import Model.Action (isEdit)
import Model.Notebook
import EffectTypes
import Halogen.HTML.Events.Monad (runEvent)
import Data.Date (now)
import Controller.Notebook (handleMenuSignal)
import Control.Plus (empty)
import Control.Timer (interval)
import Model.Notebook.Menu (
  MenuNotebookSignal(..),
  MenuInsertSignal(..),
  MenuCellSignal(..),
  MenuHelpSignal(..),
  MenuSignal(..))
import Data.Inject1 (inj)
import Driver.Notebook.Routing (routing, Routes(..))
import qualified Halogen as H
import qualified Routing as R
import qualified Routing.Match as R
import qualified Routing.Match.Class as R
import qualified Routing.Hash as R
import Model.Resource (resourcePath, parent, _name)
import Api.Fs (children)
import Input.Notebook (Input(..))
import App.Notebook.Ace (AceKnot())
import Optic.Core ((.~), (^.))

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
            Left _ ->
              _error .~ "Incorrect path"
            Right siblings -> 
              if elemIndex res siblings == -1 
              then _error .~ ("There is no notebook at " <> resourcePath res)
              else _siblings .~ siblings
        update  (\x -> x # _resource .~ res
                       # _initialName .~ (res ^. _name))
        handleShortcuts ref k

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


