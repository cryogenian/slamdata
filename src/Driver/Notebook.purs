module Driver.Notebook where

import Data.Either
import Data.Maybe
import Control.Monad.Eff
import Control.Monad.Aff (runAff, attempt)
import Data.Array (findIndex)
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
import Model.Path (decodeURIPath, path2str, parent, getName)
import Model.Action (isEdit)
import Model.Notebook
import Model.File.Item
import Api.Fs (listing)
import EffectTypes
import Halogen.HTML.Events.Monad (runEvent)
import Controller.Notebook (handleMenuSignal)
import Control.Plus (empty)
import Model.Notebook.Menu (
  MenuNotebookSignal(..),
  MenuEditSignal(..),
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

driver :: forall e. H.Driver Input (NotebookComponentEff e) ->
          Eff (NotebookAppEff e) Unit
driver k =
  R.matches' decodeURIPath routing \old new -> do
    case new of
      NotebookRoute path editable -> do
        k $ SetEditable (isEdit editable) 
        let uri = (path2str $ parent path) <> "/"
        flip (runAff (const $ pure unit)) (attempt $ listing uri) $ \ei -> do
          k $ SetLoaded true
          k $ case ei of
            Left _ ->
              SetError "Incorrect path" 
            Right items -> 
              if findIndex (\x -> x.name == getName path) items == -1
              then SetError ("There is no notebook at " <> path2str path)
              else SetItems items
        k $ SetPath path

        handleShortcuts k
        
      _ -> k $ SetError "Incorrect path"

handleShortcuts :: forall e. (Input -> Eff (NotebookAppEff e) Unit) ->
                   Eff (NotebookAppEff e) Unit
handleShortcuts k = 
  document globalWindow >>=
  addKeyboardEventListener KeydownEvent handler 
  where
  handler :: DOMEvent -> _
  handler e = void do
    meta <- (||) <$> ctrlKey e <*> metaKey e
    shift <- shiftKey e
    code <- keyCode e
    let handle signal = do
          preventDefault e
          pure $ handleMenuSignal signal
    event <- case code of
      83 | meta && shift -> do 
        handle $ inj $ RenameNotebook
      80 | meta ->
        handle $ inj $ PublishNotebook
      67 | meta ->
        handle $ inj $ CopyEdit 
      88 | meta ->
        handle $ inj $ CutEdit 
      86 | meta ->
        handle $ inj $ PasteEdit 
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
    
                     
