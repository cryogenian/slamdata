module Driver.Notebook where

import Config
import Data.Either
import Data.List
import Data.Maybe
import Control.Alt
import Control.Apply
import Control.Monad.Eff
import Control.Monad.Aff (runAff, attempt)
import Data.Array (findIndex)
import Model.Path
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
import Model.Action
import qualified Utils as U
import qualified Data.String as Str
import qualified Data.String.Regex as Rgx
import qualified Halogen as H
import qualified Routing as R
import qualified Routing.Match as R
import qualified Routing.Match.Class as R
import qualified Routing.Hash as R
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


data Routes
  = CellRoute Path CellId Action
  | NotebookRoute Path Action


routing :: R.Match Routes
routing = CellRoute <$> notebook <*> (R.lit "cells" *> cellId) <*> action
          <|>
          NotebookRoute <$> notebook <*> action
  where notebook = Path <$> (oneSlash *> (R.list notName)) <*> name
        oneSlash = R.lit ""
        notebookName input =
          if Str.indexOf notebookExtension input == -1 then
            Left input
          else Right input
        pathPart input =
          if input == "" || Str.indexOf notebookExtension input /= -1 then
            Left "incorrect path part"
          else Right input

        name = R.eitherMatch (notebookName <$> R.str)
        notName = R.eitherMatch (pathPart <$> R.str)

        action = (R.eitherMatch (string2action <$> R.str)) <|> pure View
        cellId = R.eitherMatch (string2cellId <$> R.str)


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
    preventDefault e 
    meta <- (||) <$> ctrlKey e <*> metaKey e
    shift <- shiftKey e
    code <- keyCode e 
    let event =  case code of
          83 | meta && shift ->
            handleMenuSignal $ inj $ RenameNotebook
          80 | meta ->
            handleMenuSignal $ inj $ PublishNotebook
          67 | meta ->
            handleMenuSignal $ inj $ CopyEdit 
          88 | meta ->
            handleMenuSignal $ inj $ CutEdit 
          86 | meta ->
            handleMenuSignal $ inj $ PasteEdit 
          49 | meta ->
            handleMenuSignal $ inj $ QueryInsert 
          50 | meta ->
            handleMenuSignal $ inj $ MarkdownInsert 
          51 | meta ->
            handleMenuSignal $ inj $ ExploreInsert 
          52 | meta ->
            handleMenuSignal $ inj $ SearchInsert 
          13 | meta ->
            handleMenuSignal $ inj $ EvaluateCell
          _ -> empty
    runEvent (\_ -> U.log "Error in handleShortcuts") k event
    
                     
