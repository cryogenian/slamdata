module Controller.Notebook (
  handleMenuSignal,
  handleSubmitName) where

import Data.Maybe
import Data.Either
import Control.Alt ((<|>))
import Control.Plus (empty)
import Control.Monad.Eff.Class
import Control.Monad.Aff.Class
import Control.Monad.Aff (attempt)
import Model.Notebook.Menu (
  MenuNotebookSignal(..),
  MenuEditSignal(..),
  MenuInsertSignal(..),
  MenuCellSignal(..),
  MenuHelpSignal(..),
  MenuSignal(..))
import Model.Notebook (I(), State(), Input(..), CellType(..))
import Data.Inject1 (prj)
import Debug.Foreign -- mark for grep -nr to not remove. mocking handlers
import Model.Path (path2str, updateName, parent, getName, decodeURIPath)
import Model.File.Item (initNotebook, itemPath)
import Api.Fs (moveItem, listing, delete)
import Routing.Hash (setHash, getHash)
import Config (notebookExtension, notebookNameEditorId, notebookUrl, homeHash)
import Data.Array (elemIndex)
import Data.DOM.Simple.Window (globalWindow, document, location, setLocation)
import Data.DOM.Simple.Document 
import Data.DOM.Simple.Element (getElementById, focus)
import Utils (newTab)
import Routing (matchHash')
import Driver.Notebook.Routing (routing, Routes(..))
import qualified Data.String.Regex as Rgx
import qualified Halogen.HTML.Events.Types as E

handleMenuNotebook :: forall e. MenuNotebookSignal -> I e
handleMenuNotebook RenameNotebook = do
  liftEff $ do
    document globalWindow
      >>= getElementById notebookNameEditorId
      >>= maybe (pure unit) focus
  pure $ CloseDropdowns
handleMenuNotebook PublishNotebook = do
  liftEff $ do
    (toView <$> getHash) >>= newTab
  empty
  where
  editSuffix = Rgx.regex "/edit$" Rgx.noFlags
  toView :: String -> String
  toView = ((notebookUrl <> "#") <>) <<< 
           (Rgx.replace editSuffix "/view")
handleMenuNotebook signal = do
  mbPath <- liftEff $ do
    h <- getHash 
    flip (either (const $ pure Nothing))
      (matchHash' decodeURIPath routing h) \r -> do
      case r of
        NotebookRoute path _ -> do
          pure $ Just (path2str path) 
        _ -> pure Nothing
  liftAff $ maybe (pure unit) delete mbPath
  liftEff $ do
    location globalWindow
      >>= setLocation homeHash
  empty

handleMenuEdit :: forall e. MenuEditSignal -> I e
handleMenuEdit signal = do
  liftEff $ fprint signal
  empty

handleMenuInsert :: forall e. MenuInsertSignal -> I e
handleMenuInsert ExploreInsert = pure $ AddCell Explore
handleMenuInsert MarkdownInsert = pure $ AddCell Markdown
handleMenuInsert QueryInsert = pure $ AddCell Query
handleMenuInsert SearchInsert = pure $ AddCell Search

handleMenuCell :: forall e. MenuCellSignal -> I e
handleMenuCell signal = do
  liftEff $ fprint signal
  empty

handleMenuHelp :: forall e. MenuHelpSignal -> I e
handleMenuHelp signal = do
  liftEff $ fprint signal
  empty

handleMenuSignal :: forall e. MenuSignal -> I e
handleMenuSignal signal =
  maybe empty id $
  (handleMenuNotebook <$> prj signal)
  <|>
  (handleMenuEdit <$> prj signal)
  <|>
  (handleMenuInsert <$> prj signal)
  <|>
  (handleMenuCell <$> prj signal)
  <|>
  (handleMenuHelp <$> prj signal)

handleSubmitName :: forall e. State -> I e
handleSubmitName state = do
  let root = (path2str $ parent $ state.path) <> "/"
      oldItem = initNotebook {
        root = root,
        name = state.name
        }
      destination = path2str $ state.path
      newName = getName state.path
      newItem = oldItem {
        name = newName
        }

  -- slamdata/slamengine#693
  if newName == state.name || newName <> notebookExtension == state.name
    then empty
    else do
    siblings <- liftAff $ listing root
    -- slamdata/slamengine#693
    if elemIndex newName (_.name <$> siblings) /= -1
      then pure $ SetModalError ("File " <> destination <> " already exists")
      else do 
      error <- liftAff $ moveItem oldItem destination
      case error of
        "" -> do
          liftEff $ setHash $ itemPath newItem <> "/edit"
          empty
        e -> 
          pure $ SetModalError ("Rename error: " <> e)
