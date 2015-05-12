module Controller.Notebook (
  I(),
  handleMenuSignal,
  handleSubmitName) where

import Data.Date (now)
import Data.Maybe
import Data.Either
import Control.Alt ((<|>))
import Control.Plus (empty)
import Control.Monad.Eff.Class
import Control.Monad.Aff.Class
import Control.Monad.Aff (attempt)
import Model.Notebook.Menu (
  MenuNotebookSignal(..),
  MenuInsertSignal(..),
  MenuCellSignal(..),
  MenuHelpSignal(..),
  MenuSignal(..))
import Model.Notebook (State(), activeCellId)
import Model.Notebook.Cell (CellType(..))
import Input.Notebook (Input(..), runCellEvent)
import Data.Inject1 (prj)
import Debug.Foreign -- mark for grep -nr to not remove. mocking handlers
import Model.Path (decodeURIPath)
import Api.Fs (move, children, delete)
import Routing.Hash (setHash, getHash)
import Config (notebookExtension, notebookNameEditorId, notebookUrl, homeHash)
import Data.Array (elemIndex)
import Data.DOM.Simple.Window (globalWindow, document, location, setLocation)
import Data.DOM.Simple.Document 
import Data.DOM.Simple.Element (getElementById, focus)
import Utils (newTab, mailOpen)
import Routing (matchHash')
import Driver.Notebook.Routing (routing, Routes(..))
import qualified Data.String.Regex as Rgx
import qualified Halogen.HTML.Events.Types as E
import Optic.Core
import Model.Resource
import Data.Path.Pathy
import Halogen.HTML.Events.Monad (Event())
import EffectTypes (NotebookAppEff())

type I e = Event (NotebookAppEff e) Input

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
  mbRes <- liftEff $ do
    h <- getHash 
    flip (either (const $ pure Nothing))
      (matchHash' decodeURIPath routing h) \r -> do
      case r of
        NotebookRoute res _ -> do
          pure $ Just res
        _ -> pure Nothing
  liftAff $ maybe (pure unit) delete mbRes
  liftEff $ do
    location globalWindow
      >>= setLocation homeHash
  empty

handleMenuCell :: forall e. State -> MenuCellSignal -> I e
handleMenuCell state signal =
  case signal of
    EvaluateCell -> runCellEvent (state ^. activeCellId)
    DeleteCell -> pure $ TrashCell (state ^. activeCellId)
    _ -> empty

handleMenuInsert :: forall e. MenuInsertSignal -> I e
handleMenuInsert ExploreInsert = pure $ AddCell Explore
handleMenuInsert MarkdownInsert = pure $ AddCell Markdown
handleMenuInsert QueryInsert = pure $ AddCell Query
handleMenuInsert SearchInsert = pure $ AddCell Search


handleMenuHelp :: forall e. MenuHelpSignal -> I e
handleMenuHelp TutorialHelp = do
  liftEff $ newTab "http://slamdata.com/documentation"
  empty
handleMenuHelp SQLTutorialHelp = do
  liftEff $ newTab "http://slamdata.com/documentation"
  empty
handleMenuHelp SQLReferenceHelp = do
  liftEff $ newTab "http://slamdata.com/documentation"
  empty
handleMenuHelp ReportBugHelp = do
  liftEff $ mailOpen "mailto:support@slamdatacom?subject=Bug%20Found"
  empty
handleMenuHelp RequestSupportHelp = do 
  liftEff $ mailOpen "mailto:support@slamdatacom?subject=Request%20Help"
  empty
  

handleMenuSignal :: forall e. State -> MenuSignal -> I e
handleMenuSignal state signal =
  maybe empty id $
  (handleMenuNotebook <$> prj signal)
  <|>
  (handleMenuInsert <$> prj signal)
  <|>
  (handleMenuCell state <$> prj signal)
  <|>
  (handleMenuHelp <$> prj signal)

handleSubmitName :: forall e. State -> I e
handleSubmitName state = do
  let oldResource = state.resource # nameL .~ state.name
      newResource = state.resource
      parent = oldResource 
  -- slamdata/slamengine#693
  if oldResource == newResource 
    then empty
    else do
    siblings <- liftAff $ children
                (newDirectory `setPath`
                 (Right
                  (maybe rootDir (rootDir </>)
                   (sandbox rootDir $ resourceDir oldResource))))
    -- slamdata/slamengine#693
    if elemIndex (newResource ^. nameL) ((\x -> x ^. nameL) <$> siblings) /= -1
      then pure $ SetModalError ("File " <> (resourcePath newResource) <> " already exists")
      else do 
      error <- liftAff $ move oldResource (getPath newResource) 
      case error of
        "" -> do
          liftEff $ setHash $ resourcePath newResource <> "/edit"
          empty
        e -> 
          pure $ SetModalError ("Rename error: " <> e)
