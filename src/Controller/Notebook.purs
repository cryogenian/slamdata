module Controller.Notebook
  ( I()
  , handleMenuSignal
  , handleSubmitName
  , handleNameInput
  ) where

import Api.Fs (move, children, delete, saveNotebook)
import Config (notebookNameEditorId, notebookUrl, homeHash)
import Control.Alt ((<|>))
import Control.Monad.Aff (attempt)
import Control.Monad.Aff.Class
import Control.Monad.Eff.Class
import Control.Monad.Eff.Exception (Error(), message)
import Control.Plus (empty)
import Controller.Notebook.Cell (requestCellContent)
import Data.Array (elemIndex)
import Data.Date (now)
import Data.DOM.Simple.Document
import Data.DOM.Simple.Element (getElementById, focus)
import Data.DOM.Simple.Window (globalWindow, document)
import Data.Either
import Data.Inject1 (prj)
import Data.Maybe
import Data.Path.Pathy
import Data.These
import Driver.Notebook.Routing (routing, Routes(..))
import EffectTypes (NotebookAppEff())
import Halogen.HTML.Events.Monad (Event())
import Input.Notebook (Input(..))
import Model.Action (Action(Edit))
import Model.Notebook (State(), _dialog, _notebook)
import Model.Notebook.Cell (CellContent(..))
import Model.Notebook.Cell.Explore (initialExploreRec)
import Model.Notebook.Cell.Markdown (initialMarkdownRec)
import Model.Notebook.Cell.Query (initialQueryRec)
import Model.Notebook.Cell.Search (initialSearchRec)
import Model.Notebook.Dialog
import Model.Notebook.Menu (MenuNotebookSignal(..), MenuInsertSignal(..), MenuCellSignal(..), MenuHelpSignal(..), MenuSignal(..))
import Model.Path (decodeURIPath)
import Model.Resource
import Optic.Core
import Optic.Fold ((^?))
import Routing (matchHash')
import Routing.Hash (setHash, getHash)
import Utils (newTab, mailOpen, setLocation)

import qualified Data.Maybe.Unsafe as U
import qualified Data.String.Regex as Rgx
import qualified Halogen.HTML.Events.Types as E
import qualified Model.Notebook.Domain as N

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
  liftEff $ setLocation homeHash
  empty

handleMenuCell :: forall e. State -> MenuCellSignal -> I e
handleMenuCell state signal =
  case signal of
    EvaluateCell ->
      let activeCell = state ^? _notebook .. N._activeCell
      in maybe empty requestCellContent activeCell
    DeleteCell -> pure $ TrashCell (state ^. _notebook .. N._activeCellId)
    _ -> empty

handleMenuInsert :: forall e. MenuInsertSignal -> I e
handleMenuInsert ExploreInsert = pure $ AddCell (Explore initialExploreRec)
handleMenuInsert MarkdownInsert = pure $ AddCell (Markdown initialMarkdownRec)
handleMenuInsert QueryInsert = pure $ AddCell (Query initialQueryRec)
handleMenuInsert SearchInsert = pure $ AddCell (Search initialSearchRec)


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
  liftEff $ mailOpen "mailto:support@slamdata.com?subject=Bug%20Found"
  empty
handleMenuHelp RequestSupportHelp = do
  liftEff $ mailOpen "mailto:support@slamdata.com?subject=Request%20Help"
  empty


handleMenuSignal :: forall e. State -> MenuSignal -> I e
handleMenuSignal state signal =
  maybe empty id $ (handleMenuNotebook <$> prj signal)
               <|> (handleMenuInsert <$> prj signal)
               <|> (handleMenuCell state <$> prj signal)
               <|> (handleMenuHelp <$> prj signal)

handleNameInput :: forall e. String -> I e
handleNameInput newName =
  pure $ WithState $ _notebook .. N._name %~ (thisOrBoth newName <<< theseRight)

handleSubmitName :: forall e. State -> I e
handleSubmitName state = liftAff (attempt $ saveNotebook (state ^. _notebook)) >>= go
  where
  go :: Either Error N.Notebook -> I e
  go (Left err) = pure $ WithState (_dialog ?~ ErrorDialog (message err))
  go (Right nb) = do
    liftEff $ setLocation $ U.fromJust $ N.notebookURL nb Edit
    pure $ WithState $ _notebook .~ nb
