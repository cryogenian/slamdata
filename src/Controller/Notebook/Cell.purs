module Controller.Notebook.Cell
  ( requestCellContent
  , runCell
  , viewCell
  , handleRunClick
  , handleShareClick
  , isRunning
  ) where

import Api.Fs (saveNotebook)
import Control.Monad.Aff (attempt)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (message)
import Control.Plus (empty)
import Controller.Notebook.Cell.Explore (runExplore, viewExplore)
import Controller.Notebook.Cell.Query (runQuery, viewQuery)
import Controller.Notebook.Cell.Search (runSearch, viewSearch)
import Controller.Notebook.Cell.Viz (runViz)
import Controller.Notebook.Common (I())
import Data.Date (now)
import Data.Either (Either(..))
import Halogen.HTML.Events.Monad (andThen)
import Input.Notebook (CellResultContent(MarkdownContent), Input(..))
import Model.Action
import Model.Notebook (State(), _dialog, _notebook)
import Model.Notebook.Cell (Cell(), CellContent(..), _cellId, _runState, _content, RunState(..))
import Model.Notebook.Domain (notebookURL, cellURL)
import Model.Notebook.Dialog
import Optic.Core ((.~), (^.), (?~))
import Utils (locationString, replaceLocation)

import qualified Data.Maybe.Unsafe as U

runMarkdown :: forall eff. Cell -> I eff
runMarkdown cell = result <$> liftEff now
  where result d = CellResult (cell ^. _cellId) d $ Right MarkdownContent

runCell :: forall eff. Cell -> I eff
runCell cell = do
  d <- liftEff now
  case cell ^. _content of
    Search _ -> runSearch (cell # _runState .~ (RunningSince d))
    Explore _ -> runExplore cell
    Visualize _ -> runViz cell
    Markdown _ -> runMarkdown cell
    Query _ -> runQuery cell


viewCell :: forall eff. Cell -> I eff
viewCell cell = do
  d <- liftEff now
  case cell ^._content of
    Search _ -> viewSearch (cell # _runState .~ (RunningSince d))
    Explore _ -> viewExplore cell
    Visualize _ -> runViz cell
    Markdown _ -> runMarkdown cell
    Query _ -> viewQuery cell

requestCellContent :: forall eff. Cell -> I eff
requestCellContent cell = pure $ RequestCellContent cell

handleRunClick :: forall e. Cell -> I e
handleRunClick cell | isRunning cell = pure (StopCell $ cell ^. _cellId)
                    | otherwise = requestCellContent cell

handleShareClick :: forall e. State -> Cell -> I e
handleShareClick state cell = do
  r <- liftAff $ attempt $ saveNotebook (state ^. _notebook)
  case r of
    Left err -> pure $ WithState $ _dialog ?~ ErrorDialog ("Could not save notebook for sharing: " ++ message err)
    Right nb -> do
      loc <- liftEff $ do
        replaceLocation $ U.fromJust $ notebookURL nb Edit
        locationString
      let url = U.fromJust $ cellURL nb (cell ^. _cellId) View
      pure $ WithState $ _dialog ?~ ShareDialog (loc ++ "/" ++ url)

isRunning :: Cell -> Boolean
isRunning cell = case cell ^. _runState of
  RunningSince _ -> true
  _ -> false
