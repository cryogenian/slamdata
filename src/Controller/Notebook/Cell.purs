module Controller.Notebook.Cell
  ( requestCellContent
  , runCell
  , viewCell
  , handleRunClick
  , handleEmbedClick
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
import Controller.Notebook.Common (I(), update)
import Data.Date (now)
import Data.Either (Either(..))
import Halogen.HTML.Events.Monad (andThen)
import Input.Notebook (CellResultContent(MarkdownContent), Input(..))
import Model.Action
import Model.Notebook (State(), _dialog, _notebook, _requesting)
import Model.Notebook.Cell (Cell(), CellContent(..), _cellId, _runState, _content, RunState(..), _hasRun)
import Model.Notebook.Domain (notebookURL, cellURL, Notebook(), ancestors, _dependencies, cellById)
import Model.Notebook.Dialog
import Optic.Core ((.~), (^.), (?~), (<>~))
import Optic.Fold ((^?))
import Utils (locationString, replaceLocation)

import Data.Maybe (maybe)
import qualified Data.Maybe.Unsafe as U

runMarkdown :: forall eff. Cell -> I eff
runMarkdown cell = result <$> liftEff now
  where result d = CellResult (cell ^. _cellId) d $ Right MarkdownContent

runCell :: forall eff. Cell -> I eff
runCell cell = do
  case cell ^. _content of
    Search _ -> runSearch cell
    Explore _ -> runExplore cell
    Visualize _ -> runViz cell
    Markdown _ -> runMarkdown cell
    Query _ -> runQuery cell

viewCell :: forall eff. Cell -> I eff
viewCell cell = do
  case cell ^._content of
    Search _ -> viewSearch cell
    Explore _ -> viewExplore cell
    Visualize _ -> runViz cell
    Markdown _ -> runMarkdown cell
    Query _ -> viewQuery cell


handleRunClick :: forall e. Notebook -> Cell -> I e
handleRunClick notebook cell | isRunning cell = stopCell notebook cell
                             | otherwise = requestCellContent notebook cell

stopCell :: forall e. Notebook -> Cell -> I e
stopCell notebook cell =
  pure (StopCell (cell ^._cellId))

requestCellContent :: forall eff. Notebook -> Cell -> I eff
requestCellContent notebook cell =
  (pure $ WithState (_requesting <>~ [cell ^._cellId])) <>
  (update cell (_hasRun .~ true)) <>
  (case ps of
      [] -> go
      x:_ -> maybe go (pure <<< RequestCellContent) (notebook ^? cellById  x))
  where
  ps = ancestors (cell ^._cellId) (notebook ^._dependencies)
  go = pure $ RequestCellContent cell

handleEmbedClick :: forall e. State -> Cell -> I e
handleEmbedClick state cell = do
  r <- liftAff $ attempt $ saveNotebook (state ^. _notebook)
  case r of
    Left err -> pure $ WithState $ _dialog ?~ ErrorDialog ("Could not save notebook for sharing: " ++ message err)
    Right nb -> do
      loc <- liftEff $ do
        replaceLocation $ U.fromJust $ notebookURL nb Edit
        locationString
      let url = U.fromJust $ cellURL nb (cell ^. _cellId) View
      pure $ WithState $ _dialog ?~ EmbedDialog (loc ++ "/" ++ url)

isRunning :: Cell -> Boolean
isRunning cell = case cell ^. _runState of
  RunningSince _ -> true
  _ -> false
