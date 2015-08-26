{-
Copyright 2015 SlamData, Inc.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
-}

module Controller.Notebook.Cell
  ( requestCellContent
  , runCell
  , viewCell
  , handleRunClick
  , handleEmbedClick
  , handleRefreshClick
  , isRunning
  ) where

import Prelude
import Api.Fs (saveNotebook)
import Control.Monad.Aff (attempt)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (message)
import Controller.Notebook.Cell.Markdown (runMarkdown)
import Controller.Notebook.Cell.Explore (runExplore, viewExplore)
import Controller.Notebook.Cell.Query (runQuery, viewQuery)
import Controller.Notebook.Cell.Search (runSearch, viewSearch)
import Controller.Notebook.Cell.Viz (runViz)
import Controller.Notebook.Common (I(), update)
import Data.Array (head)
import Data.Either (Either(..))
import Input.Notebook (Input(..))
import Model.Action
import Model.Notebook (State(), _dialog, _notebook, _requesting)
import Model.Notebook.Cell (Cell(), CellContent(..), _cellId, _runState, _content, RunState(..), _hasRun)
import Model.Notebook.Domain (notebookURL, cellURL, Notebook(), ancestors, _dependencies, cellById)
import Model.Notebook.Dialog
import Optic.Core
import Optic.Fold ((^?))
import Utils (locationString, replaceLocation)

import Data.Maybe (maybe)
import qualified Data.Maybe.Unsafe as U

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

handleRefreshClick :: forall e. Cell -> I e
handleRefreshClick cell = pure (RefreshCell cell)

stopCell :: forall e. Notebook -> Cell -> I e
stopCell notebook cell = pure (StopCell (cell ^._cellId))

requestCellContent :: forall eff. Notebook -> Cell -> I eff
requestCellContent notebook cell =
  (pure $ WithState (_requesting <>~ [cell ^._cellId])) <>
  (update cell (_hasRun .~ true)) <>
  (maybe go (pure <<< RequestCellContent) $
   head ps >>= (\x -> notebook ^? cellById x))
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
