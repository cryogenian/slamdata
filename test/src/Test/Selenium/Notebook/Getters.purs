module Test.Selenium.Notebook.Getters where

import Prelude
import Data.Either (Either(..))
import Data.Traversable (traverse)
import Data.List (catMaybes, List(..))
import Data.Maybe (Maybe(..))
import Selenium.Types
import Test.Selenium.Log
import Test.Selenium.Monad
import Test.Selenium.Common
import qualified Data.String.Regex as R

getNewCellMenuTrigger :: Check Element
getNewCellMenuTrigger = do
  config <- getConfig
  getElementByCss config.newCellMenu.expandCollapse "expand collapse button not found"


getCells :: Check (List Element)
getCells = getConfig >>= (_.cell >>> _.main >>> css) >>= elements

getExploreCells :: Check (List Element)
getExploreCells = do
  cells <- getCells
  mbCells <- traverse traverseFn cells
  pure $ catMaybes mbCells 
  where
  traverseFn el = do
    eHtml <- attempt $ innerHtml el
    pure $ case eHtml of
      Left _ -> Nothing
      Right html ->
        if R.test (R.regex """<div class="cell-name">Explore</div>""" R.noFlags ) html
        then Just el
        else Nothing


fileListVisible :: Check Boolean
fileListVisible = 
  getConfig >>=
  (_.explore >>> _.list >>> flip getElementByCss "file list not found") >>= 
  visible

getInput :: Check Element
getInput =
  getConfig >>= _.explore >>> _.input >>>
  flip getElementByCss "there is no input in file list"

getPlayButton :: Check Element
getPlayButton =
  getConfig >>= _.cell >>> _.playButton >>>
  flip getElementByCss "there is no play button"

getRefreshButton :: Check Element
getRefreshButton =
  getConfig >>= _.cell >>> _.refreshButton >>>
  flip getElementByCss "there is no refresh button"

getStatusText :: Check Element
getStatusText =
  getConfig >>= _.cell >>> _.statusText >>>
  flip getElementByCss "there is no status text"

getEmbedButton :: Check Element
getEmbedButton =
  getConfig >>= _.cell >>> _.embedButton >>>
  flip getElementByCss "there is no embed button"

getPageSizeSelect :: Check Element
getPageSizeSelect =
  getConfig >>= _.explore >>> _.pageSizeSelect >>>
  flip getElementByCss "There is no page size select"

getPageSizeInput :: Check Element
getPageSizeInput =
  getConfig >>= _.explore >>> _.pageSizeInput >>>
  flip getElementByCss "There is no page size input"

getTableRows :: Check (List Element)
getTableRows =
  getConfig >>= (_.explore >>> _.row >>> css) >>= elements 

getTable :: Check Element
getTable =
  getConfig >>= _.explore >>> _.table >>>
  flip getElementByCss "There is no result table"

getFastForward :: Check Element
getFastForward = 
  getConfig >>= _.explore >>> _.paginationFastForwardContent >>> getPaginationButton

getStepForward :: Check Element
getStepForward = 
  getConfig >>= _.explore >>> _.paginationStepForwardContent >>> getPaginationButton

getFastBackward :: Check Element
getFastBackward = 
  getConfig >>= _.explore >>> _.paginationFastBackwardContent >>> getPaginationButton

getStepBackward :: Check Element
getStepBackward =
  getConfig >>= _.explore >>> _.paginationStepBackwardContent >>> getPaginationButton

getPaginationButton :: String -> Check Element
getPaginationButton content = do
  config <- getConfig
  btns <- css config.explore.paginationButtons >>= elements
  filtered <- filterByContent btns $ filterFn content
  case filtered of
    Nil -> errorMsg $ "There is no pagination button with content " <> content
    Cons el _ -> pure el
  where
  filterFn :: String -> String -> Boolean
  filterFn content html =
    R.test (R.regex content R.noFlags) html

getPaginationInput :: Check Element
getPaginationInput =
  getConfig >>= _.explore >>> _.pageInput >>>
  flip getElementByCss "there is no pagination input"

getJTableHeadContent :: Check String
getJTableHeadContent =
  getConfig >>= (_.explore >>> _.jtableHead >>>
                 flip getElementByCss "There is no JTable head") >>=
  innerHtml



