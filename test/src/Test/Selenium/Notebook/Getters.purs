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

module Test.Selenium.Notebook.Getters where

import Prelude
import Data.Either (Either(..))
import Data.Traversable (traverse)
import Data.List (catMaybes, List(..), fromList, toList, length, (!!))
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (Tuple(..))
import Data.Foldable (fold)
import Data.Monoid.Disj (Disj(..), runDisj)
import Data.Monoid.Conj (Conj(..), runConj)
import Selenium.Types
import Test.Selenium.Log
import Test.Selenium.Monad
import Test.Selenium.Common
import Test.Selenium.Types 
import qualified Data.String.Regex as R
import Selenium.Monad


newCellMenuExpanded :: Check Boolean
newCellMenuExpanded = do
  config <- getConfig
  btns <- traverse getEl $ toList [ Tuple config.newCellMenu.queryButton "query" 
                                  , Tuple config.newCellMenu.mdButton "markdown" 
                                  , Tuple config.newCellMenu.exploreButton "explore"
                                  , Tuple config.newCellMenu.searchButton "search"
                                  ]
  viss <- traverse isDisplayed btns
  let orVis = (runDisj <<< fold <<< (Disj <$>)) viss
  if not orVis
    then pure false
    else
    let andVis = (runConj <<< fold <<< (Conj <$>)) viss
    in if andVis
       then pure true
       else errorMsg "Some of new cell buttons is isDisplayed and some is not"
  where
  getEl :: Tuple String String -> Check Element
  getEl (Tuple selector msg) =
    getElementByCss selector $ msg <> " not found in new cell menu"
    


getNewCellMenuTrigger :: Check Element
getNewCellMenuTrigger = do
  config <- getConfig
  getElementByCss config.newCellMenu.expandCollapse
    "expand collapse button not found"


getCells :: Check (List Element)
getCells = getConfig >>= (_.cell >>> _.main >>> byCss) >>= findElements

getCell :: Int -> Check Element
getCell n = do
  cells <- getCells
  maybe (errorMsg $ "there is no cell#" <> show n) pure $ cells !! n

getCellsWithContent :: String -> Check (List Element)
getCellsWithContent content = do
  cells <- getCells
  mbCells <- traverse traverseFn cells
  pure $ catMaybes mbCells
  where
  traverseFn el = do
    eHtml <- attempt $ getInnerHtml el
    pure $ case eHtml of
      Left _ -> Nothing
      Right html ->
        if R.test (R.regex content R.noFlags) html
        then Just el
        else Nothing

getExploreCells :: Check (List Element)
getExploreCells =
  getConfig >>= _.cell >>> _.exploreFlag >>> getCellsWithContent

getSearchCells :: Check (List Element)
getSearchCells =
  getConfig >>= _.cell >>> _.searchFlag >>> getCellsWithContent

getMdCells :: Check (List Element)
getMdCells =
  getConfig >>= _.cell >>> _.mdFlag >>> getCellsWithContent


waitNextCellSearch :: Check Element
waitNextCellSearch = do
  config <- getConfig
  waitExistentCss config.cell.nextCellSearch "There is no next search cell button"

waitNextVizCell :: Check Element
waitNextVizCell = do
  config <- getConfig
  waitExistentCss config.cell.nextCellViz "There is no next viz cell button"

waitCanvas :: Check Element
waitCanvas = do
  getConfig >>= _.viz >>> _.canvas >>>
  flip waitExistentCss "There is no canvas" 

fileListVisible :: Check Boolean
fileListVisible = 
  getConfig
    >>= (_.explore >>> _.list >>> flip getElementByCss "file list not found")
    >>= isDisplayed

waitVizHeightInput :: Check Element
waitVizHeightInput =
  getConfig >>= _.viz >>> _.heightInput >>>
  flip waitExistentCss "There is no viz height input"

waitVizWidthInput :: Check Element
waitVizWidthInput =
  getConfig >>= _.viz >>> _.widthInput >>>
  flip waitExistentCss "There is no viz width input"

getInput :: Check Element
getInput =
  getConfig >>= _.explore >>> _.input >>>
  flip getElementByCss "there is no input in file list"

getAceInput :: Check Element
getAceInput =
  getConfig >>= _.ace >>> _.textInput >>>
  flip getElementByCss "there is no ace input"

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
  getConfig >>= (_.explore >>> _.row >>> byCss) >>= findElements 

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
  btns <- byCss config.explore.paginationButtons >>= findElements
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
  getInnerHtml


getSearchInput :: Check Element
getSearchInput =
  getConfig >>= _.searchCell >>> _.searchInput >>>
  flip getElementByCss "There is no search input"

getSearchFileList :: Check Element
getSearchFileList =
  getConfig >>= _.searchCell >>> _.fileListInput >>>
  flip getElementByCss "There is no file list in search cell"

getSearchButton :: Check Element
getSearchButton =
  getConfig >>= _.searchCell >>> _.searchButton >>>
  flip getElementByCss "There is no button to submit search"

getSearchClear :: Check Element
getSearchClear =
  getConfig >>= _.searchCell >>> _.searchClear >>>
  flip getElementByCss "There is no search clear button"

waitOutputLabel :: Check Element
waitOutputLabel = do
  config <- getConfig
  waitExistentCss config.cell.cellOutputLabel "There is no output label"

getPager :: Check Element
getPager = getConfig >>= _.explore >>> _.pager >>>
           flip getElementByCss "There is no pager"

getPageCount :: Check Int 
getPageCount = do
  getPager >>= getInnerHtml >>= extract
  where
  extract html =
    let countStr = R.replace (R.regex "\\D+(\\d+)" R.noFlags) "$1" html
    in parseToInt countStr
  
getRowCount :: Check RowCount 
getRowCount = do
  tc <- length <$> getTableRows
  pc <- getPageSizeSelect >>= flip getAttribute "value" >>= parseToInt
  pure {table: tc, pager: pc}

import Utils.Log

getEnabledRecord :: Check EnabledRecord 
getEnabledRecord = do
    ff <- getFastForward
    sf <- getStepForward
    fb <- getFastBackward
    sb <- getStepBackward
    input <- getPaginationInput
    r <- { ff: _
         , sf: _
         , fb: _
         , sb: _
         , value: _}
         <$> isEnabled ff
         <*> isEnabled sf
         <*> isEnabled fb
         <*> isEnabled sb
         <*> getAttribute input "value"
    pure $ EnabledRecord r
