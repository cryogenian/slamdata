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
import Data.Foldable (fold)
import Data.List (catMaybes, List(..), fromList, toList, length, (!!))
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid.Conj (Conj(..), runConj)
import Data.Monoid.Disj (Disj(..), runDisj)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Data.String (joinWith)
import Selenium.Monad
import Selenium.Types
import Test.Config
import Test.Selenium.Common
import Test.Selenium.Log
import Test.Selenium.Monad (Check(), getConfig)
import Test.Selenium.Types
import Test.Selenium.Finders (findSingle)

import Data.String.Regex as R

waitNextCellSearch :: Check Element
waitNextCellSearch = do
  config <- getConfig
  tryRepeatedlyTo $ byCss config.cell.nextCellSearch >>= findExact

waitNextVizCell :: Check Element
waitNextVizCell = do
  config <- getConfig
  tryRepeatedlyTo $ byCss config.cell.nextCellViz >>= findExact

waitNextQueryCellFor :: Element -> Check Element
waitNextQueryCellFor cell = tryRepeatedlyTo do
  config <- getConfig
  byCss config.cell.nextCellQuery >>= childExact cell

waitNextVizCellFor :: Element -> Check Element
waitNextVizCellFor cell = tryRepeatedlyTo do
  config <- getConfig
  byCss config.cell.nextCellViz >>= childExact cell

waitCanvas :: Check Element
waitCanvas = do
  config <- getConfig
  tryRepeatedlyTo $ byCss config.vizSelectors.canvas >>= findExact

fileListVisible :: Check Boolean
fileListVisible =
  getConfig
    >>= (_.explore >>> _.list >>> flip getElementByCss "file list not found")
    >>= isDisplayed

waitVizHeightInput :: Check Element
waitVizHeightInput = do
  config <- getConfig
  tryRepeatedlyTo $ byCss config.vizSelectors.heightInput >>= findExact

waitVizWidthInput :: Check Element
waitVizWidthInput = do
  config <- getConfig
  tryRepeatedlyTo $ byCss config.vizSelectors.widthInput >>= findExact

getInputLocator :: Check Locator
getInputLocator = do
  config <- getConfig
  tryRepeatedlyTo $ byCss config.explore.input

getInput :: Check Element
getInput = getInputLocator >>= findSingle

getAceInput :: Check Element
getAceInput = do
  config <- getConfig
  tryRepeatedlyTo $ byCss config.ace.textInput >>= findExact

getAceFor :: Element -> Check Element
getAceFor cell =
  tryRepeatedlyTo
  $ getConfig
  >>= _.ace >>> _.textInput >>> byCss
  >>= childExact cell

findPlayButton :: Check Element
findPlayButton = tryRepeatedlyTo $ byCss "button[aria-label=\"Play\"]" >>= findExact

getPlayButtonFor :: Element -> Check Element
getPlayButtonFor cell =
  tryRepeatedlyTo
  $ getConfig
  >>= _.cell >>> _.playButton >>> byCss
  >>= childExact cell

getRefreshButton :: Check Element
getRefreshButton =
  getConfig >>= _.cell >>> _.refreshButton >>>
  flip getElementByCss "there is no refresh button"

getStatus :: Check Element
getStatus =
  getConfig >>= _.cell >>> _.status >>>
  flip getElementByCss "there is no status text"

getEmbedButton :: Check Element
getEmbedButton =
  getConfig >>= _.cell >>> _.embedButton >>>
  flip getElementByCss "there is no embed button"

pageSizeSelectLocator :: Check Locator
pageSizeSelectLocator = getConfig >>= _.explore >>> _.pageSizeSelect >>> byCss

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

getPaginationInputLocator :: Check Locator
getPaginationInputLocator =
  getConfig >>= _.explore >>> _.pageInput >>> byCss

getPaginationInput :: Check Element
getPaginationInput = getPaginationInputLocator >>= findSingle

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
  tryRepeatedlyTo $ byCss config.cell.cellOutputLabel >>= findExact

getPager :: Check Element
getPager = getConfig >>= _.explore >>> _.pager >>>
           flip getElementByCss "There is no pager"

getPageSizeSelect :: Check Element
getPageSizeSelect = pageSizeSelectLocator >>= findSingle

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
  loc <- pageSizeSelectLocator
  pc <- findSingle loc >>= flip getAttribute attr
                       >>= maybe (attrFail loc attr) parseToInt
  pure {table: tc, pager: pc}
    where
    attr = "value"

getEnabledRecord :: Check EnabledRecord
getEnabledRecord = do
  ff <- getFastForward
  sf <- getStepForward
  fb <- getFastBackward
  sb <- getStepBackward
  inputLocator <- getPaginationInputLocator
  input <- findSingle inputLocator
  r <- { ff: _
       , sf: _
       , fb: _
       , sb: _
       , value: _}
       <$> isEnabled ff
       <*> isEnabled sf
       <*> isEnabled fb
       <*> isEnabled sb
       <*> (getAttribute input attr >>= maybe (attrFail inputLocator attr) pure)
  pure $ EnabledRecord r
    where
    attr = "value"

-- VIZ
getPieEditor :: Check Element
getPieEditor = do
  config <- getConfig
  tryRepeatedlyTo $ byCss config.vizSelectors.pieEditor >>= findExact

getLineEditor :: Check Element
getLineEditor = do
  config <- getConfig
  tryRepeatedlyTo $ byCss config.vizSelectors.lineEditor >>= findExact

getBarEditor :: Check Element
getBarEditor = do
  config <- getConfig
  tryRepeatedlyTo $ byCss config.vizSelectors.barEditor >>= findExact

getCurrentEditor :: Check Element
getCurrentEditor = do
  pieDisplayed <- getPieEditor >>= isDisplayed
  barDisplayed <- getBarEditor >>= isDisplayed
  lineDisplayed <- getLineEditor >>= isDisplayed
  if pieDisplayed
    then getPieEditor
    else if barDisplayed
         then getBarEditor
         else if lineDisplayed
              then getLineEditor
              else errorMsg "Error: Incorrect state of viz editor"

barShown :: Check Boolean
barShown = getBarEditor >>= isDisplayed

lineShown :: Check Boolean
lineShown = getLineEditor >>= isDisplayed

pieShown :: Check Boolean
pieShown = getPieEditor >>= isDisplayed

getPieTypeIcon :: Check Element
getPieTypeIcon = do
  config <- getConfig
  tryRepeatedlyTo $ byCss config.vizSelectors.pieIcon >>= findExact

getLineTypeIcon :: Check Element
getLineTypeIcon = do
  config <- getConfig
  tryRepeatedlyTo $ byCss config.vizSelectors.lineIcon >>= findExact

getBarTypeIcon :: Check Element
getBarTypeIcon = do
  config <- getConfig
  tryRepeatedlyTo $ byCss config.vizSelectors.barIcon >>= findExact


getOptions :: Maybe Element -> Check (List Element)
getOptions el = maybe (pure Nil) (\p -> byCss "option" >>= findChildren p) $ el


getChartSwitchers :: Check ChartSwitchers
getChartSwitchers = do
  {bar: _, line: _, pie: _}
  <$> getBarTypeIcon
  <*> getLineTypeIcon
  <*> getPieTypeIcon


getChartOptions :: Element -> Check ChartOptions
getChartOptions el = do
  config <- getConfig
  p <- { measureOne: _
       , measureTwo: _
       , category: _
       , dimension: _
       , seriesOne: _
       , seriesTwo: _
       }
    <$> optionTxts config.vizSelectors.measureOne
    <*> optionTxts config.vizSelectors.measureTwo
    <*> optionTxts config.vizSelectors.category
    <*> optionTxts config.vizSelectors.dimension
    <*> optionTxts config.vizSelectors.seriesOne
    <*> optionTxts config.vizSelectors.seriesTwo
  pure $ ChartOptions p
  where
  optionTxts sel =
    map fromList
    $ byCss sel
    >>= findChild el
    >>= getOptions
    >>= traverse getInnerHtml


getCurrentChartOptions :: Check ChartOptions
getCurrentChartOptions = tryRepeatedlyTo $
  getCurrentEditor >>= getChartOptions


getCurrentEditorChild :: String -> Check Element
getCurrentEditorChild sel = tryRepeatedlyTo do
  edit <- getCurrentEditor
  byCss sel >>= childExact edit


getCategoryInput :: Check Element
getCategoryInput =
  getConfig >>= _.vizSelectors >>> _.category
  >>> getCurrentEditorChild

getMeasureOneInput :: Check Element
getMeasureOneInput =
  getConfig >>= _.vizSelectors >>> _.measureOne
  >>> getCurrentEditorChild

getMeasureTwoInput :: Check Element
getMeasureTwoInput =
  getConfig >>= _.vizSelectors >>> _.measureTwo
  >>> getCurrentEditorChild

getDimensionInput :: Check Element
getDimensionInput =
  getConfig >>= _.vizSelectors >>> _.dimension
  >>> getCurrentEditorChild

getSeriesOneInput :: Check Element
getSeriesOneInput =
  getConfig >>= _.vizSelectors >>> _.seriesOne
  >>> getCurrentEditorChild

getSeriesTwoInput :: Check Element
getSeriesTwoInput =
  getConfig >>= _.vizSelectors >>> _.seriesTwo
  >>> getCurrentEditorChild


getAggregationSelect :: Check Element
getAggregationSelect =
  getConfig >>= _.vizSelectors >>> _.aggregation
  >>> getCurrentEditorChild

getAggregationOption :: Element -> String -> Check Element
getAggregationOption select value =
  byCss ("option[value=\"" <> value <> "\"]")
  >>= childExact select
