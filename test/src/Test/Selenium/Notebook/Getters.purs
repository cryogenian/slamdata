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
import Data.Maybe (Maybe(..), maybe, isNothing, isJust)
import Data.Maybe.Unsafe (fromJust)
import Data.Tuple (Tuple(..))
import Data.Foldable (fold)
import Data.Monoid.Disj (Disj(..), runDisj)
import Data.Monoid.Conj (Conj(..), runConj)
import Selenium.Types
import Test.Selenium.Log
import Test.Selenium.Monad
import Test.Selenium.Common
import Test.Selenium.Types
import Test.Config
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
    waitExistentCss selector $ msg <> " not found in new cell menu"



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

waitNextQueryCellFor :: Element -> Check Element
waitNextQueryCellFor cell = waiter do
  config <- getConfig
  byCss config.cell.nextCellQuery >>= childExact cell

waitNextVizCellFor :: Element -> Check Element
waitNextVizCellFor cell = waiter do
  config <- getConfig
  byCss config.cell.nextCellViz >>= childExact cell

waitCanvas :: Check Element
waitCanvas = do
  getConfig >>= _.vizSelectors >>> _.canvas >>>
  flip waitExistentCss "There is no canvas"

fileListVisible :: Check Boolean
fileListVisible =
  getConfig
    >>= (_.explore >>> _.list >>> flip getElementByCss "file list not found")
    >>= isDisplayed

waitVizHeightInput :: Check Element
waitVizHeightInput =
  getConfig >>= _.vizSelectors >>> _.heightInput >>>
  flip waitExistentCss "There is no viz height input"

waitVizWidthInput :: Check Element
waitVizWidthInput =
  getConfig >>= _.vizSelectors >>> _.widthInput >>>
  flip waitExistentCss "There is no viz width input"

getInput :: Check Element
getInput =
  getConfig >>= _.explore >>> _.input >>>
  flip getElementByCss "there is no input in file list"

waitTextField :: String -> Check Element
waitTextField name =
  waitExistentCss selector errorMessage
    where
    errorMessage = "There is no text field named \"" ++ name ++ "\""
    selector = "[type='text'][name='" ++ name ++ "']"

getAceInput :: Check Element
getAceInput =
  getConfig >>= _.ace >>> _.textInput >>>
  flip getElementByCss "there is no ace input"

getAceFor :: Element -> Check Element
getAceFor cell =
  waiter
  $ getConfig
  >>= _.ace >>> _.textInput >>> byCss
  >>= childExact cell

getPlayButton :: Check Element
getPlayButton =
  getConfig >>= _.cell >>> _.playButton >>>
  flip getElementByCss "there is no play button"

getPlayButtonFor :: Element -> Check Element
getPlayButtonFor cell =
  waiter
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


-- VIZ
getPieEditor :: Check Element
getPieEditor = do
  config <- getConfig
  waitExistentCss config.vizSelectors.pieEditor "There is no pie editor"

getLineEditor :: Check Element
getLineEditor = do
  config <- getConfig
  waitExistentCss config.vizSelectors.lineEditor "There is no line editor"

getBarEditor :: Check Element
getBarEditor = do
  config <- getConfig
  waitExistentCss  config.vizSelectors.barEditor "There is no bar editor"

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
  getConfig >>= _.vizSelectors >>> _.pieIcon
    >>> flip waitExistentCss "There is no pie type switcher"

getLineTypeIcon :: Check Element
getLineTypeIcon = do
  getConfig >>= _.vizSelectors >>> _.lineIcon
    >>> flip waitExistentCss "There is no line type switcher"

getBarTypeIcon :: Check Element
getBarTypeIcon = do
  getConfig >>= _.vizSelectors >>> _.barIcon
    >>> flip waitExistentCss "There is no bar type switcher"



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
getCurrentChartOptions = waiter $
  getCurrentEditor >>= getChartOptions


getCurrentEditorChild :: String -> Check Element
getCurrentEditorChild sel = waiter do
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
