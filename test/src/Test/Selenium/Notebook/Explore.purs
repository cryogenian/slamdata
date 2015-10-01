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

module Test.Selenium.Notebook.Explore (test) where

import Prelude
import Control.Apply ((*>))
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Random (randomInt)
import Data.Either (Either(..), either)
import Data.Foldable (fold, for_)
import Data.List (List(..), fromList, toList, length, catMaybes, null, (!!), head, reverse)
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), snd, fst)
import Driver.File.Routing (Routes(..), routing)
import Selenium.ActionSequence hiding (sequence)
import Selenium.Combinators (checker, tryToFind)
import Selenium.Monad
import Selenium.MouseButton
import Selenium.Types
import Test.Config
import Test.Selenium.ActionSequence (selectAll, sendDelete, sendEnter)
import Test.Selenium.Common
import Test.Selenium.Expect (expect, toEq)
import Test.Selenium.File hiding (test)
import Test.Selenium.Log
import Test.Selenium.Monad
import Test.Selenium.Notebook.Contexts
import Test.Selenium.Notebook.Getters
import Test.Selenium.Types
import qualified Data.String as S
import qualified Data.String.Regex as R


import qualified Config as SDConfig
import qualified Test.Selenium.Notebook.FileList as FL
import qualified Test.Selenium.Notebook.Common as C


checkInitialExplore :: Check Unit
checkInitialExplore =
  withExploreCell $  C.checkInitial do
    config <- getConfig
    value <- getElementByCss config.explore.input "there is no input"
             >>= flip getAttribute "value"
    expect value toEq $ Just ""

checkEmptyInputErrors :: Check Unit
checkEmptyInputErrors = do
  config <- getConfig
  withExploreCell do
    C.checkIncorrect getRefreshButton
  withExploreCell do
    C.checkIncorrect findPlayButton


checkIncorrectInputs :: Check Unit
checkIncorrectInputs = do
  checkInexistentFileMounted
  checkInexistentFileNotMounted
  checkDirectoryFailure


checkFailure :: String -> String -> Check Unit
checkFailure label keys = withExploreCell do
  config <- getConfig
  input <- getInput
  play <- findPlayButton
  sequence do
    leftClick input
    sendKeys keys
    leftClick play
  tryToFind $ byCss config.cell.failures
  successMsg $ "Ok, failures are shown " <> label
  checkNotExists ("There should not be results " <> label) config.cell.cellOutputResult
  checkNotExists ("There should not be output label " <> label) config.cell.cellOutputLabel
  successMsg $ "Ok, there is no results " <> label

checkDirectoryFailure :: Check Unit
checkDirectoryFailure =
  getConfig >>= _.explore >>> _.directory >>> checkFailure "(directory)"

checkInexistentFileNotMounted :: Check Unit
checkInexistentFileNotMounted =
  getConfig >>= _.explore >>> _.notMounted >>> checkFailure "(not mounted)"

checkInexistentFileMounted :: Check Unit
checkInexistentFileMounted = withExploreCell do
  config <- getConfig
  input <- getInput
  play <- findPlayButton
  sequence do
    leftClick input
    sendKeys config.explore.mounted
    leftClick play
  C.checkTableEmpty
  successMsg "Ok, expected behaviour when file list contains incorrect mounted file"

checkStatus :: Check Unit
checkStatus = withSmallZipsOpened do
  config <- getConfig
  refresh <- getRefreshButton
  wait (checker finished) config.selenium.waitTime
  successMsg "Ok, correct status text"
  sequence do
    leftClick refresh
  wait (checker finished) config.selenium.waitTime
  successMsg "Ok, correct status text"
  where
  finished = do
    v <- getStatus >>= getInnerHtml
    pure $ R.test (R.regex "Finished: took \\d+ms" R.noFlags) v

  waitFn p = do
    statusText <- getStatus >>= getInnerHtml
    pure $ statusText /= p


checkOutputLabel :: Check Unit
checkOutputLabel = do
  config <- getConfig
  withSmallZipsOpened do
    zipsLabel <- getElementByCss config.cell.cellOutputLabel "no output label"
                 >>= getInnerHtml
    check zipsLabel config.explore.smallZipsName
    successMsg "Ok, smallZips label is checked"
  withOlympicsOpened do
    olympicLabel <- getElementByCss config.cell.cellOutputLabel "no output label"
                 >>= getInnerHtml
    check olympicLabel config.explore.olympicsName
    successMsg "Ok, olympics label is checked"
  where
  check content expected =
    let extracted = S.trim $ R.replace (R.regex "^([^:]+).+$" R.noFlags) "$1" content
    in if extracted == expected
       then successMsg "Ok, correct output label"
       else errorMsg $
            "Incorrect output label\n" <>
            "Should be: " <> expected <>
            "\nactual: " <> extracted




checkPageCount :: Check Unit
checkPageCount = do
  config <- getConfig
  withSmallZipsOpened $ go config.explore.smallZipsPageCount
  withOlympicsOpened $ go config.explore.olympicsPageCount
  successMsg "Ok, correct page count"
  where
  go expected = do
    actual <- getPageCount
    if actual == expected
      then pure unit
      else errorMsg "Incorrect page count"


checkRowCount' :: (Int -> Int -> Boolean) -> Int -> Check Unit
checkRowCount' assertFn expected = do
  {table: tableCount, pager: pagerCount} <- getRowCount
  if assertFn tableCount pagerCount
    then successMsg "Ok, correct row count"
    else errorMsg $ "Incorrect row count\n" <>
         "expected: " <> show expected <>
         "\nin table: " <> show tableCount <>
         "\nin pager: " <> show pagerCount


checkRowCount :: Int -> Check Unit
checkRowCount expected =
  checkRowCount' (\tc pc -> tc == expected && pc == expected) expected


checkInitialRowCount :: Check Unit
checkInitialRowCount = do
  config <- getConfig
  withSmallZipsOpened $ checkRowCount config.explore.initialRowCount

checkRowsPerPageSwitching :: Check Unit
checkRowsPerPageSwitching = do
  checkRowsPerPageSelect
  checkRowsPerPageCustom

setPageSizeOption :: String -> Check Unit
setPageSizeOption str = do
  select <- getPageSizeSelect
  option <- getOption str
  sequence do
    leftClick select
    leftClick option
    sendEnter
  where
  getOption str = do
    config <- getConfig
    options <- byCss config.explore.option >>= findElements
    filtered <- filterByContent options (\content -> content == str)
    case filtered of
      Nil -> errorMsg $ "There is no option with value " <> str
      Cons el _ -> pure el

checkRowsPerPageSelect :: Check Unit
checkRowsPerPageSelect = withSmallZipsOpened do
  config <- getConfig
  select <- getPageSizeSelect
  for_ (reverse $ toList config.explore.optionNums) traverseFn
  where
  traverseFn numStr = do
    config <- getConfig
    tableHtml <- getTable >>= getInnerHtml
    setPageSizeOption numStr
    afterTableReload tableHtml
    count <- parseToInt numStr
    checkRowCount count
    successMsg $ "Ok, page size changed to " <> numStr



checkRowsPerPageCustom :: Check Unit
checkRowsPerPageCustom = withSmallZipsOpened do
  config <- getConfig
  setPageSizeOption config.explore.optionCustom
  wait (checker check) config.selenium.waitTime
  input <- getPageSizeInput
  successMsg "Ok, input has been appeared"
  rnd <- getRandom 10

  tableHtml <- getTable >>= getInnerHtml
  modifierKey <- getModifierKey
  sequence do
    leftClick input
    selectAll modifierKey
    sendDelete
    sendKeys (show rnd)
    sendEnter
  afterTableReload tableHtml
  checkRowCount' (\tc _ -> tc == rnd) rnd
  successMsg $ "Ok, random (" <> show rnd <> ") row per page works"

  where
  getRandom n = do
    if n /= 10
      then pure n
      else do
      m <- liftEff $ randomInt 1 99
      getRandom m
  check = do
    attempt getPageSizeInput >>= pure <<< either (const false) (const true)

checkPagination :: Check Unit
checkPagination = withSmallZipsOpened do
  ff <- getFastForward
  sf <- getStepForward
  fb <- getFastBackward
  sb <- getStepBackward
  input <- getPaginationInput
  config <- getConfig

  checkRecord initialER "initial"
  checkRowContent config.explore.firstPageContent "initial"
  initialHtml <- getTable >>= getInnerHtml
  sequence $ leftClick sf
  afterTableReload initialHtml

  checkRecord secondPageER "second page"
  checkRowContent config.explore.secondPageContent "second page"
  secondHtml <- getTable >>= getInnerHtml
  sequence $ leftClick ff
  afterTableReload secondHtml

  checkRecord lastPageER "last page"
  checkRowContent config.explore.lastPageContent "last page"
  lastHtml <- getTable >>= getInnerHtml
  sequence $ leftClick sb
  afterTableReload lastHtml

  checkRecord prenultPageER "prenult page"
  checkRowContent config.explore.prenultPageContent "prenult page"
  prenultHtml <- getTable >>= getInnerHtml
  sequence $ leftClick fb
  afterTableReload prenultHtml

  checkRecord initialER "first page"
  checkRowContent config.explore.firstPageContent "first page"
  firstHtml <- getTable >>= getInnerHtml
  modifierKey <- getModifierKey
  sequence do
    leftClick input
    selectAll modifierKey
    sendKeys config.explore.customPageNumber
    sendEnter
  afterTableReload firstHtml

  let customMsg = "custom page (" <> config.explore.customPageNumber <> ")"
  checkRecord (customPageER config.explore.customPageNumber) $ customMsg
  checkRowContent config.explore.customPageContent customMsg

  successMsg "Ok, pagination is checked, content probe is correct"

  where
  initialER = EnabledRecord {ff: true, sf: true, fb: false, sb: false, value: "1"}
  secondPageER = EnabledRecord {ff: true, sf: true, fb: true, sb: true, value: "2"}
  lastPageER = EnabledRecord {ff: false, sf: false, fb: true, sb: true, value: "10"}
  prenultPageER = EnabledRecord {ff: true, sf: true, fb: true, sb: true, value: "9"}
  customPageER num =
    EnabledRecord {ff: true, sf: true, fb: true, sb: true, value: num}

  checkRowContent sel msg = do
    config <- getConfig
    correctRows <- byCss config.explore.row >>= findElements >>=
                   flip filterByContent (== sel)
    case length correctRows of
      0 -> errorMsg $ "There is no content that should be on first page (" <>
           msg <> ")"
      1 -> successMsg $ "Ok, page content checked (" <> msg <> ")"
      _ -> errorMsg $ "There is row dublicates (" <> msg <> ")"

  checkRecord expected msg = do
    emsg <- errMsg
    await emsg do
      eq expected <$> getEnabledRecord
    successMsg $ "Ok, enabled records are equal (" <> msg <> ")"
    where
    errMsg = do
      actual <- getEnabledRecord
      pure $ "Incorrect pagination buttons are enabled:\n"
        <> "case: " <> msg
        <> "\nexpected: " <> show expected
        <> "\nactual: " <> show actual


checkColumns :: Check Unit
checkColumns = do
  config <- getConfig
  withSmallZipsOpened do
    smallZipsColumns <- getJTableHeadContent
    if smallZipsColumns == config.explore.smallZipsHead
      then successMsg "Ok, small zips columns are correct"
      else errorMsg "small zips columns are incorrect"
  withOlympicsOpened do
    olympicsColumns <- getJTableHeadContent
    if olympicsColumns == config.explore.olympicsHead
      then successMsg "Ok, olympics columns are correct"
      else errorMsg "olympics columns are incorrect"
  withNestedOpened do
    nestedColumns <- getJTableHeadContent
    if nestedColumns == config.explore.nestedHead ||
       nestedColumns == config.explore.nestedHeadInversed
      then successMsg "Ok, nested columns are correct"
      else errorMsg $ "nested columns are incorrect"
           <> "\nexpected: " <> config.explore.nestedHead
           <> "\nactual  : " <> nestedColumns
           <> "\nexpected attribute inversed: " <> config.explore.nestedHeadInversed


-- | Should be run after setUp
test :: Check Unit
test = do
  config <- getConfig

  sectionMsg "make explore cell check"
  C.checkMakeCell getExploreCells makeExploreCell

  sectionMsg "check deleting explore cells"
  C.checkDeleting getExploreCells

  sectionMsg "check show/hide explore editor"
  withExploreCell $ C.checkHideShow config.cell.exploreEditor

  sectionMsg "File list in explore cell checking"
  FL.test withExploreCell

  sectionMsg "check that embed button, next cell menu and result is not visible"
  checkInitialExplore

  sectionMsg "check failures with empty input"
  checkEmptyInputErrors

  sectionMsg "check incorrect inputs"
  checkIncorrectInputs

  sectionMsg "check embed button"
  withSmallZipsOpened $ C.checkEmbedButton

  sectionMsg "check status"
  checkStatus

  sectionMsg "check next cells"
  withSmallZipsOpened $ C.checkNextCells config.cell.nextCellsForExplore


  sectionMsg "check output label"
  checkOutputLabel

  sectionMsg "check page count"
  checkPageCount

  sectionMsg "check inital row count"
  checkInitialRowCount

  sectionMsg "check rows per page switching"
  checkRowsPerPageSwitching


  sectionMsg "check forward/backward/set page"
  checkPagination

  sectionMsg "check columns (most of this checks should be in jtable tests)"
  checkColumns
