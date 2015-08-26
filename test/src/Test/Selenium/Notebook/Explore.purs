module Test.Selenium.Notebook.Explore (test) where

import Prelude
import Control.Apply ((*>))
import Control.Monad.Eff.Random (randomInt)
import Control.Monad.Eff.Class (liftEff)
import Data.Either (Either(..), either, isRight)
import Data.List (List(..), fromList, toList, length, catMaybes, replicateM, null, (!!), head, reverse)
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.Tuple (Tuple(..), snd, fst)
import Data.Foldable (fold, for_)
import Data.Traversable (traverse)
import Test.Config
import Selenium.ActionSequence
import Selenium.MouseButton
import Selenium.Types 
import Test.Selenium.Common
import Test.Selenium.Monad
import Test.Selenium.Log
import Test.Selenium.Notebook.Getters
import Test.Selenium.Notebook.Contexts
import Driver.File.Routing (Routes(..), routing)
import Test.Selenium.File hiding (test)
import qualified Data.String.Regex as R
import qualified Data.String as S
import Halogen.HTML.Renderer.String (renderHTMLToString)
import qualified Halogen.HTML as H
import qualified Halogen.HTML.Attributes as A
import Utils.Halide (width', height', frameBorder)
import Utils.Log
import qualified Config as SDConfig
import qualified Test.Selenium.Notebook.FileList as FL
import qualified Test.Selenium.Notebook.Common as C 

checkNewCellMenu :: Check Unit
checkNewCellMenu = do
  expand <- getNewCellMenuTrigger
  html <- innerHtml expand
  vis <- newCellMenuExpanded
  if vis
    then errorMsg "At least one of new cell menu button is visible"
    else pure unit 
  successMsg "Ok, initial new cell menu is collapsed"
  actions $ leftClick expand
  await "Expand/collapse button has not been changed" do 
    newHtml <- innerHtml expand
    pure $ newHtml /= html
  newVis <- newCellMenuExpanded
  if not newVis
    then errorMsg "At least one of new cell menu is not visible after expanding"
    else pure unit 
  successMsg "Ok, expanded"
  -- checking collapse
  actions $ leftClick expand
  await "Expand/collapse butotn has not returned to default state" do
    collapsedHtml <- innerHtml expand
    pure $ collapsedHtml == html
  collapsedVis <- newCellMenuExpanded
  if collapsedVis
    then errorMsg "At least one of new cell menu button is visible after collapse"
    else successMsg "Ok, collapsed"
      


checkMakeExploreCell :: Check Unit
checkMakeExploreCell = do
  count <- length <$> getExploreCells
  if count /= 0
    then errorMsg "Notebook already has explore cells"
    else pure unit
  toMake <- liftEff $ randomInt 1 20
  replicateM toMake makeExploreCell
  await "Not all explore cells was created" do
    newCount <- length <$> getExploreCells
    pure $ newCount == toMake
  successMsg "Ok, all explore cells have been created"
  -- We need to be sure that autosave is triggered
  waitTime (SDConfig.autosaveTick * 2)
  reloadAndSpyXHR
  await "Explore cells have not been saved" do
    reloadCount <- length <$> getExploreCells
    pure $ reloadCount == toMake
  successMsg "Ok, explore cells have been saved"

checkHideShow :: Check Unit
checkHideShow = withExploreCell do
  config <- getConfig
  hide <- css config.cell.hide >>= element
  show <- css config.cell.show >>= element
  case Tuple hide show of
    Tuple Nothing _ -> errorMsg "Incorrect hide/show state"
    Tuple _ (Just _) -> errorMsg "Incorrect hide/show state"
    Tuple (Just hider) _ -> do
      editor <- getElementByCss config.cell.exploreEditor "cell editor not found"
      actions $ leftClick hider
      mbEditor <- css config.cell.exploreEditor >>= element
      case mbEditor of
        Just _ -> errorMsg "hide editor doesn't work"
        Nothing -> do
          newHide <- css config.cell.hide >>= element
          newShow <- css config.cell.show >>= element
          case Tuple newHide newShow of
            Tuple (Just _) _ -> errorMsg "Incorrect hide/show state after hiding"
            Tuple _ Nothing -> errorMsg "Incorrect hide/show state after hiding"
            Tuple _ (Just shower) -> do
              actions $ leftClick shower
              getElementByCss config.cell.exploreEditor "cell editor not found"
              successMsg "Ok, hide/show button works"



checkInitialExplore :: Check Unit
checkInitialExplore = withExploreCell do
  config <- getConfig
  checkNotExists "Embed button is shown" config.cell.embedButton
  successMsg "Ok, there is no embed button"
  checkNotExists "Next cell menu is show" config.cell.nextCellList
  successMsg "Ok, there is no next cell menu"
  checkNotExists "Cell ouput label is shown" config.cell.cellOutputLabel
  successMsg "Ok, there is no output label"
  checkNotExists "Cell output result is shown" config.cell.cellOutputResult
  successMsg "Ok, there is no output result"
  checkNotExists "Failure messages button is shown" config.cell.showMessages
  successMsg "Ok, there is no show messages button"
  checkNotExists "Failures is shown" config.cell.failures
  successMsg "Ok, there is no failures"
  getElementByCss config.cell.evalLine "There is no eval line, but should"
  successMsg "Ok, there is eval line"
  status <- getStatusText
  successMsg "Ok, status text exists"
  html <- innerHtml status
  if html /= ""
    then errorMsg "Status text should be empty"
    else successMsg "Ok, status text is empty"
  value <- getElementByCss config.explore.input "there is no input"
           >>= flip attribute "value"
  if value /= ""
    then errorMsg "value of input should be empty"
    else successMsg "Ok, input value is empty"
  getPlayButton
  successMsg "Ok, there is play button"
  getRefreshButton
  successMsg "Ok, there is refresh button"
  checkNotExists "Hide failures button should not exist" config.cell.hideMessages
  successMsg "Ok, there is no hide failures button"

checkEmptyInputBtn :: Element -> Check Unit
checkEmptyInputBtn btn = do
  config <- getConfig
  actions $ leftClick btn
  failures <- waitExistentCss config.cell.failures "There is no failures but should"
  html <- innerHtml failures 
  show <- getElementByCss config.cell.showMessages "There is no showMessages but should"
  actions $ leftClick show
  await "There is no difference between hidden and shown failures" do
    shownHtml <- innerHtml failures
    pure $ shownHtml /= html
  successMsg "Ok, shown failures is defferent with hidden"
  hide <- waitExistentCss config.cell.hideMessages "There is no hideMessages"
  actions $ leftClick hide
  await "Hidden failures are not equal with initial" do 
    hiddenHtml <- innerHtml failures
    pure $ hiddenHtml == html
  successMsg "Ok, hidden failures is equal with initial"

checkEmptyInputErrors :: Check Unit
checkEmptyInputErrors = do
  config <- getConfig
  withExploreCell do 
    refresh <- getRefreshButton
    checkEmptyInputBtn refresh
  withExploreCell do 
    play <- getPlayButton
    checkEmptyInputBtn play


checkIncorrectInputs :: Check Unit
checkIncorrectInputs = do
  checkInexistentFileMounted
  checkInexistentFileNotMounted
  checkDirectoryFailure


checkFailure :: String -> String -> Check Unit
checkFailure label keys = withExploreCell do
  config <- getConfig
  input <- getInput
  play <- getPlayButton
  actions do
    leftClick input
    sendKeys keys
    leftClick play
  waitExistentCss config.cell.failures $ "There is no failures but should " <> label
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
  play <- getPlayButton
  actions do
    leftClick input
    sendKeys config.explore.mounted
    leftClick play
  waitNotExistentCss "There should not be failures" config.cell.failures

  waitExistentCss config.cell.cellOutputLabel "There is no output label"
  res <- waitExistentCss config.cell.cellOutputResult "There is no output result"
  mbTbl <- css "table" >>= child res
  table <- maybe (errorMsg "There is no table in result") pure mbTbl
  tableHtml <- innerHtml table
  if tableHtml == "<thead></thead><tbody></tbody>"
    then successMsg "Ok, table is empty"
    else errorMsg "Table should be empty"

checkEmbedButton :: Check Unit
checkEmbedButton = withSmallZipsOpened do
  config <- getConfig
  embed <- getEmbedButton
  actions $ leftClick embed
  waitCheck (checker $ isRight <$> attempt getModal) config.selenium.waitTime
  modal <- getElementByCss config.modal "Modal should be visible"
  box <- getElementByCss config.cell.embedBox "Embed box hidden"
  value <- attribute box "value"
  expected <- expectedValue
  if expected == value
    then successMsg "Ok, embedding cell value is correct"
    else do
    errorMsg $ "Embed value is not correct"
      <> "\nexpected: " <> expected
      <> "\nactual  : " <> value
  reloadAndSpyXHR
  where
  getModal = do
    config <- getConfig 
    getElementByCss config.cell.embedBox "Embed box hidden"
  expectedValue = do
    config <- getConfig 
    pure $ renderHTMLToString $
      H.iframe [ A.src $ url config
               , width' "100%"
               , height' "100%"
               , frameBorder 0
               ] [ ]
  url config =
    config.slamdataUrl <> config.notebookUrl <> "#/" <> config.mount.name <> "/" <>
    config.database.name <> config.explore.notebookPath


checkStatus :: Check Unit
checkStatus = withSmallZipsOpened do
  config <- getConfig 
  refresh <- getRefreshButton
  waitCheck (checker finished) config.selenium.waitTime
  successMsg "Ok, correct status text"
  actions do
    leftClick refresh
  waitCheck (checker finished) config.selenium.waitTime
  successMsg "Ok, correct status text"
  where
  finished = do
    v <- getStatusText >>= innerHtml
    pure $ R.test (R.regex "Finished: took \\d+ms" R.noFlags) v

  waitFn p = do
    statusText <- getStatusText >>= innerHtml
    pure $ statusText /= p


checkNextCells :: Check Unit
checkNextCells = withSmallZipsOpened do
  config <- getConfig
  count <- length <$> (css config.cell.nextCellButton >>= elements)
  if count /= 3
    then errorMsg "Incorrect count of next cell button"
    else successMsg "Ok, correct count of next cell button"
  getElementByCss config.cell.nextCellQuery "query output button not found"
  getElementByCss config.cell.nextCellSearch "search output button not found"
  getElementByCss config.cell.nextCellViz "visualize output button not found"
  successMsg "Ok, all next cell button found"


checkOutputLabel :: Check Unit
checkOutputLabel = do
  config <- getConfig 
  withSmallZipsOpened do
    zipsLabel <- getElementByCss config.cell.cellOutputLabel "no output label"
                 >>= innerHtml
    check zipsLabel config.explore.smallZipsName
    successMsg "Ok, smallZips label is checked"
  withOlympicsOpened do
    olympicLabel <- getElementByCss config.cell.cellOutputLabel "no output label"
                 >>= innerHtml
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
  getPager = getConfig >>= _.explore >>> _.pager >>>
             flip getElementByCss "There is no pager"
  go expected = do
    actual <- getPager >>= innerHtml >>= extract
    if actual == expected
      then pure unit
      else errorMsg "Incorrect page count"
  extract html =
    let countStr = R.replace (R.regex "\\D+(\\d+)" R.noFlags) "$1" html
    in parseToInt countStr

checkRowCount' :: (Int -> Int -> Boolean) -> Int -> Check Unit
checkRowCount' assertFn expected = do
  tableCount <- length <$> getTableRows
  ePagerCount <- attempt getPageSizeSelect 
  pagerCount <- getPageSizeSelect >>= flip attribute "value" >>= parseToInt
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
  actions do
    leftClick select
    leftClick option
    sendEnter
  where 
  getOption str = do
    config <- getConfig 
    options <- css config.explore.option >>= elements
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
    tableHtml <- getTable >>= innerHtml 
    setPageSizeOption numStr
    afterTableReload tableHtml
    count <- parseToInt numStr
    checkRowCount count
    successMsg $ "Ok, page size changed to " <> numStr



checkRowsPerPageCustom :: Check Unit
checkRowsPerPageCustom = withSmallZipsOpened do
  config <- getConfig 
  setPageSizeOption config.explore.optionCustom
  waitCheck (checker check) config.selenium.waitTime
  input <- getPageSizeInput
  successMsg "Ok, input has been appeared"
  rnd <- liftEff $ randomInt 1 99
  tableHtml <- getTable >>= innerHtml 
  actions do
    leftClick input
    sendSelectAll
    sendDelete
    sendKeys (show rnd)
    sendEnter
  afterTableReload tableHtml
  checkRowCount' (\tc _ -> tc == rnd) rnd
  successMsg $ "Ok, random (" <> show rnd <> ") row per page works"
  
  where
  check = do
    attempt getPageSizeInput >>= pure <<< either (const false) (const true)

newtype EnabledRecord =
  EnabledRecord { ff :: Boolean
                , sf :: Boolean
                , fb :: Boolean
                , sb :: Boolean
                , value :: String 
                }

instance eqEnabledRecord :: Eq EnabledRecord where
  eq (EnabledRecord r) (EnabledRecord r') =
    r.ff == r'.ff &&
    r.sf == r'.sf &&
    r.fb == r'.fb &&
    r.sb == r'.sb &&
    r.value == r'.value 

instance showEnabledRecord :: Show EnabledRecord where
  show (EnabledRecord r) =
    "(EnabledRecord { ff = " <> show r.ff <>
    ", sf = " <> show r.sf <>
    ", fb = " <> show r.fb <>
    ", sb = " <> show r.sb <>
    ", value = " <> show r.value <> 
    "})"
    
checkPagination :: Check Unit
checkPagination = withSmallZipsOpened do
  ff <- getFastForward
  sf <- getStepForward
  fb <- getFastBackward
  sb <- getStepBackward
  input <- getPaginationInput
  config <- getConfig

  enabledRecord <- getEnabledRecord
  checkRecord enabledRecord initialER "initial" 
  checkRowContent config.explore.firstPageContent "initial"
  initialHtml <- getTable >>= innerHtml
  actions $ leftClick sf
  afterTableReload initialHtml
  
  secondPageRecord <- getEnabledRecord
  checkRecord secondPageRecord secondPageER "second page"
  checkRowContent config.explore.secondPageContent "second page"
  secondHtml <- getTable >>= innerHtml
  actions $ leftClick ff
  afterTableReload secondHtml
  
  lastPageRecord <- getEnabledRecord
  checkRecord lastPageRecord lastPageER "last page"
  checkRowContent config.explore.lastPageContent "last page"
  lastHtml <- getTable >>= innerHtml
  actions $ leftClick sb
  afterTableReload lastHtml
  
  prenultPageRecord <- getEnabledRecord 
  checkRecord prenultPageRecord prenultPageER "prenult page"
  checkRowContent config.explore.prenultPageContent "prenult page"
  prenultHtml <- getTable >>= innerHtml
  actions $ leftClick fb
  afterTableReload prenultHtml
  
  firstPageRecord <- getEnabledRecord
  checkRecord firstPageRecord initialER "first page"
  checkRowContent config.explore.firstPageContent "first page"
  firstHtml <- getTable >>= innerHtml
  actions do
    leftClick input
    sendSelectAll
    sendKeys config.explore.customPageNumber
    sendEnter
  afterTableReload firstHtml

  customPageRecord <- getEnabledRecord
  let customMsg = "custom page (" <> config.explore.customPageNumber <> ")"
  checkRecord customPageRecord (customPageER config.explore.customPageNumber) $ 
    customMsg
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
    correctRows <- css config.explore.row >>= elements >>=
                   flip filterByContent (== sel)
    case length correctRows of
      0 -> errorMsg $ "There is no content that should be on first page (" <>
           msg <> ")"
      1 -> successMsg $ "Ok, page content checked (" <> msg <> ")"
      _ -> errorMsg $ "There is row dublicates (" <> msg <> ")"

  checkRecord actual expected msg = 
    if actual == expected 
    then successMsg $ "Ok, enabled records are equal (" <> msg <> ")"
    else errorMsg $ "Incorrect pagination buttons are enabled:\n" <>
         "case: " <> msg <> 
         "\nexpected: " <> show expected <>
         "\nactual: " <> show actual
         
  getEnabledRecord = do
    ff <- getFastForward
    sf <- getStepForward
    fb <- getFastBackward
    sb <- getStepBackward
    input <- getPaginationInput
    successMsg "Ok, all nav buttons is present in pager"
    r <- { ff: _
         , sf: _
         , fb: _
         , sb: _
         , value: _}
         <$> enabled ff
         <*> enabled sf
         <*> enabled fb
         <*> enabled sb
         <*> attribute input "value"
    pure $ EnabledRecord r

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
  sectionMsg "check notebook page loaded"
  notebookLoaded

  sectionMsg "new cell menu check"
  checkNewCellMenu
  
  sectionMsg "make explore cell check"
  checkMakeExploreCell
  
  sectionMsg "check deleting explore cells"
  C.checkDeleting getExploreCells
  
  sectionMsg "check show/hide editor (only for explore)"
  checkHideShow
  
  sectionMsg "File list in explore cell checking"
  FL.test withExploreCell
  
  sectionMsg "check that embed button, next cell menu and result is not visible"
  checkInitialExplore
  
  sectionMsg "check failures with empty input"
  checkEmptyInputErrors
  
  sectionMsg "check incorrect inputs"
  checkIncorrectInputs

  sectionMsg "check embed button"
  checkEmbedButton

  sectionMsg "check status"
  checkStatus
  
  sectionMsg "check next cells"
  checkNextCells
  
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
