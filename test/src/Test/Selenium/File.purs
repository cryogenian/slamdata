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

module Test.Selenium.File where

import Prelude
import Control.Bind ((=<<))
import Control.Alt ((<|>))
import Control.Monad.Error.Class (throwError)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Trans (lift)
import Data.Argonaut.Parser (jsonParser)
import Data.Argonaut.Core (toArray)
import Data.Argonaut.JCursor (toPrims)
import Data.Tuple (Tuple(..))
import Data.Maybe (Maybe(..), maybe, fromMaybe, isJust, isNothing)
import Data.Maybe.Unsafe (fromJust)
import Data.Either (Either(..), either)
import Data.Foldable (foldl, elem)
import Data.Traversable (traverse)
import Data.List (List(), length, reverse, filter, null, fromList, (!!))
import Selenium.Types
import Selenium.MouseButton
import Selenium.ActionSequence hiding (sequence)
import Selenium.Monad
import Selenium.Combinators (checker, awaitUrlChanged, waitUntilJust, tryToFind)
import Selenium (showLocator)
import Node.FS.Aff
import Node.Encoding (Encoding(UTF8))

import Test.Config
import SlamData.FileSystem.Routing (Routes(..), routing)
import SlamData.FileSystem.Routing.Search (searchPath)
import Routing (matchHash)
import Data.Array as Arr
import Data.String.Regex as R
import Data.String as Str
import Data.Set as S
import SlamData.Config as SDCfg
import SlamData.Config.Version as Version
import Test.Selenium.ActionSequence
import Test.Selenium.Common
import Test.Selenium.Monad
import Test.Selenium.Log


foreign import data MODULE :: !

click' :: Element -> Check Unit
click' = sequence <<< leftClick

home :: Check Unit
home = do
  getConfig >>= get <<< _.slamdataUrl
  fileComponentLoaded

findOpenItem :: String -> Check Element
findOpenItem name = tryRepeatedlyTo $ findExact =<< byXPath xPath
  where
  xPath = "//a[text()='" ++ name ++ "']"

loseItem :: String -> Check Unit
loseItem name = tryRepeatedlyTo $ loseElement =<< byXPath xPath
  where
  xPath = "//*[text()='" ++ name ++ "']"

findItem :: String -> Check Element
findItem name =
  tryRepeatedlyTo $ findExact =<< byXPath (selectXPath `xPathOr` deselectXPath)
  where
  selectXPath = "//*[@aria-label='Select " ++ name ++ "']"
  deselectXPath = "//*[@aria-label='Deselect " ++ name ++ "']"
  xPathOr x y = x ++ "|" ++ y

selectFile :: String -> Check Unit
selectFile filename = click' =<< findSingle =<< byCss (selectFileCss filename)
  where
  selectFileCss filename = "*[aria-label='Select " ++ filename ++ "']"

findTestDb :: Check Element
findTestDb = do
  config <- getConfig
  findItem config.database.name

findOpenTestDb :: Check Element
findOpenTestDb = do
  config <- getConfig
  findOpenItem config.database.name

findUploadedItem :: Check Element
findUploadedItem = do
  config <- getConfig
  findItem config.move.name

type MountConfigR =
  { host :: String
  , port :: Int
  }

mountConfigFromConfig :: Check MountConfigR
mountConfigFromConfig = do
  config <- getConfig
  pure { host : config.mongodb.host
       , port : config.mongodb.port
       }

mountDatabaseWithMountConfig :: MountConfigR -> Check Unit
mountDatabaseWithMountConfig mountConfig = do
  home
  getMountDatabaseButton >>= sequence <<< leftClick
  waitModalShown
  mac <- isMac
  chrome <- isChrome
  if mac && chrome
    then fieldByField
    else copyPaste

  where
  connectionUri :: Config -> String
  connectionUri config =
    "mongodb://"
    ++ mountConfig.host
    ++ ":"
    ++ show mountConfig.port

  fieldByField :: Check Unit
  fieldByField = tryRepeatedlyTo do
    warnMsg $ "This test doesn't check correctness of copy/paste.\n"
      <> "It's known bug of selenium/chrome/mac combination that modifier keys\n"
      <> "doesn't work"
    config <- getConfig
    nameField <- getNameField
    portField <- getPortField
    hostField <- getHostField
    pathField <- getPathField
    saveButton <- getSaveButton
    sequence do
      leftClick nameField
      keys config.mount.name
      leftClick portField
      keys $ show mountConfig.port
      leftClick hostField
      keys mountConfig.host
      leftClick pathField
      keys config.database.name
      leftClick saveButton

  copyPaste :: Check Unit
  copyPaste = tryRepeatedlyTo do
    config <- getConfig
    uriField <- getUriField
    nameField <- getNameField
    saveButton <- getSaveButton
    modifierKey <- getModifierKey
    sequence do
      leftClick nameField
      keys $ connectionUri config
      selectAll modifierKey
      copy modifierKey
      selectAll modifierKey
      keys config.mount.name

      leftClick uriField
      paste modifierKey

      leftClick saveButton

  getSaveButton :: Check Element
  getSaveButton = do
    config <- getConfig
    tryRepeatedlyTo $ byCss config.configureMount.saveButton >>= findExact

  getUriField :: Check Element
  getUriField = do
    config <- getConfig
    tryRepeatedlyTo $ byCss config.configureMount.uriField >>= findExact

  getNameField :: Check Element
  getNameField = do
    config <- getConfig
    tryRepeatedlyTo $ byCss config.configureMount.nameField >>= findExact

  getPathField :: Check Element
  getPathField = do
    config <- getConfig
    tryRepeatedlyTo $ byCss config.configureMount.pathField >>= findExact

  getPortField :: Check Element
  getPortField = do
    config <- getConfig
    tryRepeatedlyTo $ byCss config.configureMount.portField >>= findExact

  getHostField :: Check Element
  getHostField = do
    config <- getConfig
    tryRepeatedlyTo $ byCss config.configureMount.hostField >>= findExact

  getMountDatabaseButton :: Check Element
  getMountDatabaseButton = do
    config <- getConfig
    tryToFind $ byAriaLabel config.toolbar.mountDatabase

goodMountDatabase :: Check Unit
goodMountDatabase = do
  sectionMsg "MOUNT TEST DATABASE"
  mountConfigFromConfig
    >>= mountDatabaseWithMountConfig

  config <- getConfig
  tryRepeatedlyTo expectMountShown

  where
  expectMountShown :: Check Unit
  expectMountShown = do
    config <- getConfig
    void $ findItem config.mount.name

badMountDatabase :: Check Unit
badMountDatabase = do
  sectionMsg "BAD MOUNT TEST DATABASE"
  config <- getConfig
  badMountConfig
    >>= mountDatabaseWithMountConfig
  warningBox <- getElementByCss config.configureMount.warningBox "no warning box"
  -- wait for any old validation messages to disappear
  wait (checker $ not <$> isDisplayed warningBox) config.selenium.waitTime
  -- wait for the server error to appear
  wait (checker $ isDisplayed warningBox) 8000

  tryRepeatedlyTo
    $ getElementByCss config.configureMount.cancelButton "no cancel button"
    >>= sequence <<< leftClick
  waitModalDismissed
  where
    badMountConfig :: Check MountConfigR
    badMountConfig = do
      mountConfig <- mountConfigFromConfig
      pure $ mountConfig { port = mountConfig.port - 1 }

unmountDatabase :: Check Unit
unmountDatabase = do
  sectionMsg "UNMOUNT TEST DATABASE"
  home
  config <- getConfig
  click' =<< findItem config.mount.name
  click' =<< itemGetDeleteIcon =<< findItem config.mount.name
  loseItem config.mount.name
  successMsg "successfully unmounted"


checkMountedDatabase :: Check Unit
checkMountedDatabase = do
  sectionMsg "CHECK TEST DATABASE IS MOUNTED"
  enterMount
  void $ findTestDb
  successMsg "test database found"

checkConfigureMount :: Check Unit
checkConfigureMount = do
  sectionMsg "CHECK CONFIGURE MOUNT DIALOG"
  enterMount

  button <- getConfigureMountButton
  successMsg "got configure-mount button"
  sequence $ leftClick button

  config <- getConfig
  waitModalShown
  successMsg "configure-mount dialog shown"

  -- make sure a no-op edit doesn't result in a validation error
  usernameField <- getElementByCss config.configureMount.usernameField "no usernameField field"

  modifierKey <- getModifierKey
  sequence do
    leftClick usernameField
    sendBackspaces 100
    keys "hello"
    undo modifierKey

  getElementByCss config.configureMount.saveButton "no save button"
    >>= isEnabled
    >>= assertBoolean "save button should be enabled"

  where
  getConfigureMountButton :: Check Element
  getConfigureMountButton = do
    config <- getConfig
    tryToFind $ byAriaLabel config.toolbar.configureMount

getItemToolbar :: Check { listGroupItem :: Element, itemToolbar :: Element}
getItemToolbar = do
  config <- getConfig
  listGroupItem <- getElementByCss config.item.main "there is no list-group-item"
  byCss config.item.toolbar
    >>= findChild listGroupItem
    >>= maybe toolbarErrorMsg (\tb -> pure { listGroupItem : listGroupItem, itemToolbar : tb })
  where
    toolbarErrorMsg = errorMsg "there is no toolbar in list-group-item"

checkItemToolbar :: Check Unit
checkItemToolbar = do
  sectionMsg "CHECK ITEM TOOLBAR"
  enterMount
  -- W/o this it will show toolbar
  sequence $ mouseToLocation {x: zero, y: zero}
  { listGroupItem : groupItem, itemToolbar : toolbar } <- getItemToolbar

  apathize do
    assertBoolean "toolbar should not be displayed" <<< not =<< isDisplayed toolbar
    successMsg "toolbar is hidden"
    sequence $ hover groupItem
    assertBoolean "hovered toolbar should be visible" =<< isDisplayed toolbar
    successMsg "hovered toolbar is visible"
  apathize do
    style <- getCssValue groupItem "background-color"
    sequence $ leftClick groupItem
    newStyle <- getCssValue groupItem "background-color"
    assertBoolean "background-color should have been changed after click'" $ newStyle /= style
    successMsg "background-color has been changed after click'"

checkURL :: Check Unit
checkURL = do
  sectionMsg "CHECKING URL"
  home
  getCurrentUrl >>= getHashFromURL >>= checkHash

  where
  checkHash :: Routes -> Check Unit
  checkHash (Salted sort search salt) = do
    successMsg "hash is correct"
    if searchPath search == Just "/"
      then successMsg "search path is correct"
      else errorMsg "incorrect search path"
  checkHash _ =
    errorMsg "need additional redirects"


enterMount :: Check Unit
enterMount = do
  home
  url <- getCurrentUrl
  config <- getConfig
  click' =<< findOpenItem config.mount.name
  wait (awaitUrlChanged url) config.selenium.waitTime
  fileComponentLoaded

goDown :: Check Unit
goDown = do
  sectionMsg "CHECKING GO DOWN"

  enterMount
  url <- getCurrentUrl
  oldHash <- either (const $ errorMsg "incorrect initial hash in goDown") pure $ matchHash routing (dropHash url)
  checkOldHash url oldHash

  where
  checkOldHash url old@(Salted oldSort oldSearch oldSalt) = do
    click' =<< findOpenTestDb
    config <- getConfig
    wait (awaitUrlChanged url) config.selenium.waitTime
    fileComponentLoaded
    getCurrentUrl >>= getHashFromURL >>= checkHashes old
  checkOldHash _ _ = errorMsg "weird initial hash in goDown"

  checkHashes (Salted oldSort oldSearch oldSalt) (Salted sort search salt) = do
    config <- getConfig
    if (oldSalt == salt) &&
       (oldSort == sort) &&
       ((searchPath search) == (Just $ "/" <> config.mount.name <> "/" <> config.database.name <> "/"))
      then successMsg "correct hash after goDown"
      else errorMsg $ "incorrect hash after goDown " <> (fromMaybe "" $ searchPath search)
  checkHashes _ _ = do
    errorMsg "weird hash after goDown"

checkBreadcrumbs :: Check Unit
checkBreadcrumbs = do
  sectionMsg "BREADCRUMBS"
  home
  texts <- bTexts
  config <- getConfig
  assertBoolean "incorrect root breadcrumb" $ elem config.breadcrumbs.home texts
  successMsg "correct root breadcrumb"

  goDown
  nTexts <- bTexts
  assertBoolean "incorrect root breadcrumb after goDown" $ elem config.breadcrumbs.home texts
  assertBoolean "breadcrumbs are not updated" $ elem config.database.name nTexts
  successMsg "breadcrumbs are updated"

  homeBreadcrumb <- getHomeBreadcrumb
  sequence $ leftClick homeBreadcrumb
  fileComponentLoaded
  checkURL
  successMsg "Ok, went home after click' on root breadcrumb"

  where
  getHomeBreadcrumb :: Check Element
  getHomeBreadcrumb =
    anchors
      >>= foldl foldMFn (pure Nothing)
      >>= maybe errHome pure

  errHome = errorMsg "There is no Home breadcrumb: since you have already checked that it exists, there is probably error in test"

  breadcrumbs :: Check Element
  breadcrumbs = do
    config <- getConfig
    getElementByCss config.breadcrumbs.main "There is no breadcrumbs"

  anchors :: Check (List Element)
  anchors = do
    bs <- breadcrumbs
    config <- getConfig
    byCss config.breadcrumbs.text
      >>= findChildren bs

  bTexts :: Check (List String)
  bTexts = anchors >>= traverse getInnerHtml

  foldMFn :: Check (Maybe Element) -> Element -> Check (Maybe Element)
  foldMFn cMe el = cMe >>= \me ->
    case me of
      Just el -> pure $ pure el
      Nothing -> do
        html <- getInnerHtml el
        config <- getConfig
        if config.breadcrumbs.home == html
          then pure $ pure el
          else pure Nothing

getItemTexts :: Check (List String)
getItemTexts = do
    config <- getConfig
    locator <- byCss config.sort.main
    els <- findElements locator
    if length els == 0
       then throwError $ error $ "No elements found using locator " ++ showLocator locator
       else pure unit
    (extractText <$>) <$> traverse getInnerHtml els
  where
  extractText =
    R.replace (R.regex "<i.+>.*</i>" R.noFlags) ""

sorting :: Check Unit
sorting = do
  sectionMsg "SORTING CHECK"
  goDown
  texts <- getItemTexts
  getCurrentUrl >>= getHashFromURL >>= checkHash texts
  where
  checkHash :: List String -> Routes -> Check Unit
  checkHash texts (Salted sort search salt) = do
    config <- getConfig
    sortButton <- getElementByCss config.sort.button "there is no sort button"
    click' sortButton
    fileComponentLoaded
    nTexts <- getItemTexts
    if reverse nTexts == texts
      then successMsg "OK, sort works"
      else errorMsg "Sorting doesn't work"
  checkHash _ _ = errorMsg "need additional redirects in sorting"

fileUpload :: Check Unit
fileUpload = do
  sectionMsg "FILE UPLOAD"
  config <- getConfig
  successMsg "went down"
  oldItems <- S.fromList <$> getItemTexts
  script """
  var els = document.getElementsByTagName('i');
  for (var i = 0; i < els.length; i++) {
    if (/hidden-file-input/.test(els[i].className)) {
      els[i].className = "";
    }
  }
  """
  uploadInput <- getElementByCss config.upload.input "There is no upload input"
  sendKeysEl config.upload.filePath uploadInput
  wait awaitInNotebook config.selenium.waitTime
  successMsg "Ok, explore notebook created"
  navigateBack
  fileComponentLoaded

  tryRepeatedlyTo do
    items <- S.fromList <$> getItemTexts
    if S.isEmpty $ S.difference items oldItems
      then errorMsg "items has not changed after upload"
      else successMsg "new items added after upload"

shareFile :: Check Unit
shareFile = do
  sectionMsg "SHARE FILE"
  goDown
  config <- getConfig
  selectFile config.move.name
  click' =<< itemGetShareIcon =<< findUploadedItem
  waitModalShown
  successMsg "Share modal dialog appeared"
  urlFieldLocator <- byCss config.share.urlField
  urlField <- findSingle urlFieldLocator
  urlValue <- getAttribute urlField attr >>= maybe (attrFail urlFieldLocator attr) pure
  get urlValue
  wait awaitInNotebook config.selenium.waitTime
  successMsg "Ok, share link led to notebook"
    where
    attr = "value"
    itemGetShareIcon :: Element -> Check Element
    itemGetShareIcon item = do
      config <- getConfig
      tryRepeatedlyTo $ byAriaLabel config.share.markShare >>= childExact item

searchForUploadedFile :: Check Unit
searchForUploadedFile = do
  sectionMsg "SEARCH"
  home
  config <- getConfig
  searchInput <- getElementByCss config.search.searchInput "no search input field"
  url <- getCurrentUrl
  let filename = fromMaybe config.upload.filePath
                 $ Arr.last $ Str.split "/" config.upload.filePath
  searchButton <- getElementByCss config.search.searchButton "no search button"
  sequence $ do
    leftClick searchInput
    keys filename

  searchButton <- getElementByCss config.search.searchButton "no search button"
  sequence $ leftClick searchButton
  wait (awaitUrlChanged url) config.selenium.waitTime
  wait (awaitItemWithPhrase filename) config.selenium.waitTime
  successMsg "Searched for and found item"

awaitItemWithPhrase :: String -> Check Boolean
awaitItemWithPhrase phrase = checker $ do
  texts <- attempt getItemTexts
  pure $ case texts of
    Left _ -> false
    Right texts -> not $ null $ filter (contains phrase) texts

  where
    contains :: String -> String -> Boolean
    contains phrase = R.test (R.regex phrase R.noFlags)


awaitInNotebook :: Check Boolean
awaitInNotebook = checker $ do
  url <- getCurrentUrl
  rgx <- nbRegex
  successMsg $ "URL: " ++ url
  pure $ R.test rgx url

  where
    nbRegex = do
      config <- getConfig
      let phrase = "notebook.html#/explore/" <> config.mount.name <> "/" <> config.database.name
      pure $ R.regex phrase R.noFlags


itemGetDeleteIcon :: Element -> Check Element
itemGetDeleteIcon item = do
  config <- getConfig
  tryRepeatedlyTo $ byAriaLabel config.move.markDelete >>= childExact item

-- | Activate the item's toolbar and click' a button/icon in it
itemClickToolbarIcon :: Element -> Element -> Check Unit
itemClickToolbarIcon item icon = do
  config <- getConfig
  sequence $ leftClick item
  wait (checker $ isDisplayed icon) config.selenium.waitTime
  sequence $ leftClick icon

trashCheck :: Check Unit
trashCheck = do
  sectionMsg "TRASH CHECK"
  goDown
  expectTrashFileToBeHidden
  successMsg "Trash is hidden"
  click' =<< getShowHideButton
  expectTrashFileToBePresented
  click' =<< findOpenItem SDCfg.trashFolder
  fileComponentLoaded
  deletedItem <- findDeletedItem
  successMsg "Deleted item found"

  where
  expectTrashFileToBeHidden = loseItem SDCfg.trashFolder
  expectTrashFileToBePresented = void $ findItem SDCfg.trashFolder

  getShowHideButton :: Check Element
  getShowHideButton = do
    config <- getConfig
    tryRepeatedlyTo $ findExact =<< byAriaLabel config.toolbar.showHide

  findDeletedItem :: Check Element
  findDeletedItem = do
    config <- getConfig
    findItem config.move.other

createFolder :: Check Unit
createFolder = do
  sectionMsg "NEW FOLDER CHECK"
  goDown
  loseItem SDCfg.newFolderName
  click' =<< getNewFolderButton
  click' =<< findOpenItem SDCfg.newFolderName
  tryRepeatedlyTo $ getCurrentUrl >>= getHashFromURL >>= checkHash

  where

  getNewFolderButton :: Check Element
  getNewFolderButton = do
    config <- getConfig
    tryToFind $ byAriaLabel config.toolbar.newFolder

  checkHash :: Routes -> Check Unit
  checkHash (Salted sort search salt) = do
    config <- getConfig
    let expectedPath = "/" <> config.mount.name <> "/" <> config.database.name <> "/" <> SDCfg.newFolderName <> "/"
    let actualPath = searchPath search
    if actualPath == (Just expectedPath)
      then successMsg "ok, hash correct"
      else throwError $ error $
        "hash incorrect in created folder; expected '"
         <> expectedPath
         <> "', but got '"
         <> show actualPath
         <> "'."
  checkHash _ = errorMsg "incorrect hash"

createNotebookAndThen :: Check Unit -> Check Unit
createNotebookAndThen andThen = do
  goDown
  config <- getConfig
  sequence <<< leftClick =<< getNewNotebookButton
  attempt (wait notebookCheck config.selenium.waitTime)
    >>= either (\_ -> errorMsg "no redirect to notebook") (\_ -> successMsg "ok, notebook created")
  andThen
  where
  getNewNotebookButton :: Check Element
  getNewNotebookButton = do
    config <- getConfig
    tryToFind $ byAriaLabel config.toolbar.newNotebook

  notebookCheck :: Check Boolean
  notebookCheck = checker $ R.test (R.regex "notebook.html" R.noFlags) <$> getCurrentUrl

createNotebook :: Check Unit
createNotebook = do
  sectionMsg "NEW NOTEBOOK CHECK"
  createNotebookAndThen do
    navigateBack
    fileComponentLoaded
    newNotebook <- getNewNotebook
    successMsg "OK, new notebook found in parent directory"

  where
  getNewNotebook :: Check Element
  getNewNotebook = findItem SDCfg.newNotebookName



checkTitle :: Check Unit
checkTitle = do
  windowTitle <- getTitle
  if Str.contains Version.slamDataVersion windowTitle
    then successMsg "Title contains version"
    else errorMsg $ "Title (" ++ windowTitle ++ ") doesn't contain version"

moveDelete :: String -> Check Unit -> String -> String -> Check Unit
moveDelete msg setUp src tgt = do
  sectionMsg $ "check move/delete " <> msg
  setUp
  config <- getConfig
  selectFile src
  click' =<< (itemGetMoveIcon =<< findItem src )
  waitModalShown
  tryRepeatedlyTo
    $ getElementByCss config.move.nameField "no rename field"
    >>= editNameField

  tryRepeatedlyTo
    $ getElementByCss config.move.submit "no submit button"
    >>= sequence <<< leftClick

  findItem tgt
  successMsg $ "ok, successfully renamed (" <> msg <> ")"

  selectFile tgt
  click' =<< (itemGetDeleteIcon =<< findItem tgt)
  tryRepeatedlyTo $ loseItem tgt
  successMsg $ "ok, successfully deleted (" <> msg <> ")"
  where
  itemGetMoveIcon :: Element -> Check Element
  itemGetMoveIcon item = do
    config <- getConfig
    tryRepeatedlyTo $ byAriaLabel config.move.markMove >>= childExact item

  editNameField :: Element -> Check Unit
  editNameField nameField = do
    config <- getConfig
    modifierKey <- getModifierKey
    sequence do
      leftClick nameField
      sendBackspaces 100
      keys tgt

moveDeleteDatabase :: Check Unit
moveDeleteDatabase = do
  config <- getConfig
  moveDelete "database" home config.mount.name config.mount.otherName

moveDeleteFolder :: Check Unit
moveDeleteFolder = do
  config <- getConfig
  moveDelete "folder" goDown SDCfg.newFolderName config.move.other

moveDeleteNotebook :: Check Unit
moveDeleteNotebook = do
  config <- getConfig
  moveDelete "notebook" goDown SDCfg.newNotebookName config.move.other

moveDeleteFile:: Check Unit
moveDeleteFile = do
  config <- getConfig
  moveDelete "file" goDown config.move.name config.move.other



downloadResource :: Check Unit
downloadResource = do
  sectionMsg "Download resource, smoke tests"
  enterMount
  goDown
  config <- getConfig
  tryRepeatedlyTo do
    btn <- getToolbarDownloadButton
    sequence $ leftClick btn
  waitModalShown
  successMsg "Ok, global download dialog shown"
  cancelDownload
  successMsg "Ok, global download dialog hidden"
  tryRepeatedlyTo do
    item <- getDownloadItem
    sequence $ hover item
    getItemDownloadButton item >>= sequence <<< leftClick
  waitModalShown
  successMsg "Ok, item download dialog shown"
  tryRepeatedlyTo do
    val <- getSourceInput >>= flip getAttribute "value"
    assertBoolean "Error, value should be equal to resource path"
      $ Just "/test-mount/testDb/smallZips" == val
  successMsg "Ok, correct source value"
  proceedDownload
  content <- readDownloaded
  checkCSV "\n" "," content >>= assertBoolean "Error: incorrect csv file"
  successMsg "Ok, csv is correct"
  rmDownloaded
  tryRepeatedlyTo do
    click' =<< getRowDelimiterInput
    sequence $ sendBackspaces 10
    sequence $ keys "*"
    click' =<< getColDelimiterInput
    sequence $ sendBackspaces 10
    sequence $ keys ";"
    proceedDownload
  semicolonContent <- readDownloaded
  checkCSV "*" ";" semicolonContent
    >>= assertBoolean "Error: incorrect csv file (after delimiters change)"
  successMsg "Ok, delimiters set correctly"
  rmDownloaded
  tryRepeatedlyTo $ getJsonTab >>= sequence <<< leftClick
  proceedDownload
  warnMsg "Note, application/json can't be autodownloaded"
  mainHandle <- getWindowHandle

  tryRepeatedlyTo do
    handles <- getAllWindowHandles
    -- This is benigh. We use newTab to download file.
    switchTo $ fromJust $ handles !! 1
  jsonContent <- tryRepeatedlyTo $ byCss "pre" >>= findExact >>= getText
  checkJson jsonContent >>= assertBoolean "Error: incorrect json"
  closeWindow
  switchTo mainHandle
  successMsg "Ok, correct json file"
  getMutliLineRadio >>= sequence <<< leftClick
  proceedDownload
  jsonMLContent <- readDownloaded
  checkMultiLineJson jsonMLContent
    >>= assertBoolean "Error: incorrect multiline json"
  successMsg "Ok, multiline json file"
  rmDownloaded
  where
  getToolbarDownloadButton = do
    config <- getConfig
    tryToFind $ byAriaLabel config.toolbar.download

  cancelDownload = do
    config <- getConfig
    btn <- tryToFind $ byAriaLabel config.download.cancel
    sequence $ leftClick btn
    waitModalDismissed

  proceedDownload = do
    config <- getConfig
    btn <- tryToFind $ byAriaLabel config.download.proceed
    sequence $ leftClick btn

  getDownloadItem = do
    config <- getConfig
    findItem config.download.item
  getItemDownloadButton item = do
    config <- getConfig
    tryRepeatedlyTo $ byAriaLabel config.toolbar.download >>= childExact item

  getSourceInput = do
    config <- getConfig
    byCss config.download.sourceInputSelector >>= findExact

  getTargetInput = do
    config <- getConfig
    byCss config.download.targetInputSelector >>= findExact

  getRowDelimiterInput = do
    config <- getConfig
    byXPath config.download.rowDelimiterInputSelector >>= findExact

  getColDelimiterInput = do
    config <- getConfig
    byXPath config.download.colDelimiterInputSelector >>= findExact

  getJsonTab = do
    config <- getConfig
    byXPath config.download.jsonTabSelector >>= findExact

  getMutliLineRadio = do
    config <- getConfig
    byXPath config.download.multiLineJsonRadioSelector >>= findExact

  readDownloaded = tryRepeatedlyTo do
    config <- getConfig
    await "File has not been downloaded" do
      files <- lift $ readdir config.download.folder
      pure $ isJust $ Arr.elemIndex config.download.item files
    res <- lift $ readTextFile UTF8
           $ config.download.folder <> "/" <> config.download.item
    if res == ""
      then throwError $ error "empty file content"
      else pure res

  checkCSV rowSep colSep content = do
    config <- map _.download getConfig
    let lines = Arr.filter (not <<< eq "") $ Str.split rowSep content
        cells = Arr.filter (not <<< eq "") $ lines >>= Str.split colSep
    -- one for headers
    pure $ (Arr.length lines) == (config.rowCount + one)
      && (Arr.length cells) == (config.rowCount + one) * config.colCount

  rmDownloaded = do
    config <- getConfig
    lift $ unlink $ config.download.folder
      <> "/" <> config.download.item
    files <- lift $ readdir config.download.folder
    if isJust $ Arr.elemIndex config.download.item files
      then rmDownloaded
      else pure unit

  checkJson content = do
    config <- map _.download getConfig
    pure $ either (const false) id do
      json <- jsonParser content
      arr <- maybe (Left "it's not an array") Right $ toArray json
      let tplList = arr >>= toPrims >>> fromList
      pure $ Arr.length arr == config.rowCount
        &&  Arr.length tplList == config.rowCount * config.colCount

  checkMultiLineJson content = do
    config <- map _.download getConfig
    let arr = Arr.filter (not <<< eq "") $ Str.split "\n" content
        tplList = (either (const []) id $ traverse jsonParser arr)
                >>= toPrims >>> fromList
    pure $ Arr.length arr == config.rowCount
      && Arr.length tplList == config.rowCount * config.colCount

test :: Check Unit
test = do
  home
  startSpying
  badMountDatabase
  goodMountDatabase
  moveDeleteDatabase
  goodMountDatabase
  unmountDatabase
  goodMountDatabase
  checkMountedDatabase
  checkConfigureMount
  checkItemToolbar
  checkURL
  goDown
  checkBreadcrumbs
  sorting
  fileUpload
  searchForUploadedFile
  shareFile
  moveDeleteFile
  trashCheck
  checkTitle
  createFolder
  moveDeleteFolder
--  createNotebook
--  moveDeleteNotebook
  downloadResource
