module Test.Selenium.File where

import Prelude
import DOM (DOM())
import Control.Apply ((*>))
import Control.Bind ((>=>), (=<<))
import Control.Monad.Error.Class (throwError)
import Control.Monad.Eff.Exception (error)
import Data.Tuple (Tuple(..))
import Data.Maybe (Maybe(..), maybe, fromMaybe, isJust, isNothing)
import Data.Maybe.Unsafe (fromJust)
import Data.Either (Either(..), either, isLeft)
import Data.Foldable (foldl, elem, find)
import Data.Traversable (traverse)
import Data.List (List(), reverse, filter, null)
import Selenium.Types
import Selenium.MouseButton
import Selenium.ActionSequence
import Selenium.Key

import Utils.Log
import Test.Config
import Driver.File.Routing (Routes(..), routing)
import Driver.File.Search (searchPath)
import Routing (matchHash)
import qualified Data.Array as Arr
import qualified Data.String.Regex as R
import qualified Data.String as Str
import qualified Data.Char as Ch
import qualified Data.StrMap as SM
import qualified Data.Set as S
import qualified Config as SDCfg
import Test.Selenium.Common
import Test.Selenium.Monad
import Test.Selenium.Log

foreign import data MODULE :: !

home :: Check Unit
home = do
  getConfig >>= goTo <<< _.slamdataUrl
  fileComponentLoaded

findItem :: String -> Check (Maybe Element)
findItem name = do
  config <- getConfig
  els <- css config.item.main >>= elements
  tpls <- traverse traverseFn els
  pure $ foldl foldFn Nothing tpls
  where
  traverseFn el = do
    eHtml <- attempt $ innerHtml el
    pure $ Tuple el $ case eHtml of
      Left _ -> ""
      Right html -> html
  foldFn (Just el) _ = Just el
  foldFn Nothing (Tuple el html) =
    if R.test (R.regex name R.noFlags) html
    then Just el
    else Nothing

toolbarButton :: String -> Check (Maybe Element)
toolbarButton key = do
  config <- getConfig
  toolbar <- getElementByCss config.toolbar.main "no toolbar"
  checkLocator (locator key) >>= child toolbar

  where
  locator :: String -> Element -> Check Element
  locator key el = do
    config <- getConfig
    css config.toolbar.button >>=
      children el >>=
      traverse traverseFn >>=
      foldl foldFn Nothing >>>
      maybe (throwError $ error "there is no button") pure
  traverseFn btn = do
    ch <- css key >>= child btn
    pure $ Tuple btn (isJust ch)
  foldFn :: Maybe Element -> Tuple Element Boolean -> Maybe Element
  foldFn Nothing (Tuple btn true) = Just btn
  foldFn a _ = a


findTestDb :: Check (Maybe Element)
findTestDb = do
  config <- getConfig
  findItem config.database.name

getTestDb :: Check Element
getTestDb = findTestDb >>= maybe (errorMsg "There is no test database") pure

findUploadedItem :: Check (Maybe Element)
findUploadedItem = do
  config <- getConfig
  findItem config.move.name

getUploadedItem :: Check Element
getUploadedItem =
  findUploadedItem
    >>= maybe (errorMsg "File has not been uploaded") pure

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
  mountButton <- getMountDatabaseButton
  actions $ leftClick mountButton
  config <- getConfig
  waitCheck modalShown config.selenium.waitTime
  uriField <- getElementByCss config.configureMount.uriField "no connection uri field"
  nameField <- getElementByCss config.configureMount.nameField "no mount name field"
  pathField <- getElementByCss config.configureMount.pathField "no path field"
  saveButton <- getElementByCss config.configureMount.saveButton "no save button"

  let connectionUri = "mongodb://" ++ mountConfig.host ++ ":" ++ show mountConfig.port ++ "/" ++ config.database.name
  actions $ do
    -- a strange hack follows to get the uri onto the clipboard, since the uri
    -- field cannot be edited except by pasting.
    leftClick nameField
    sendKeys connectionUri
    sendSelectAll
    sendCopy
    sendSelectAll
    sendKeys config.mount.name

    leftClick uriField
    sendPaste

    leftClick saveButton

  where
  getMountDatabaseButton :: Check Element
  getMountDatabaseButton = do
    config <- getConfig
    toolbarButton config.toolbar.mountDatabase
      >>= maybe (errorMsg "No mount database button") pure

goodMountDatabase :: Check Unit
goodMountDatabase = do
  sectionMsg "MOUNT TEST DATABASE"
  mountConfigFromConfig
    >>= mountDatabaseWithMountConfig

  config <- getConfig
  waitCheck mountShown config.selenium.waitTime

  where
    mountShown :: Check Boolean
    mountShown = checker $ do
      config <- getConfig
      isJust <$> findItem config.mount.name

badMountDatabase :: Check Unit
badMountDatabase = do
  sectionMsg "BAD MOUNT TEST DATABASE"
  config <- getConfig

  badMountConfig
    >>= mountDatabaseWithMountConfig

  warningBox <- getElementByCss config.configureMount.warningBox "no warning box"

  -- wait for any old validation messages to disappear
  waitCheck (checker $ not <$> visible warningBox) config.selenium.waitTime
  -- wait for the server error to appear
  waitCheck (checker $ visible warningBox) 8000

  cancelButton <- getElementByCss config.configureMount.cancelButton "no cancel button"
  actions $ leftClick cancelButton
  waitCheck modalDismissed config.selenium.waitTime

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
  mountItem <-
    findItem config.mount.name
      >>= maybe (errorMsg "No mount item") pure

  actions $ leftClick mountItem
  itemGetDeleteIcon mountItem >>= itemClickToolbarIcon mountItem
  waitCheck (checker $ isNothing <$> findItem config.mount.name) config.selenium.waitTime
  successMsg "successfully unmounted"


checkMountedDatabase :: Check Unit
checkMountedDatabase = do
  sectionMsg "CHECK TEST DATABASE IS MOUNTED"
  enterMount
  _ <- getTestDb
  successMsg "test database found"

checkConfigureMount :: Check Unit
checkConfigureMount = do
  sectionMsg "CHECK CONFIGURE MOUNT DIALOG"
  enterMount

  button <- getConfigureMountButton
  successMsg "got configure-mount button"
  actions $ leftClick button

  config <- getConfig
  waitCheck modalShown config.selenium.waitTime
  successMsg "configure-mount dialog shown"

  -- make sure a no-op edit doesn't result in a validation error
  usernameField <- getElementByCss config.configureMount.usernameField "no usernameField field"
  actions do
    leftClick usernameField
    sendSelectAll
    sendKeys "hello"
    sendUndo

  getElementByCss config.configureMount.saveButton "no save button"
    >>= enabled
    >>= assertBoolean "save button should be enabled"

  where
  getConfigureMountButton :: Check Element
  getConfigureMountButton = do
    config <- getConfig
    toolbarButton config.toolbar.configureMount
      >>= maybe (errorMsg "No configure mount button") pure


getItemToolbar :: Check { listGroupItem :: Element, itemToolbar :: Element}
getItemToolbar = do
  config <- getConfig
  listGroupItem <- getElementByCss config.item.main "there is no list-group-item"
  css config.item.toolbar
    >>= child listGroupItem
    >>= maybe toolbarErrorMsg (\tb -> pure { listGroupItem : listGroupItem, itemToolbar : tb })
  where
    toolbarErrorMsg = errorMsg "there is no toolbar in list-group-item"

checkItemToolbar :: Check Unit
checkItemToolbar = do
  sectionMsg "CHECK ITEM TOOLBAR"
  enterMount
  -- W/o this it will show toolbar
  actions $ mouseToLocation {x: 0.0, y: 0.0}
  { listGroupItem : groupItem, itemToolbar : toolbar } <- getItemToolbar

  apathize do
    assertBoolean "toolbar should not be displayed" <<< not =<< visible toolbar
    successMsg "toolbar is hidden"
    actions $ hover groupItem
    assertBoolean "hovered toolbar should be visible" =<< visible toolbar
    successMsg "hovered toolbar is visible"
  apathize do
    style <- getCss groupItem "background-color"
    actions $ leftClick groupItem
    newStyle <- getCss groupItem "background-color"
    assertBoolean "background-color should have been changed after click" $ newStyle /= style
    successMsg "background-color has been changed after click"

checkURL :: Check Unit
checkURL = do
  sectionMsg "CHECKING URL"
  home
  getURL >>= getHashFromURL >>= checkHash

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
  url <- getURL
  config <- getConfig
  mountItem <- findItem config.mount.name >>= maybe (errorMsg "No mount item") pure
  actions $ doubleClick leftButton mountItem
  waitCheck (awaitUrlChanged url) config.selenium.waitTime
  fileComponentLoaded

goDown :: Check Unit
goDown = do
  sectionMsg "CHECKING GO DOWN"

  enterMount
  url <- getURL
  testDb <- getTestDb
  oldHash <- either (const $ errorMsg "incorrect initial hash in goDown") pure $ matchHash routing (dropHash url)
  checkOldHash url testDb oldHash

  where

  checkOldHash url el old@(Salted oldSort oldSearch oldSalt) = do
    actions $ doubleClick leftButton el
    config <- getConfig
    waitCheck (awaitUrlChanged url) config.selenium.waitTime
    fileComponentLoaded
    getURL >>= getHashFromURL >>= checkHashes old
  checkOldHash _ _ _ = errorMsg "weird initial hash in goDown"

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
  actions $ leftClick homeBreadcrumb
  fileComponentLoaded
  checkURL
  successMsg "Ok, went home after click on root breadcrumb"

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
    css config.breadcrumbs.text
      >>= children bs

  bTexts :: Check (List String)
  bTexts = anchors >>= traverse innerHtml

  foldMFn :: Check (Maybe Element) -> Element -> Check (Maybe Element)
  foldMFn cMe el = cMe >>= \me ->
    case me of
      Just el -> pure $ pure el
      Nothing -> do
        html <- innerHtml el
        config <- getConfig
        if config.breadcrumbs.home == html
          then pure $ pure el
          else pure Nothing

getItemTexts :: Check (List String)
getItemTexts = do
    config <- getConfig
    els <- css config.sort.main >>= elements
    (extractText <$>) <$> traverse innerHtml els
  where
  extractText =
    R.replace (R.regex "<i.+>.*</i>" R.noFlags) ""

sorting :: Check Unit
sorting = do
  sectionMsg "SORTING CHECK"
  goDown
  texts <- getItemTexts
  getURL >>= getHashFromURL >>= checkHash texts
  where
  checkHash :: List String -> Routes -> Check Unit
  checkHash texts (Salted sort search salt) = do
    config <- getConfig
    sortButton <- getElementByCss config.sort.button "there is no sort button"
    actions $ click leftButton sortButton
    fileComponentLoaded
    nTexts <- getItemTexts
    if reverse nTexts == texts
      then successMsg "OK, sort works"
      else errorMsg "Sorting doesn't work"
  checkHash _ _ = errorMsg "need additional redirects in sorting"


fileUpload :: Check Unit
fileUpload = do
  sectionMsg "FILE UPLOAD"
  goDown
  config <- getConfig

  successMsg "went down"
  uploadInput <- getElementByCss config.upload.input "There is no upload input"
  oldItems <- S.fromList <$> getItemTexts
  script """
  var els = document.getElementsByTagName('i');
  for (var i = 0; i < els.length; i++) {
    if (/hidden-file-input/.test(els[i].className)) {
      els[i].className = "";
    }
  }
  """

  keys config.upload.filePath uploadInput
  waitCheck awaitInNotebook config.selenium.waitTime
  successMsg "Ok, explore notebook created"
  back
  fileComponentLoaded

  items <- S.fromList <$> getItemTexts
  if S.isEmpty $ S.difference items oldItems
    then errorMsg "items has not changed after upload"
    else successMsg "new items added after upload"

shareFile :: Check Unit
shareFile = do
  sectionMsg "SHARE FILE"
  goDown
  config <- getConfig
  uploadedItem <- getUploadedItem
  itemGetShareIcon uploadedItem >>= itemClickToolbarIcon uploadedItem
  waitCheck modalShown config.selenium.waitTime
  successMsg "Share modal dialog appeared"
  urlField <- getElementByCss config.share.urlField "there is no url field"
  urlValue <- attribute urlField "value"
  successMsg $ "Share url: " <> urlValue
  goTo urlValue
  waitCheck awaitInNotebook config.selenium.waitTime
  successMsg "Ok, share link led to notebook"

  where
    itemGetShareIcon :: Element -> Check Element
    itemGetShareIcon item =
      checkLocator shareLoc
        >>= child item
        >>= maybe (errorMsg "no share icon") pure

    shareLoc :: Element -> Check Element
    shareLoc el = getConfig >>= \config -> buttonLoc config.share.markShare el


searchForUploadedFile :: Check Unit
searchForUploadedFile = do
  sectionMsg "SEARCH"
  home
  config <- getConfig
  searchInput <- getElementByCss config.search.searchInput "no search input field"
  url <- getURL
  let filename = fromMaybe config.upload.file $ Arr.last $ Str.split "/" config.upload.file
  actions $ do
    leftClick searchInput
    sendKeys filename
  searchButton <- getElementByCss config.search.searchButton "no search button"
  actions $ leftClick searchButton
  waitCheck (awaitUrlChanged url) config.selenium.waitTime
  waitCheck (awaitItemShown filename) config.selenium.waitTime
  matchingItems <- filter (contains filename) <$> getItemTexts

  if null matchingItems
    then errorMsg "Failed searching for uploaded file"
    else successMsg "Searched for and found uploaded file"

  where
    contains :: String -> String -> Boolean
    contains phrase = R.test (R.regex phrase R.noFlags)

    awaitItemShown :: String -> Check Boolean
    awaitItemShown name = checker $ not <<< null <<< filter (contains name) <$> getItemTexts

awaitInNotebook :: Check Boolean
awaitInNotebook = checker $ do
  url <- getURL
  rgx <- nbRegex
  successMsg $ "URL: " ++ url
  pure $ R.test rgx url

  where
    nbRegex = do
      config <- getConfig
      let phrase = "notebook.html#/explore/" <> config.mount.name <> "/" <> config.database.name
      pure $ R.regex phrase R.noFlags




buttonLoc :: String -> Element -> Check Element
buttonLoc ty el = do
  config <- getConfig
  tpls <- css config.move.button >>= children el >>=
    traverse (\el -> Tuple el <$> (css ty >>= child el))
  maybe (throwError $ error $ "no such button " <> ty)
    pure $ foldl foldFn Nothing tpls
  where
    foldFn (Just el) _ = Just el
    foldFn Nothing (Tuple el Nothing) = Nothing
    foldFn Nothing (Tuple el (Just _)) = Just el

itemGetDeleteIcon :: Element -> Check Element
itemGetDeleteIcon item =
  checkLocator deleteLoc
    >>= child item
    >>= maybe (errorMsg "no delete icon") pure

 where
   deleteLoc :: Element -> Check Element
   deleteLoc el = getConfig >>= \config -> buttonLoc config.move.markDelete el


-- | Activate the item's toolbar and click a button/icon in it
itemClickToolbarIcon :: Element -> Element -> Check Unit
itemClickToolbarIcon item icon = do
  config <- getConfig
  actions $ leftClick item
  waitCheck (checker $ visible icon) config.selenium.waitTime
  actions $ leftClick icon

trashCheck :: Check Unit
trashCheck = do
  sectionMsg "TRASH CHECK"
  goDown
  assertBoolean "Trash must not be shown" <<< not =<< isTrashVisible
  successMsg "Trash is hidden"

  showHideButton <- getShowHideButton
  actions $ leftClick showHideButton

  config <- getConfig
  waitCheck (checker isTrashVisible) config.selenium.waitTime

  trashItem <- fromJust <$> findItem trashKey
  actions $ doubleClick leftButton trashItem
  fileComponentLoaded
  deletedItem <- findDeletedItem
  successMsg "Deleted item found"

  where
  isTrashVisible :: Check Boolean
  isTrashVisible =
    findItem trashKey
      >>= maybe (pure false) visible

  trashKey :: String
  trashKey = R.replace (R.regex "\\." R.noFlags{global=true}) "\\." SDCfg.trashFolder

  getShowHideButton :: Check Element
  getShowHideButton = do
    config <- getConfig
    toolbarButton config.toolbar.showHide
      >>= maybe (errorMsg "No show/hide button") pure

  findDeletedItem :: Check Element
  findDeletedItem = do
    config <- getConfig
    findItem config.move.other
      >>= maybe (errorMsg "Can't find deleted item") pure

createFolder :: Check Unit
createFolder = do
  sectionMsg "NEW FOLDER CHECK"
  goDown
  config <- getConfig

  newFolderButton <- getNewFolderButton
  actions $ leftClick newFolderButton
  folder <- waitUntilJust (findItem SDCfg.newFolderName) config.selenium.waitTime
  actions $ doubleClick leftButton folder
  fileComponentLoaded

  getURL >>= getHashFromURL >>= checkHash

  where

  getNewFolderButton :: Check Element
  getNewFolderButton = do
    config <- getConfig
    toolbarButton config.toolbar.newFolder
      >>= maybe (errorMsg "No create folder button") pure

  getNewFolder :: Check Element
  getNewFolder =
    findItem SDCfg.newFolderName
      >>= maybe (errorMsg "new folder has not been created") pure

  checkHash :: Routes -> Check Unit
  checkHash (Salted sort search salt) = do
    config <- getConfig
    let expectedPath = "/" <> config.mount.name <> "/" <> config.database.name <> "/" <> SDCfg.newFolderName <> "/"
    let actualPath = searchPath search
    if actualPath == (Just expectedPath)
      then successMsg "ok, hash correct"
      else errorMsg $
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
  actions <<< leftClick =<< getNewNotebookButton
  attempt (waitCheck notebookCheck config.selenium.waitTime)
    >>= either (\_ -> errorMsg "no redirect to notebook") (\_ -> successMsg "ok, notebook created")
  andThen
  where
  getNewNotebookButton :: Check Element
  getNewNotebookButton = do
    config <- getConfig
    toolbarButton config.toolbar.newNotebook
      >>= maybe (errorMsg "No create notebook button") pure

  notebookCheck :: Check Boolean
  notebookCheck = checker $ R.test (R.regex "notebook.html" R.noFlags) <$> getURL

createNotebook :: Check Unit
createNotebook = do
  sectionMsg "NEW NOTEBOOK CHECK"
  createNotebookAndThen do
    back
    fileComponentLoaded
    newNotebook <- getNewNotebook
    successMsg "OK, new notebook found in parent directory"

  where
  getNewNotebook :: Check Element
  getNewNotebook =
    findItem SDCfg.newNotebookName
      >>= maybe (errorMsg "No notebook in parent directory") pure



checkTitle :: Check Unit
checkTitle = do
  driver <- getDriver
  config <- getConfig
  windowTitle <- title
  if Str.contains config.version windowTitle
    then successMsg "Title contains version"
    else errorMsg $ "Title (" ++ windowTitle ++ ") doesn't contain version"


moveDelete :: String -> Check Unit -> String -> String -> Check Unit
moveDelete msg setUp src tgt = do
  sectionMsg $ "check move/delete " <> msg
  setUp
  config <- getConfig
  item <- findItem src
          >>= maybe (errorMsg $ "there is no source " <> msg) pure
  itemGetMoveIcon item >>= itemClickToolbarIcon item
  waitCheck modalShown config.selenium.waitTime
  getElementByCss config.move.nameField "no rename field"
    >>= editNameField

  getElementByCss config.move.submit "no submit button"
    >>= actions <<< leftClick

  renamed <- waitUntilJust (findItem tgt) config.selenium.waitTime
  successMsg $ "ok, successfully renamed (" <> msg <> ")"
  
  itemGetDeleteIcon renamed >>= itemClickToolbarIcon renamed
  waitCheck (checker $ isNothing <$> findItem tgt) config.selenium.waitTime
  successMsg $ "ok, successfully deleted (" <> msg <> ")"
  where
  itemGetMoveIcon :: Element -> Check Element
  itemGetMoveIcon item =
    checkLocator moveLoc
      >>= child item
      >>= maybe (errorMsg "no move/rename icon") pure

  moveLoc :: Element -> Check Element
  moveLoc el = getConfig >>= \config -> buttonLoc config.move.markMove el
  
  editNameField :: Element -> Check Unit
  editNameField nameField = do
    config <- getConfig
    actions do
      leftClick nameField
      sendSelectAll
      sendDelete
      sendKeys tgt

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


test :: Check Unit
test = do
  home
  spyXHR
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
  createNotebook
  moveDeleteNotebook
