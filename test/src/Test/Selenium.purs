module Test.Selenium where

import Prelude
import DOM (DOM())
import Control.Bind ((>=>), (=<<))
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (log, CONSOLE())
import Control.Monad.Eff.Exception (error)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Reader.Trans
import Control.Monad.Reader.Class
import Control.Monad.Trans
import Data.Tuple (Tuple(..))
import Data.Maybe (Maybe(..), maybe, fromMaybe, isJust)
import Data.Maybe.Unsafe (fromJust)
import Data.Either (Either(..), either, isLeft)
import Data.Foldable (foldl, elem, find)
import Data.Traversable (traverse)
import Data.List (List(), reverse)
import Text.Chalk
import Selenium
import Selenium.Types
import Selenium.MouseButton
import Selenium.ActionSequence
import Selenium.Key
import Selenium.Browser
import Selenium.Builder
import Utils.Log
import Test.Config
import qualified Control.Monad.Aff as A
import qualified Control.Monad.Aff.Console as AC
import Driver.File.Routing (Routes(..), routing)
import Driver.File.Search (searchPath)
import Routing (matchHash)
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

foreign import makePublic :: forall a eff. String -> a -> Eff (module :: MODULE | eff) Unit

main = do
  makePublic "test" test

home :: Check Unit
home = do
  getConfig >>= goTo <<< _.slamdataUrl
  loaded

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

findUploaded :: Check (Maybe Element)
findUploaded = do
  config <- getConfig
  findItem config.move.name


checkMountedDatabase :: Check Unit
checkMountedDatabase = do
  sectionMsg "CHECK TEST DATABASE IS MOUNTED"
  home
  mbTestDb <- findTestDb
  maybe error success mbTestDb
  where
  error = errorMsg "There is no test database"
  success _ = successMsg "test database found"

checkConfigureMount :: Check Unit
checkConfigureMount = do
  sectionMsg "CHECK CONFIGURE MOUNT DIALOG"
  home

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
    keyDown commandKey
    sendKeys "a"
    keyUp commandKey
    sendKeys "hello"
    keyDown commandKey
    sendKeys "z"
    keyUp commandKey

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
  home
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


goDown :: Check Unit
goDown = do
  sectionMsg "CHECKING GO DOWN"
  home
  url <- getURL
  testDb <- getTestDb
  oldHash <- either (const $ errorMsg "incorrect initial hash in goDown") pure $ matchHash routing (dropHash url)
  checkOldHash url testDb oldHash

  where

  getTestDb = findTestDb >>= maybe (errorMsg "no test db") pure

  checkOldHash url el old@(Salted oldSort oldSearch oldSalt) = do
    actions $ doubleClick leftButton el
    config <- getConfig
    waitCheck (changed url) config.selenium.waitTime
    loaded
    getURL >>= getHashFromURL >>= checkHashes old
  checkOldHash _ _ _ = errorMsg "weird initial hash in goDown"

  checkHashes (Salted oldSort oldSearch oldSalt) (Salted sort search salt) = do
    config <- getConfig
    if (oldSalt == salt) &&
       (oldSort == sort) &&
       ((searchPath search) == (Just $ "/" <> config.database.name <> "/"))
      then successMsg "correct hash after goDown"
      else errorMsg $ "incorrect hash after goDown " <> (fromMaybe "" $ searchPath search)
  checkHashes _ _ = do
    errorMsg "weird hash after goDown"

  changed oldUrl = do
    url <- getURL
    if url == oldUrl
      then changed oldUrl
      else pure true


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
  loaded
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
    loaded
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
  waitCheck inNotebook config.selenium.waitTime
  successMsg "Ok, explore notebook created"
  back
  loaded

  items <- S.fromList <$> getItemTexts
  if S.isEmpty $ S.difference items oldItems
    then errorMsg "items has not changed after upload"
    else successMsg "new items added after upload"

  where
  inNotebook = do
    url <- getURL
    rgx <- nbRegex
    if R.test rgx url
      then pure true
      else later 1000 inNotebook

  nbRegex = do
    config <- getConfig
    pure $ R.regex ("notebook.html#/explore/" <>
                    config.database.name) R.noFlags



moveDelete :: Check Unit
moveDelete = do
  sectionMsg "MOVE DELETE"
  goDown

  getUploadedItem
    >>= checkMove
    >>= checkDelete

  where
  errUploaded = errorMsg "File has not been uploaded"

  getUploadedItem :: Check Element
  getUploadedItem =
    findUploaded
      >>= maybe (errorMsg "File has not been uploade") pure

  -- | Move an item, and return the new/moved item
  checkMove :: Element -> Check Element
  checkMove item = do
    config <- getConfig
    itemGetMoveIcon item >>= itemClickToolbarIcon item
    waitCheck modalShown config.selenium.waitTime

    getElementByCss config.move.nameField "no rename field"
      >>= editNameField

    getElementByCss config.move.submit "no submit button"
      >>= actions <<< leftClick

    waitCheck (later 3000 $ pure false) 5000
    renamedItem <- findItem config.move.other >>= maybe (errorMsg "not renamed") pure
    successMsg "successfully renamed"
    pure renamedItem

    where
      -- | Type a new name into the "name" field
      editNameField :: Element -> Check Unit
      editNameField nameField = do
        config <- getConfig
        actions do
          leftClick nameField
          keyDown commandKey
          sendKeys "a"
          keyUp commandKey
          sendKeys $ Str.fromChar $ Ch.fromCharCode 57367
          sendKeys config.move.other

      -- | Get an item's "move/rename" icon
      itemGetMoveIcon :: Element -> Check Element
      itemGetMoveIcon item =
        checkLocator moveLoc
          >>= child item
          >>= maybe (errorMsg "no move/rename icon") pure

  checkDelete :: Element -> Check Unit
  checkDelete item = do
    config <- getConfig
    itemGetDeleteIcon item >>= itemClickToolbarIcon item
    waitCheck (later 3000 $ pure false) 5000
    findItem config.move.other
      >>= maybe (pure unit) (const $ errorMsg "not deleted")
    successMsg "successfully deleted"

    where
      itemGetDeleteIcon :: Element -> Check Element
      itemGetDeleteIcon item =
        checkLocator deleteLoc
          >>= child item
          >>= maybe (errorMsg "no delete icon") pure

  -- | Activate the item's toolbar and click a button/icon in it
  itemClickToolbarIcon :: Element -> Element -> Check Unit
  itemClickToolbarIcon item icon = do
    config <- getConfig
    actions $ leftClick item
    waitCheck (checker $ visible icon) config.selenium.waitTime
    actions $ leftClick icon

  moveLoc :: Element -> Check Element
  moveLoc el = getConfig >>= \config -> buttonLoc config.move.markMove el

  deleteLoc :: Element -> Check Element
  deleteLoc el = getConfig >>= \config -> buttonLoc config.move.markDelete el

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

trashCheck :: Check Unit
trashCheck = do
  sectionMsg "TRASH CHECK"
  goDown
  assertBoolean "Trash must not be shown" <<< not =<< isTrashVisible
  successMsg "Trash is hidden"

  showHideButton <- getShowHideButton
  actions $ leftClick showHideButton
  waitCheck (later 1000 $ pure true) 2000
  assertBoolean "Trash should be shown" =<< isTrashVisible

  trashItem <- fromJust <$> findItem trashKey
  actions $ doubleClick leftButton trashItem
  loaded
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
  waitCheck (later 1000 $ pure true) config.selenium.waitTime

  folder <- getNewFolder
  actions $ doubleClick leftButton folder
  loaded

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
    let expectedPath = "/" <> config.database.name <> "/" <> SDCfg.newFolderName <> "/"
    if (searchPath search) == (Just expectedPath)
      then successMsg "ok, hash correct"
      else errorMsg "hash incorrect in created folder"
  checkHash _ = errorMsg "incorrect hash"


createNotebook :: Check Unit
createNotebook = do
  sectionMsg "NEW NOTEBOOK CHECK"
  goDown
  config <- getConfig
  actions <<< leftClick =<< getNewNotebookButton
  attempt (waitCheck notebookCheck config.selenium.waitTime)
    >>= either (\_ -> errorMsg "no redirect to notebook") (\_ -> successMsg "ok, notebook created")

  back
  loaded
  newNotebook <- getNewNotebook
  successMsg "OK, new notebook found in parent directory"

  where
  getNewNotebook :: Check Element
  getNewNotebook =
    findItem SDCfg.newNotebookName
      >>= maybe (errorMsg "No notebook in parent directory") pure

  getNewNotebookButton :: Check Element
  getNewNotebookButton = do
    config <- getConfig
    toolbarButton config.toolbar.newNotebook
      >>= maybe (errorMsg "No create notebook button") pure

  notebookCheck :: Check Boolean
  notebookCheck = do
    url <- getURL
    if R.test (R.regex "notebook.html" R.noFlags) url
      then pure true
      else later 1000 notebookCheck

-- TODO: Test the version in the nav bar
title :: Check Unit
title = do
  driver <- getDriver
  config <- getConfig
  windowTitle <- lift $ getTitle driver
  if Str.contains config.version windowTitle
    then successMsg "Title contains version"
    else errorMsg "Title doesn't contain version"

test :: Config -> A.Aff _ Unit
test config =
  maybe error go $ str2browser config.selenium.browser
  where
  error = void $ AC.log $ red "Incorrect browser"
  go br = do
    AC.log $ yellow $ config.selenium.browser <> " setted as browser for tests\n\n"
    driver <- build $ browser br
    res <- A.attempt $ flip runReaderT {config: config, driver: driver} do
      home
      checkMountedDatabase
      checkConfigureMount
      checkItemToolbar
      checkURL
      goDown
      checkBreadcrumbs
      sorting
      fileUpload
      moveDelete
      trashCheck
      title
      createFolder
      createNotebook
    case res of
      Left e -> do
        quit driver
        throwError e
      Right _ ->
        quit driver

