module Test.Selenium where

import Prelude
import DOM (DOM())
import Control.Bind ((>=>))
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
import Data.Foldable (traverse_, foldl, elem, find)
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

checkElements :: Check Unit
checkElements = do
  config <- getConfig
  traverse_ traverseFn $ SM.toList config.locators
  successMsg "all elements here, page is loaded"
  where
  traverseFn :: Tuple String String -> Check Unit
  traverseFn (Tuple key selector) = do
    driver <- getDriver
    css selector >>= element >>= checkMsg key

checkMsg :: String -> Maybe _ -> Check Unit
checkMsg msg Nothing =
  errorMsg $ msg <> " not found"
checkMsg _ _ = pure unit

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
  css config.toolbar.main >>= element >>= maybe noToolbar \toolbar ->
    checkLocator (locator key) >>= child toolbar
  where
  noToolbar = errorMsg "no toolbar"
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

checkItemToolbar :: Check Unit
checkItemToolbar = do
  sectionMsg "CHECK ITEM TOOLBAR"
  home
  config <- getConfig
  css config.item.main >>= element >>= maybe errParent goParent
  where
  errParent = errorMsg "there is no list-group-item"
  goParent el = do
    getConfig >>= (\config -> css config.item.toolbar) >>=
    child el >>= maybe errToolbar (goToolbar el)

  errToolbar = errorMsg "there is no toolbar in list-group-item"
  goToolbar el toolbar = do
    apathize do
      displayed <- visible toolbar
      if displayed
        then errorMsg "toolbar should not be displayed"
        else do
        successMsg "toolbar is hidden"
        actions do
          hover el
        displayedHovered <- visible toolbar
        if displayedHovered
          then successMsg "hovered toolbar is visible"
          else errorMsg "hovered toolbar is not visible"
    apathize do
      style <- getCss el "background-color"
      actions $ leftClick el
      newStyle <- getCss el "background-color"
      if newStyle == style
        then errorMsg "background-color has not been changed after click"
        else successMsg "bacground-color has been changed after click"

checkURL :: Check Unit
checkURL = do
  sectionMsg "CHECKING URL"
  home
  url <- getURL
  either errHash goHash $ matchHash routing $ dropHash url
  where
  errHash _ = errorMsg "incorrect hash"
  goHash (Salted sort search salt) = do
    successMsg "hash is correct"
    if searchPath search == Just "/"
      then successMsg "search path is correct"
      else errorMsg "incorrect search path"
  goHash _ =
    errorMsg "need additional redirects"


goDown :: Check Unit
goDown = do
  sectionMsg "CHECKING GO DOWN"
  home
  url <- getURL
  findTestDb >>= maybe noTestDb (goTestDb url)
  where
  noTestDb = errorMsg "no test db"
  goTestDb url el =
    either incorrectOldHash (goOldHash url el) $ matchHash routing (dropHash url)
  incorrectOldHash _ = errorMsg "incorrect initial hash in goDown"
  goOldHash url el old@(Salted oldSort oldSearch oldSalt) = do
    actions $ doubleClick leftButton el
    config <- getConfig
    waitCheck (changed url) config.selenium.waitTime
    loaded
    newUrl <- getURL
    either errHash (goHash old) $ matchHash routing (dropHash newUrl)
  goOldHash _ _ _ = errorMsg "weird initial hash in goDown"

  errHash _ = errorMsg "incorrect has after goDown"
  goHash (Salted oldSort oldSearch oldSalt) (Salted sort search salt) = do
    config <- getConfig
    if (oldSalt == salt) &&
       (oldSort == sort) &&
       ((searchPath search) == (Just $ "/" <> config.database.name <> "/"))
      then successMsg "correct hash after goDown"
      else errorMsg $ "incorrect hash after goDown " <> (fromMaybe "" $ searchPath search)
  goHash _ _ = do
    errorMsg "weird hash after goDown"

  changed oldUrl = do
    url <- getURL
    if url == oldUrl
      then changed oldUrl
      else pure true


breadcrumbs :: Check Unit
breadcrumbs = do
  sectionMsg "BREADCRUMBS"
  home
  texts <- bTexts
  config <- getConfig
  if not $ elem config.breadcrumbs.home texts
    then errorMsg "incorrect root breadcrumb"
    else do
    successMsg "correct root breadcrumb"
    goDown
    nTexts <- bTexts
    if not $ elem config.breadcrumbs.home texts
      then errorMsg "incorrect root breadcrumb after goDown"
      else if not $ elem config.database.name nTexts
           then errorMsg "breadcrumbs are not updated"
           else do
             successMsg "breadcrumbs are updated"
             as <- anchors
             mbEl <- foldl foldMFn (pure Nothing) as
             maybe errHome goHome mbEl

  where
  errHome = errorMsg "There is no Home breadcrumb: since you have already checked that it exists, there is probably error in test"
  goHome el = do
    actions $ leftClick el
    loaded
    checkURL
    successMsg "Ok, went home after click on root breadcrumb"
  anchors :: Check (List Element)
  anchors = do
    config <- getConfig
    css config.breadcrumbs.main >>= element >>= maybe err go
    where
    err = errorMsg "There is no breadcrumbs"
    go bs =
      getConfig >>= \config -> css config.breadcrumbs.text >>= children bs

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
  url <- getURL
  either errHash (goHash texts) $ matchHash routing $ dropHash url
  where

  errHash _ = errorMsg "Incorrect hash"
  goHash texts (Salted sort search salt) = do
    config <- getConfig
    css config.sort.button >>= element >>=
      maybe errNoSortButton (goSortButton texts sort)
  goHash _ _ = errorMsg "need addtional redirects in sorting"

  errNoSortButton = errorMsg "there is no sort button"
  goSortButton texts sort el = do
    actions $ click leftButton el
    loaded
    nTexts <- getItemTexts
    if reverse nTexts == texts
      then successMsg "OK, sort works"
      else errorMsg "Sorting doesn't work"


fileUpload :: Check Unit
fileUpload = do
  sectionMsg "FILE UPLOAD"
  goDown
  config <- getConfig
  css config.upload.input >>= element >>= maybe errNoInput goInput
  where
  errNoInput = errorMsg "There is no upload input"
  goInput el = do
    config <- getConfig
    oldItems <- S.fromList <$> getItemTexts
    script """
    var els = document.getElementsByTagName('i');
    for (var i = 0; i < els.length; i++) {
      if (/hidden-file-input/.test(els[i].className)) {
        els[i].className = "";
      }
    }
    """
    keys config.upload.filePath el
    waitCheck inNotebook config.selenium.waitTime
    successMsg "Ok, explore notebook created"
    back
    loaded
    items <- S.fromList <$> getItemTexts
    if S.isEmpty $ S.difference items oldItems
      then errorMsg "items has not changed after upload"
      else successMsg "new items added after upload"



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
  findUploaded >>= maybe errUploaded goUploaded
  where
  errUploaded = errorMsg "File has not been uploaded"
  goUploaded el = do
    config <- getConfig
    checkLocator moveLoc >>= child el >>= maybe errIcon (goIcon el)
  errIcon = errorMsg "no move/rename icon"
  goIcon el icon = do
    config <- getConfig
    actions do
      leftClick el
    waitCheck (checker $ visible icon) config.selenium.waitTime
    actions do
      leftClick icon
    waitCheck modalShown config.selenium.waitTime
    css config.move.nameField >>= element >>= maybe errNoInput goInput
  modalShown = do
    config <- getConfig
    vis <- css config.modal >>= element >>= maybe (pure false) visible
    if vis
      then pure true
      else later 1000 modalShown

  errNoInput = errorMsg "no rename field"
  goInput el = do
    config <- getConfig
    actions do
      leftClick el
      keyDown commandKey
      sendKeys "a"
      keyUp commandKey
      sendKeys $ Str.fromChar $ Ch.fromCharCode 57367
      sendKeys config.move.other
    css config.move.submit >>= element >>= maybe errNoSubmit goSubmit

  errNoSubmit = errorMsg "no submit button"
  goSubmit button = do
    config <- getConfig
    actions do
      leftClick button
    waitCheck (later 3000 $ pure false) 5000
    findItem config.move.other >>= maybe errRename goRename

  errRename = errorMsg "not renamed"
  goRename el = do
    successMsg "successfully renamed"
    checkLocator deleteLoc >>= child el >>= maybe errDelete (goDelete el)

  errDelete = errorMsg "there is no delete icon"
  goDelete el icon = do
    config <- getConfig
    actions do
      leftClick el
    waitCheck (checker $ visible icon) config.selenium.waitTime
    actions do
      leftClick icon
    waitCheck (later 3000 $ pure false) 5000
    findItem config.move.other >>= maybe okDeleted (const errDeleted)

  errDeleted = errorMsg "item has not been deleted"
  okDeleted = do
    successMsg "item has been deleted"

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
  visible <- isTrashVisible
  if visible
    then errorMsg "Trash is shown"
    else do
    config <- getConfig
    successMsg "Trash is hidden"
    toolbarButton config.toolbar.showHide >>= maybe errShowHide goShowHide

  where
  isTrashVisible = findItem trashKey >>= maybe (pure false) visible

  trashKey = R.replace (R.regex "\\." R.noFlags{global=true}) "\\." SDCfg.trashFolder

  errShowHide = errorMsg "No show/hide button"
  goShowHide btn = do
    actions do
      leftClick btn
    waitCheck (later 1000 $ pure true) 2000
    visible <- isTrashVisible
    if not visible
      then errorMsg "Trash should be visible now"
      else do
      successMsg "Trash is visible"
      config <- getConfig
      trash <- fromJust <$> findItem trashKey
      actions do
        doubleClick leftButton trash
      loaded
      findItem config.move.other >>= maybe errNoDeleted goDeleted

  errNoDeleted = errorMsg "Can't find deleted item"
  goDeleted _ = successMsg "Deleted item found"


createFolder :: Check Unit
createFolder = do
  sectionMsg "NEW FOLDER CHECK"
  goDown
  config <- getConfig
  toolbarButton config.toolbar.newFolder >>= maybe errButton goButton
  where
  errButton = errorMsg "No create folder button"
  goButton btn = do
    config <- getConfig
    actions $ leftClick btn
    waitCheck (later 1000 $ pure true) config.selenium.waitTime
    findItem SDCfg.newFolderName >>= maybe errNoNewFolder goNewFolder

  errNoNewFolder = errorMsg "new folder has not been created"
  goNewFolder folder = do
    actions $ doubleClick leftButton folder
    loaded
    url <- getURL
    either errHash goHash $ matchHash routing $ dropHash url

  errHash _ = errorMsg "incorrect hash"

  goHash (Salted sort search salt) = do
    config <- getConfig
    let expectedPath = "/" <> config.database.name <> "/" <> SDCfg.newFolderName <> "/"
    if (searchPath search) == (Just expectedPath)
      then successMsg "ok, hash correct"
      else errorMsg "hash incorrect in created folder"
  goHash _ = errorMsg "incorrect hash"


createNotebook :: Check Unit
createNotebook = do
  sectionMsg "NEW NOTEBOOK CHECK"
  goDown
  config <- getConfig
  toolbarButton config.toolbar.newNotebook >>= maybe errButton goButton
  where
  errButton = errorMsg "No create notebook button"
  goButton btn = do
    config <- getConfig
    actions $ leftClick btn
    e <- attempt $ waitCheck notebookCheck config.selenium.waitTime
    case e of
      Left _ -> errorMsg "no redirect to notebook"
      Right _ -> do
        successMsg "ok, notebook created"
        back
        loaded
        findItem SDCfg.newNotebookName >>= maybe errNoNotebook goNotebook

  errNoNotebook = errorMsg "No notebook in parent directory"
  goNotebook _ = successMsg "OK, new notebook found in parent directory"
  notebookCheck = do
    url <- getURL
    if R.test (R.regex "notebook.html" R.noFlags) url
      then pure true
      else later 1000 notebookCheck

loaded :: Check Unit
loaded = do
  driver <- getDriver
  config <- getConfig
  waitCheck checkEls config.selenium.waitTime
  where
  checkEls = do
    res <- attempt $ checkElements
    if isLeft res
      then later 1000 $ checkEls
      else pure true

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
      checkItemToolbar
      checkURL
      goDown
      breadcrumbs
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

dropHash :: String -> String
dropHash h = R.replace (R.regex "^[^#]*#" R.noFlags) "" h
