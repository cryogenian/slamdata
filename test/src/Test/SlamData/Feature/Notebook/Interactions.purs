module Test.SlamData.Feature.Notebook.Interactions where

import Control.Apply ((<*), (*>)) -- <------------ remove <*s
import Control.Bind ((<=<), (=<<))
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Random (randomInt)
import Data.Foldable (traverse_) as F
import Data.Tuple (Tuple(..))
import Data.Maybe (Maybe(..))
import Data.List (replicateM)
import Data.Traversable (traverse) as T
import Prelude
import Selenium.ActionSequence (leftClick)
import Selenium.Monad (get, refresh)
import Selenium.Types (Element())
import Test.Feature (click, clickWithProperties, hover, pressEnter, typeString, selectAll, provideFieldValue, provideFieldValueWithProperties, selectFromDropdown, expectPresentedWithProperties)
import Test.SlamData.Feature.Monad (SlamFeature())
import Test.SlamData.Feature.Common (waitTime)
import Test.SlamData.Feature.XPaths as XPaths
import Test.SlamData.Feature.Properties as Properties
import XPath as XPath

launchSlamData :: SlamFeature Unit
launchSlamData = get "http://localhost:63175"

mountTestDatabase :: SlamFeature Unit
mountTestDatabase =
  click (XPath.anywhere XPaths.accessMountDatabase)
    *> provideFieldValue (XPath.anywhere XPaths.mountName) "test-mount"
    *> selectFromDropdown (XPath.anywhere XPaths.mountType) "Mongo"
    *> provideFieldValue (XPath.index (XPath.anywhere XPaths.mountPort) 1) "63174"
    *> provideFieldValue (XPath.index (XPath.anywhere XPaths.mountHost) 1) "localhost"
    *> provideFieldValue (XPath.anywhere XPaths.mountDatabase) "testDb"
    *> click (XPath.anywhere XPaths.mountButton)

browseFolder :: String -> SlamFeature Unit
browseFolder = click <<< XPath.anywhere <<< XPath.anyWithExactText

embedCellOutput :: SlamFeature Unit
embedCellOutput = click $ XPath.anywhere XPaths.embedCellOutput

browseRootFolder :: SlamFeature Unit
browseRootFolder = click XPaths.browseRootFolder

browseTestFolder :: SlamFeature Unit
browseTestFolder = browseRootFolder *> browseFolder "test-mount" *> browseFolder "testDb"

createNotebook :: SlamFeature Unit
createNotebook = click XPaths.createNotebook

nameNotebook :: String -> SlamFeature Unit
nameNotebook =
  provideFieldValueWithProperties [Tuple "value" $ Just "Untitled Notebook"] (XPath.anywhere "input")

deleteFile :: String -> SlamFeature Unit
deleteFile = click <<< XPaths.removeFile

createNotebookInTestFolder :: String -> SlamFeature Unit
createNotebookInTestFolder name = browseTestFolder *> createNotebook *> nameNotebook name

deleteFileInTestFolder :: String -> SlamFeature Unit
deleteFileInTestFolder name = browseTestFolder *> deleteFile name

reopenCurrentNotebook :: SlamFeature Unit
reopenCurrentNotebook = waitTime 2000 *> refresh

expandNewCellMenu :: SlamFeature Unit
expandNewCellMenu = click XPaths.insertCell

insertQueryCellUsingNextActionMenu :: SlamFeature Unit
insertQueryCellUsingNextActionMenu = expandNewCellMenu *> click XPaths.insertQueryCell

insertMdCellUsingNextActionMenu :: SlamFeature Unit
insertMdCellUsingNextActionMenu = expandNewCellMenu *> click XPaths.insertMdCell

insertExploreCellUsingNextActionMenu :: SlamFeature Unit
insertExploreCellUsingNextActionMenu = expandNewCellMenu *> click XPaths.insertExploreCell

insertSearchCellUsingNextActionMenu :: SlamFeature Unit
insertSearchCellUsingNextActionMenu =  expandNewCellMenu *> click XPaths.insertSearchCell

--insertRandomNumberOfCells :: SlamFeature Unit -> SlamFeature Int
--insertRandomNumberOfCells insertCell = do
--  numberOfCellsToInsert <- liftEff $ randomInt 1 10
--  replicateM numberOfCellsToInsert insertCell
--  pure numberOfCellsToInsert
--
--insertRandomNumberOfQueryCells :: SlamFeature Int
--insertRandomNumberOfQueryCells = insertRandomNumberOfCells insertQueryCellUsingNextActionMenu
--
--insertRandomNumberOfMdCells :: SlamFeature Int
--insertRandomNumberOfMdCells = insertRandomNumberOfCells insertMdCellUsingNextActionMenu
--
--insertRandomNumberOfExploreCells :: SlamFeature Int
--insertRandomNumberOfExploreCells = insertRandomNumberOfCells insertExploreCellUsingNextActionMenu
--
--insertRandomNumberOfSearchCells :: SlamFeature Int
--insertRandomNumberOfSearchCells = insertRandomNumberOfCells insertSearchCellUsingNextActionMenu
--
---- Finds at least 1 cell and deletes it.
--deleteAllCells :: SlamFeature Unit
--deleteAllCells = Finders.findAllDeleteCellOptions >>= F.traverse_ click
--
---- Deletes any cells that there are.
--deleteAnyCells :: SlamFeature Unit
--deleteAnyCells = Finders.findAnyDeleteCellOptions >>= F.traverse_ click
--
--showQueryCellOptions :: SlamFeature Unit
--showQueryCellOptions = Finders.findShowQueryCellOptions >>= click
--
--hideQueryCellOptions :: SlamFeature Unit
--hideQueryCellOptions = Finders.findHideQueryCellOptions >>= click
--
--showMdCellOptions :: SlamFeature Unit
--showMdCellOptions = Finders.findShowMdCellOptions >>= click
--
--hideMdCellOptions :: SlamFeature Unit
--hideMdCellOptions = Finders.findHideMdCellOptions >>= click
--
--showFileList :: SlamFeature Unit
--showFileList = Finders.findShowFileList >>= click
--
--hideFileList :: SlamFeature Unit
--hideFileList = Finders.findHideFileList >>= click
--
--selectFileFromInitialFileList :: String -> SlamFeature Unit
--selectFileFromInitialFileList = click <=< Finders.findFileFromFileList
--
--showExploreCellOptions :: SlamFeature Unit
--showExploreCellOptions = Finders.findShowExploreCellOptions >>= click
--
--hideExploreCellOptions :: SlamFeature Unit
--hideExploreCellOptions = Finders.findHideExploreCellOptions >>= click
--
--showSearchCellOptions :: SlamFeature Unit
--showSearchCellOptions = Finders.findShowSearchCellOptions >>= click
--
--hideSearchCellOptions :: SlamFeature Unit
--hideSearchCellOptions = Finders.findHideSearchCellOptions >>= click
--
--markdownQueryTitleXPath :: String
--markdownQueryTitleXPath = XPaths.mdCellTitleXPath `following` XPaths.queryCellTitleXPath
--
--provideExploreFile :: String -> SlamFeature Unit
--provideExploreFile filename = focusExploreFileField *> typeString filename

provideMd :: String -> SlamFeature Unit
provideMd md = typeString (md ++ " ") <* focusMdField

focusMdField :: SlamFeature Unit
focusMdField = click XPaths.mdField

--focusExploreFileField :: SlamFeature Unit
--focusExploreFileField = Finders.findExploreFileField >>= click

changeMd :: String -> SlamFeature Unit
changeMd md = typeString md <* selectAll <* focusMdField

playMd :: SlamFeature Unit
playMd = click XPaths.mdPlayButton

--playExplore :: SlamFeature Unit
--playExplore = Finders.findExplorePlayButton >>= click
--
playMdQuery :: SlamFeature Unit
playMdQuery = click XPaths.mdQueryPlayButton
--
--focusMdQueryField :: SlamFeature Unit
--focusMdQueryField = Finders.findMdQueryField >>= click
--
--provideMdQuery :: String -> SlamFeature Unit
--provideMdQuery query = focusMdQueryField *> typeString (query ++ " ")

insertQueryAfterMd :: SlamFeature Unit
insertQueryAfterMd = click XPaths.insertQueryAfterMd

--showExploreMessages :: SlamFeature Unit
--showExploreMessages = Finders.findShowExploreMessages >>= click
