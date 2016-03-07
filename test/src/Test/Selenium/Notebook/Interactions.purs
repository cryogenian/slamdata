module Test.Selenium.Notebook.Interactions where

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
import Test.Selenium.Feature (click, clickWithProperties, hover, pressEnter, typeString, selectAll, provideFieldValue, selectFromDropdown, expectPresentedWithProperties)
import Test.Selenium.Monad (Check())
import Test.Selenium.Common (waitTime)
import Test.XPath as XPath
import Test.Selenium.XPaths as XPaths
import Test.Selenium.Properties as Properties
import Debug.Trace

launchSlamData :: Check Unit
launchSlamData = get "http://localhost:63175"

mountTestDatabase :: Check Unit
mountTestDatabase =
  click (XPath.anywhere XPaths.accessMountDatabase)
    *> provideFieldValue (XPath.anywhere XPaths.mountName) "test-mount"
    *> selectFromDropdown (XPath.anywhere XPaths.mountType) "Mongo"
    *> provideFieldValue (XPath.index (XPath.anywhere XPaths.mountPort) 1) "63174"
    *> provideFieldValue (XPath.index (XPath.anywhere XPaths.mountHost) 1) "localhost"
    *> provideFieldValue (XPath.anywhere XPaths.mountDatabase) "testDb"
    *> click (XPath.anywhere XPaths.mountButton)

browseFolder :: String -> Check Unit
browseFolder = click <<< XPath.anywhere <<< XPath.anyWithExactText

embedCellOutput :: Check Unit
embedCellOutput = click $ XPath.anywhere XPaths.embedCellOutput

browseRootFolder :: Check Unit
browseRootFolder = click XPaths.browseRootFolder

browseTestFolder :: Check Unit
browseTestFolder = browseRootFolder *> browseFolder "test-mount" *> browseFolder "testDb"

createNotebook :: Check Unit
createNotebook = click XPaths.createNotebook

nameNotebook :: String -> Check Unit
nameNotebook =
  provideFieldValueWithProperties [Tuple "value" "Untitled Notebook"] (XPath.anywhere "input")

deleteFile :: String -> Check Unit
deleteFile = click <<< XPaths.removeFile

createNotebookInTestFolder :: String -> Check Unit
createNotebookInTestFolder name = browseTestFolder *> createNotebook *> nameNotebook name

deleteFileInTestFolder :: String -> Check Unit
deleteFileInTestFolder name = browseTestFolder *> deleteFile name

reopenCurrentNotebook :: Check Unit
reopenCurrentNotebook = waitTime 2000 *> refresh

expandNewCellMenu :: Check Unit
expandNewCellMenu = click XPaths.insertCell

insertQueryCellUsingNextActionMenu :: Check Unit
insertQueryCellUsingNextActionMenu = expandNewCellMenu *> click XPaths.insertQueryCell

insertMdCellUsingNextActionMenu :: Check Unit
insertMdCellUsingNextActionMenu = expandNewCellMenu *> click XPaths.insertMdCell

insertExploreCellUsingNextActionMenu :: Check Unit
insertExploreCellUsingNextActionMenu = expandNewCellMenu *> click XPaths.insertExploreCell

insertSearchCellUsingNextActionMenu :: Check Unit
insertSearchCellUsingNextActionMenu =  expandNewCellMenu *> click XPaths.insertSearchCell

--insertRandomNumberOfCells :: Check Unit -> Check Int
--insertRandomNumberOfCells insertCell = do
--  numberOfCellsToInsert <- liftEff $ randomInt 1 10
--  replicateM numberOfCellsToInsert insertCell
--  pure numberOfCellsToInsert
--
--insertRandomNumberOfQueryCells :: Check Int
--insertRandomNumberOfQueryCells = insertRandomNumberOfCells insertQueryCellUsingNextActionMenu
--
--insertRandomNumberOfMdCells :: Check Int
--insertRandomNumberOfMdCells = insertRandomNumberOfCells insertMdCellUsingNextActionMenu
--
--insertRandomNumberOfExploreCells :: Check Int
--insertRandomNumberOfExploreCells = insertRandomNumberOfCells insertExploreCellUsingNextActionMenu
--
--insertRandomNumberOfSearchCells :: Check Int
--insertRandomNumberOfSearchCells = insertRandomNumberOfCells insertSearchCellUsingNextActionMenu
--
---- Finds at least 1 cell and deletes it.
--deleteAllCells :: Check Unit
--deleteAllCells = Finders.findAllDeleteCellOptions >>= F.traverse_ click
--
---- Deletes any cells that there are.
--deleteAnyCells :: Check Unit
--deleteAnyCells = Finders.findAnyDeleteCellOptions >>= F.traverse_ click
--
--showQueryCellOptions :: Check Unit
--showQueryCellOptions = Finders.findShowQueryCellOptions >>= click
--
--hideQueryCellOptions :: Check Unit
--hideQueryCellOptions = Finders.findHideQueryCellOptions >>= click
--
--showMdCellOptions :: Check Unit
--showMdCellOptions = Finders.findShowMdCellOptions >>= click
--
--hideMdCellOptions :: Check Unit
--hideMdCellOptions = Finders.findHideMdCellOptions >>= click
--
--showFileList :: Check Unit
--showFileList = Finders.findShowFileList >>= click
--
--hideFileList :: Check Unit
--hideFileList = Finders.findHideFileList >>= click
--
--selectFileFromInitialFileList :: String -> Check Unit
--selectFileFromInitialFileList = click <=< Finders.findFileFromFileList
--
--showExploreCellOptions :: Check Unit
--showExploreCellOptions = Finders.findShowExploreCellOptions >>= click
--
--hideExploreCellOptions :: Check Unit
--hideExploreCellOptions = Finders.findHideExploreCellOptions >>= click
--
--showSearchCellOptions :: Check Unit
--showSearchCellOptions = Finders.findShowSearchCellOptions >>= click
--
--hideSearchCellOptions :: Check Unit
--hideSearchCellOptions = Finders.findHideSearchCellOptions >>= click
--
--markdownQueryTitleXPath :: String
--markdownQueryTitleXPath = XPaths.mdCellTitleXPath `following` XPaths.queryCellTitleXPath
--
--provideExploreFile :: String -> Check Unit
--provideExploreFile filename = focusExploreFileField *> typeString filename

provideMd :: String -> Check Unit
provideMd md = typeString (md ++ " ") <* focusMdField

focusMdField :: Check Unit
focusMdField = click XPaths.mdField

--focusExploreFileField :: Check Unit
--focusExploreFileField = Finders.findExploreFileField >>= click

changeMd :: String -> Check Unit
changeMd md = typeString md <* selectAll <* focusMdField

playMd :: Check Unit
playMd = click XPaths.mdPlayButton

--playExplore :: Check Unit
--playExplore = Finders.findExplorePlayButton >>= click
--
playMdQuery :: Check Unit
playMdQuery = click XPaths.mdQueryPlayButton
--
--focusMdQueryField :: Check Unit
--focusMdQueryField = Finders.findMdQueryField >>= click
--
--provideMdQuery :: String -> Check Unit
--provideMdQuery query = focusMdQueryField *> typeString (query ++ " ")

insertQueryAfterMd :: Check Unit
insertQueryAfterMd = click XPaths.insertQueryAfterMd

--showExploreMessages :: Check Unit
--showExploreMessages = Finders.findShowExploreMessages >>= click
