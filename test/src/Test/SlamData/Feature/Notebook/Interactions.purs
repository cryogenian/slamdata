module Test.SlamData.Feature.Notebook.Interactions where

import Control.Apply ((*>))
import Control.Bind ((=<<))
import Control.Alt ((<|>))
--import Control.Monad.Eff.Class (liftEff)
--import Control.Monad.Eff.Random (randomInt)
--import Data.Foldable (traverse_) as F
import Data.Tuple (Tuple(..))
import Data.Maybe (Maybe(..))
--import Data.List (replicateM)
--import Data.Traversable (traverse) as T
import Prelude
import Selenium.Monad (get, refresh)
import Test.Feature (click, pressEnter, provideFieldValue, provideFieldValueWithProperties, selectFromDropdown)
import Test.SlamData.Feature.Monad (SlamFeature(), getConfig)
import Test.SlamData.Feature.Common (waitTime)
import Test.SlamData.Feature.XPaths as XPaths
import XPath as XPath

launchSlamData :: SlamFeature Unit
launchSlamData = get <<< _.slamdataUrl =<< getConfig

mountTestDatabase :: SlamFeature Unit
mountTestDatabase = do
  click (XPath.anywhere XPaths.accessMountDatabase)
  provideFieldValue (XPath.anywhere XPaths.mountName) "test-mount"
  selectFromDropdown (XPath.anywhere XPaths.mountType) "Mongo"
  provideFieldValue (XPath.index (XPath.anywhere XPaths.mountPort) 1) "63174"
  provideFieldValue (XPath.index (XPath.anywhere XPaths.mountHost) 1) "localhost"
  provideFieldValue (XPath.anywhere XPaths.mountDatabase) "testDb"
  click (XPath.anywhere XPaths.mountButton)

browseFolder :: String -> SlamFeature Unit
browseFolder = click <<< XPath.anywhere <<< XPath.anyWithExactText

embedCellOutput :: SlamFeature Unit
embedCellOutput = click $ XPath.anywhere XPaths.embedCellOutput

browseRootFolder :: SlamFeature Unit
browseRootFolder = click $ XPath.index (XPath.anywhere XPaths.browseRootFolder) 1

browseTestFolder :: SlamFeature Unit
browseTestFolder = browseRootFolder *> browseFolder "test-mount" *> browseFolder "testDb"

createNotebook :: SlamFeature Unit
createNotebook = click $ XPath.anywhere XPaths.createNotebook

nameNotebook :: String -> SlamFeature Unit
nameNotebook name = do
  provideFieldValueWithProperties
    [Tuple "value" $ Just "Untitled Notebook"]
    (XPath.anywhere "input")
    name
  pressEnter

deleteFile :: String -> SlamFeature Unit
deleteFile name =
  click (XPath.anywhere $ XPaths.selectFile name) *> click (XPath.anywhere $ XPaths.removeFile name)

selectFile :: String -> SlamFeature Unit
selectFile name = select name <|> (deselect name *> select name)
  where
  select = click <<< XPath.anywhere <<< XPaths.selectFile
  deselect = click <<< XPath.anywhere <<< XPaths.deselectFile

createNotebookInTestFolder :: String -> SlamFeature Unit
createNotebookInTestFolder name = browseTestFolder *> createNotebook *> nameNotebook name

deleteFileInTestFolder :: String -> SlamFeature Unit
deleteFileInTestFolder name = browseTestFolder *> deleteFile name

reopenCurrentNotebook :: SlamFeature Unit
reopenCurrentNotebook = waitTime 2000 *> refresh

expandNewCellMenu :: SlamFeature Unit
expandNewCellMenu = click (XPath.anywhere XPaths.insertCell)

insertQueryCellUsingNextActionMenu :: SlamFeature Unit
insertQueryCellUsingNextActionMenu =
  expandNewCellMenu *> click (XPath.anywhere XPaths.insertQueryCell)

insertMdCellUsingNextActionMenu :: SlamFeature Unit
insertMdCellUsingNextActionMenu =
  expandNewCellMenu *> click (XPath.anywhere XPaths.insertMdCell)

insertExploreCellUsingNextActionMenu :: SlamFeature Unit
insertExploreCellUsingNextActionMenu =
  expandNewCellMenu *> click (XPath.anywhere XPaths.insertExploreCell)

insertSearchCellUsingNextActionMenu :: SlamFeature Unit
insertSearchCellUsingNextActionMenu =
  expandNewCellMenu *> click (XPath.anywhere XPaths.insertSearchCell)

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

provideExploreFile :: String -> SlamFeature Unit
provideExploreFile = provideFieldValue (XPath.anywhere XPaths.exploreInput)

provideMd :: String -> SlamFeature Unit
provideMd = provideFieldValue (XPath.anywhere XPaths.mdField)

provideExploreSearch :: String -> SlamFeature Unit
provideExploreSearch =
  provideFieldValue
    $ XPath.anywhere
    $ XPaths.exploreCellTitle `XPath.following` XPaths.searchStringInput

provideExploreSearchSearch :: String -> SlamFeature Unit
provideExploreSearchSearch =
  provideFieldValue
    $ flip XPath.index 2
    $ XPath.anywhere
    $ XPaths.exploreCellTitle `XPath.following` XPaths.searchStringInput

focusMdField :: SlamFeature Unit
focusMdField = click $ XPath.anywhere XPaths.mdField

--focusExploreFileField :: SlamFeature Unit
--focusExploreFileField = Finders.findExploreFileField >>= click

playMd :: SlamFeature Unit
playMd = click $ XPath.anywhere XPaths.mdPlayButton

playExplore :: SlamFeature Unit
playExplore = click $ XPath.anywhere XPaths.explorePlayButton

playMdQuery :: SlamFeature Unit
playMdQuery = click $ XPath.anywhere XPaths.mdQueryPlayButton

playExploreSearch :: SlamFeature Unit
playExploreSearch =
  click
    $ XPath.anywhere
    $ XPaths.exploreCellTitle `XPath.following` XPaths.searchPlayButton

playExploreSearchSearch :: SlamFeature Unit
playExploreSearchSearch =
  click
    $ flip XPath.index 2
    $ XPath.anywhere
    $ XPaths.exploreCellTitle `XPath.following` XPaths.searchPlayButton

provideMdQuery :: String -> SlamFeature Unit
provideMdQuery = provideFieldValue (XPath.anywhere XPaths.mdQueryField)

insertQueryAfterMd :: SlamFeature Unit
insertQueryAfterMd = click $ XPath.anywhere XPaths.insertQueryAfterMd

insertSearchAfterExplore :: SlamFeature Unit
insertSearchAfterExplore = click $ XPath.anywhere XPaths.insertSearchAfterExplore

insertSearchAfterSearchAfterExplore :: SlamFeature Unit
insertSearchAfterSearchAfterExplore =
  click
    $ XPath.index
        (XPath.anywhere XPaths.insertSearchAfterExplore)
        2

--showExploreMessages :: SlamFeature Unit
--showExploreMessages = Finders.findShowExploreMessages >>= click
