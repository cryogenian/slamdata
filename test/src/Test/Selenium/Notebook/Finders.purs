{-
Copyright 2015 SlamData, Inc.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http:www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
-}

module Test.Selenium.Notebook.Finders where

import Data.List (List(), length)
import Data.Traversable (traverse)
import Control.Bind ((<=<))
import Prelude
import Selenium.Monad (loseElement, findElements, findExact, byXPath, tryRepeatedlyTo, getText)
import Test.Selenium.Finders (findInputWithPlaceholderAndValue, findByXPathAndProperty, findByXPath, findFirstByXPath, findAllByXPath, findAnyByXPath, loseByXPath)
import Selenium.Types (Element())
import Test.Selenium.XPaths as XPaths
import Test.XPath as XPath
import Test.Selenium.Monad (Check())

-- Finders
findInitialFileListInOrder :: Check Element
findInitialFileListInOrder = findByXPath $ XPath.anywhere XPaths.initialFileListInOrderXPath

loseInitialFileList :: Check Unit
loseInitialFileList = void $ traverse loseByXPath $ map XPath.anywhere XPaths.initialFileListXPaths

findFileFromFileList :: String -> Check Element
findFileFromFileList = findByXPath <<< XPath.anywhere <<< XPath.anyWithExactText

findEmbedCellOutput :: Check Element
findEmbedCellOutput = findByXPath $ XPath.anywhere XPaths.embedCellOutputXPath

loseEmbedCellOutput :: Check Unit
loseEmbedCellOutput = loseByXPath $ XPath.anywhere XPaths.embedCellOutputXPath

findInsertQueryAfterThis :: Check Element
findInsertQueryAfterThis = findByXPath $ XPath.anywhere XPaths.insertQueryAfterThisXPath

findInsertSearchAfterThis :: Check Element
findInsertSearchAfterThis = findByXPath $ XPath.anywhere XPaths.insertSearchAfterThisXPath

findInsertVisualizeAfterThis :: Check Element
findInsertVisualizeAfterThis = findByXPath $ XPath.anywhere XPaths.insertVisualizeAfterThisXPath

findInsertDownloadAfterThis :: Check Element
findInsertDownloadAfterThis = findByXPath $ XPath.anywhere XPaths.insertDownloadAfterThisXPath

loseInsertQueryAfterThis :: Check Unit
loseInsertQueryAfterThis = loseByXPath $ XPath.anywhere XPaths.insertQueryAfterThisXPath

loseInsertSearchAfterThis :: Check Unit
loseInsertSearchAfterThis = loseByXPath $ XPath.anywhere XPaths.insertSearchAfterThisXPath

loseInsertVisualizeAfterThis :: Check Unit
loseInsertVisualizeAfterThis = loseByXPath $ XPath.anywhere XPaths.insertVisualizeAfterThisXPath

loseInsertDownloadAfterThis :: Check Unit
loseInsertDownloadAfterThis = loseByXPath $ XPath.anywhere XPaths.insertDownloadAfterThisXPath

findInsertQueryAfterMd :: Check Element
findInsertQueryAfterMd = findByXPath $ XPath.anywhere XPaths.insertQueryAfterMdXPath

loseEmbedCellOutputSnippet :: Check Unit
loseEmbedCellOutputSnippet = loseByXPath $ XPath.anywhere XPaths.embedCellOutputSnippetXPath

loseEmbedCellOutputTitle :: Check Unit
loseEmbedCellOutputTitle = loseByXPath $ XPath.anywhere XPaths.embedCellOutputTitleXPath

findDismissInsertCellMenu :: Check Element
findDismissInsertCellMenu = findByXPath $ XPath.anywhere XPaths.dismissInsertCellMenuXPath

findInsertQueryCell :: Check Element
findInsertQueryCell = findByXPath $ XPath.anywhere XPaths.insertQueryCellXPath

findInsertMdCell :: Check Element
findInsertMdCell = findByXPath $ XPath.anywhere XPaths.insertMdCellXPath

findInsertExploreCell :: Check Element
findInsertExploreCell = findByXPath $ XPath.anywhere XPaths.insertExploreCellXPath

findInsertSearchCell :: Check Element
findInsertSearchCell = findByXPath $ XPath.anywhere XPaths.insertSearchCellXPath

findInsertCell :: Check Element
findInsertCell = findByXPath $ XPath.anywhere XPaths.insertCellXPath

findAllQueryCellTitles :: Check (List Element)
findAllQueryCellTitles = findAllByXPath $ XPath.anywhere XPaths.queryCellTitleXPath

findAllExploreCellTitles :: Check (List Element)
findAllExploreCellTitles = findAllByXPath $ XPath.anywhere XPaths.exploreCellTitleXPath

findAllSearchCellTitles :: Check (List Element)
findAllSearchCellTitles = findAllByXPath $ XPath.anywhere XPaths.searchCellTitleXPath

findAllMdCellTitles :: Check (List Element)
findAllMdCellTitles = findAllByXPath $ XPath.anywhere XPaths.mdCellTitleXPath

loseCellTitles :: Check Unit
loseCellTitles = loseByXPath $ XPath.anywhere XPaths.cellTitleXPath

loseQueryCellTitles :: Check Unit
loseQueryCellTitles = loseByXPath $ XPath.anywhere XPaths.queryCellTitleXPath

loseExploreCellTitles :: Check Unit
loseExploreCellTitles = loseByXPath $ XPath.anywhere XPaths.exploreCellTitleXPath

loseSearchCellTitles :: Check Unit
loseSearchCellTitles = loseByXPath $ XPath.anywhere XPaths.searchCellTitleXPath

loseMdCellTitles :: Check Unit
loseMdCellTitles = loseByXPath $ XPath.anywhere XPaths.mdCellTitleXPath

findShowFileList :: Check Element
findShowFileList = findByXPath $ XPath.anywhere XPaths.showFileListXPath

findHideFileList :: Check Element
findHideFileList = findByXPath $ XPath.anywhere XPaths.hideFileListXPath

findAllDeleteCellOptions :: Check (List Element)
findAllDeleteCellOptions = findAllByXPath $ XPath.anywhere XPaths.deleteCellXPath

findAnyDeleteCellOptions :: Check (List Element)
findAnyDeleteCellOptions = findAnyByXPath $ XPath.anywhere XPaths.deleteCellXPath

findIndexedQueryCellTitle :: Int -> Check Element
findIndexedQueryCellTitle = findByXPath <<< XPath.index (XPath.anywhere XPaths.queryCellTitleXPath)

findIndexedMdCellTitle :: Int -> Check Element
findIndexedMdCellTitle = findByXPath <<< XPath.index (XPath.anywhere XPaths.mdCellTitleXPath)

findIndexedExploreCellTitle :: Int -> Check Element
findIndexedExploreCellTitle = findByXPath <<< XPath.index (XPath.anywhere XPaths.exploreCellTitleXPath)

findIndexedSearchCellTitle :: Int -> Check Element
findIndexedSearchCellTitle = findByXPath <<< XPath.index (XPath.anywhere XPaths.searchCellTitleXPath)

findHideQueryCellOptions :: Check Element
findHideQueryCellOptions = findByXPath $ XPath.anywhere XPaths.hideQueryCellOptionsXPath

findHideMdCellOptions :: Check Element
findHideMdCellOptions = findByXPath $ XPath.anywhere XPaths.hideMdCellOptionsXPath

findHideExploreCellOptions :: Check Element
findHideExploreCellOptions = findByXPath $ XPath.anywhere XPaths.hideExploreCellOptionsXPath

findHideSearchCellOptions :: Check Element
findHideSearchCellOptions = findByXPath $ XPath.anywhere XPaths.hideSearchCellOptionsXPath

findShowQueryCellOptions :: Check Element
findShowQueryCellOptions = findByXPath $ XPath.anywhere XPaths.showQueryCellOptionsXPath

findShowMdCellOptions :: Check Element
findShowMdCellOptions = findByXPath $ XPath.anywhere XPaths.showMdCellOptionsXPath

findShowExploreCellOptions :: Check Element
findShowExploreCellOptions = findByXPath $ XPath.anywhere XPaths.showExploreCellOptionsXPath

findShowSearchCellOptions :: Check Element
findShowSearchCellOptions = findByXPath $ XPath.anywhere XPaths.showSearchCellOptionsXPath

findExploreInput :: Check Element
findExploreInput = findByXPath $ XPath.anywhere XPaths.exploreInputXPath

loseExploreInput :: Check Unit
loseExploreInput = loseByXPath $ XPath.anywhere XPaths.exploreInputXPath

findBrowseRootFolder :: Check Element
findBrowseRootFolder = findByXPath $ XPath.anywhere XPaths.browseRootFolderXPath

findRemoveFile :: String -> Check Element
findRemoveFile = findFirstByXPath <<< XPath.anywhere <<< XPaths.removeFileXPath

findCreateNotebook :: Check Element
findCreateNotebook = findFirstByXPath $ XPath.anywhere XPaths.createNotebookXPath

findUntitledNotebookNameInput :: Check Element
findUntitledNotebookNameInput =
  findByXPathAndProperty (XPath.anywhere "input") "value" "Untitled Notebook"

findSelectFileInputWithValue :: String -> Check Element
findSelectFileInputWithValue = findInputWithPlaceholderAndValue "Select a file"

findMdField :: Check Element
findMdField = findByXPath $ XPath.anywhere xPath
  where
  xPath = XPaths.mdCellTitleXPath `XPath.following` XPaths.aceEditorXPath

findMdPlayButton :: Check Element
findMdPlayButton = findByXPath $ XPath.anywhere xPath
  where
  xPath = XPaths.mdCellTitleXPath `XPath.following` XPaths.playXPath

findExplorePlayButton :: Check Element
findExplorePlayButton = findByXPath $ XPath.anywhere xPath
  where
  xPath = XPaths.exploreCellTitleXPath `XPath.following` XPaths.playXPath

findMdQueryPlayButton :: Check Element
findMdQueryPlayButton = findByXPath $ XPath.anywhere xPath
  where
  xPath = XPaths.mdQueryCellTitleXPath `XPath.following` XPaths.playXPath

findCreateMdQueryCellButton :: Check Element
findCreateMdQueryCellButton =
  findByXPath $ XPath.anywhere $ XPath.anyWithExactAriaLabel "Insert Query cell after this cell"

findMdQueryField :: Check Element
findMdQueryField = findByXPath $ XPath.anywhere xPath
  where
  xPath = XPaths.mdQueryCellTitleXPath `XPath.following` XPaths.aceEditorXPath

findMdQueryColumnCellsByIndex :: Int -> Check (List Element)
findMdQueryColumnCellsByIndex index = findAllByXPath $ XPath.index (XPath.anywhere xPath) index
  where
  xPath = XPaths.mdQueryCellTitleXPath `XPath.following` "tbody/tr/td"

findMdQueryColumnCellsByHeading :: String -> Check (List Element)
findMdQueryColumnCellsByHeading heading = findHeaderIndex >>= findMdQueryColumnCellsByIndex
  where
  columnHeadingXPath = XPath.nodeWithExactText "thead/tr/th"
  findHeaderIndex = ((+ 1) <<< length) <$> (findAnyByXPath $ XPath.anywhere priorHeadingXPath)
  priorHeadingXPath =
    XPaths.mdQueryCellTitleXPath `XPath.following` columnHeadingXPath heading `XPath.precedingSibling` XPath.any

findMdQueryColumnCellsTextByHeading :: String -> Check (List String)
findMdQueryColumnCellsTextByHeading s = findMdQueryColumnCellsByHeading s >>= traverse getText

findFile :: String -> Check Element
findFile name = findByXPath $ XPath.anywhere $ XPath.anyWithExactText name

findExploreErrorMessage :: Check Element
findExploreErrorMessage =
  findByXPath
    $ XPath.anywhere $ XPaths.exploreCellTitleXPath `XPath.following` XPaths.oneErrorMessageXPath

findShowExploreMessages :: Check Element
findShowExploreMessages =
  findByXPath $ XPath.anywhere $ XPaths.exploreCellTitleXPath `XPath.following` XPaths.showMessagesXPath

findNoFileSelectedMessage :: Check Element
findNoFileSelectedMessage = findByXPath $ XPath.anywhere XPaths.noFileSelectedMessageXPath

findExploreFileField :: Check Element
findExploreFileField =
  findByXPath
    $ XPath.anywhere $ XPaths.exploreCellTitleXPath `XPath.following` XPaths.selectFileFieldXPath

findFileDoesNotExistMessage :: String -> Check Element
findFileDoesNotExistMessage = findByXPath <<< XPath.anywhere <<< XPaths.fileDoesNotExistXPath

