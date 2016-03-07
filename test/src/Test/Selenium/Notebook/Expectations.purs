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

module Test.Selenium.Notebook.Expectations where

import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Data.Maybe (Maybe(..))
import Prelude
import Test.Selenium.Feature (expectPresentedWithProperties, expectPresented, expectHidden)
import Test.Selenium.XPaths as XPaths
import Test.XPath as XPath
import Test.Selenium.Monad (Check())
import Test.Selenium.Properties as Properties

expectInitialFileListToBePresentedInOrder :: Check Unit
expectInitialFileListToBePresentedInOrder =
  expectPresented $ XPath.anywhere XPaths.initialFileListInOrder

expectInitialFileListToBeHidden :: Check Unit
expectInitialFileListToBeHidden =
  void $ traverse expectHidden $ map XPath.anywhere XPaths.initialFileList

expectFileFromFileListToBePresented :: String -> Check Unit
expectFileFromFileListToBePresented =
  expectPresented <<< XPath.anywhere <<< XPath.anyWithExactText

expectEmbedCellOutputToBePresented :: Check Unit
expectEmbedCellOutputToBePresented =
  expectPresented $ XPath.anywhere XPaths.embedCellOutput

expectEmbedCellOutputToBeHidden :: Check Unit
expectEmbedCellOutputToBeHidden =
  expectHidden $ XPath.anywhere XPaths.embedCellOutput

expectInsertQueryAfterThisToBePresented :: Check Unit
expectInsertQueryAfterThisToBePresented =
  expectPresented $ XPath.anywhere XPaths.insertQueryAfterThis

expectInsertSearchAfterThisToBePresented :: Check Unit
expectInsertSearchAfterThisToBePresented =
  expectPresented $ XPath.anywhere XPaths.insertSearchAfterThis

expectInsertVisualizeAfterThisToBePresented :: Check Unit
expectInsertVisualizeAfterThisToBePresented =
  expectPresented $ XPath.anywhere XPaths.insertVisualizeAfterThis

expectInsertDownloadAfterThisToBePresented :: Check Unit
expectInsertDownloadAfterThisToBePresented =
  expectPresented $ XPath.anywhere XPaths.insertDownloadAfterThis

expectInsertQueryAfterThisToBeHidden :: Check Unit
expectInsertQueryAfterThisToBeHidden =
  expectHidden $ XPath.anywhere XPaths.insertQueryAfterThis

expectInsertSearchAfterThisToBeHidden :: Check Unit
expectInsertSearchAfterThisToBeHidden =
  expectHidden $ XPath.anywhere XPaths.insertSearchAfterThis

expectInsertVisualizeAfterThisToBeHidden :: Check Unit
expectInsertVisualizeAfterThisToBeHidden =
  expectHidden $ XPath.anywhere XPaths.insertVisualizeAfterThis

expectInsertDownloadAfterThisToBeHidden :: Check Unit
expectInsertDownloadAfterThisToBeHidden =
  expectHidden $ XPath.anywhere XPaths.insertDownloadAfterThis

expectInsertQueryAfterMdToBePresented :: Check Unit
expectInsertQueryAfterMdToBePresented =
  expectPresented $ XPath.anywhere XPaths.insertQueryAfterMd

expectEmbedCellOutputSnippetToBeHidden :: Check Unit
expectEmbedCellOutputSnippetToBeHidden =
  expectHidden $ XPath.anywhere XPaths.embedCellOutputSnippet

expectEmbedCellOutputTitleToBeHidden :: Check Unit
expectEmbedCellOutputTitleToBeHidden =
  expectHidden $ XPath.anywhere XPaths.embedCellOutputTitle

expectDismissInsertCellMenuToBePresented :: Check Unit
expectDismissInsertCellMenuToBePresented =
  expectPresented $ XPath.anywhere XPaths.dismissInsertCellMenu

expectInsertQueryCellToBePresented :: Check Unit
expectInsertQueryCellToBePresented =
  expectPresented $ XPath.anywhere XPaths.insertQueryCell

expectInsertMdCellToBePresented :: Check Unit
expectInsertMdCellToBePresented =
  expectPresented $ XPath.anywhere XPaths.insertMdCell

expectInsertExploreCellToBePresented :: Check Unit
expectInsertExploreCellToBePresented =
  expectPresented $ XPath.anywhere XPaths.insertExploreCell

expectInsertSearchCellToBePresented :: Check Unit
expectInsertSearchCellToBePresented =
  expectPresented $ XPath.anywhere XPaths.insertSearchCell

expectInsertCellToBePresented :: Check Unit
expectInsertCellToBePresented =
  expectPresented $ XPath.anywhere XPaths.insertCell

expectQueryCellTitlesToBePresented :: Check Unit
expectQueryCellTitlesToBePresented =
  expectPresented $ XPath.anywhere XPaths.queryCellTitle

expectExploreCellTitlesToBePresented :: Check Unit
expectExploreCellTitlesToBePresented =
  expectPresented $ XPath.anywhere XPaths.exploreCellTitle

expectSearchCellTitlesToBePresented :: Check Unit
expectSearchCellTitlesToBePresented =
  expectPresented $ XPath.anywhere XPaths.searchCellTitle

expectMdCellTitlesToBePresented :: Check Unit
expectMdCellTitlesToBePresented =
  expectPresented $ XPath.anywhere XPaths.mdCellTitle

expectCellTitlesToBeHidden :: Check Unit
expectCellTitlesToBeHidden =
  expectHidden $ XPath.anywhere XPaths.cellTitle

expectQueryCellTitlesToBeHidden :: Check Unit
expectQueryCellTitlesToBeHidden =
  expectHidden $ XPath.anywhere XPaths.queryCellTitle

expectExploreCellTitlesToBeHidden :: Check Unit
expectExploreCellTitlesToBeHidden =
  expectHidden $ XPath.anywhere XPaths.exploreCellTitle

expectSearchCellTitlesToBeHidden :: Check Unit
expectSearchCellTitlesToBeHidden =
  expectHidden $ XPath.anywhere XPaths.searchCellTitle

expectMdCellTitlesToBeHidden :: Check Unit
expectMdCellTitlesToBeHidden =
  expectHidden $ XPath.anywhere XPaths.mdCellTitle

expectShowFileListToBePresented :: Check Unit
expectShowFileListToBePresented =
  expectPresented $ XPath.anywhere XPaths.showFileList

expectHideFileListToBePresented :: Check Unit
expectHideFileListToBePresented =
  expectPresented $ XPath.anywhere XPaths.hideFileList

expectDeleteCellOptionsToBePresented :: Check Unit
expectDeleteCellOptionsToBePresented =
  expectPresented $ XPath.anywhere XPaths.deleteCell

expectIndexedQueryCellTitleToBePresented :: Int -> Check Unit
expectIndexedQueryCellTitleToBePresented =
  expectPresented <<< XPath.index (XPath.anywhere XPaths.queryCellTitle)

expectIndexedMdCellTitleToBePresented :: Int -> Check Unit
expectIndexedMdCellTitleToBePresented =
  expectPresented <<< XPath.index (XPath.anywhere XPaths.mdCellTitle)

expectIndexedExploreCellTitleToBePresented :: Int -> Check Unit
expectIndexedExploreCellTitleToBePresented =
  expectPresented <<< XPath.index (XPath.anywhere XPaths.exploreCellTitle)

expectIndexedSearchCellTitleToBePresented :: Int -> Check Unit
expectIndexedSearchCellTitleToBePresented =
  expectPresented <<< XPath.index (XPath.anywhere XPaths.searchCellTitle)

expectHideQueryCellOptionsToBePresented :: Check Unit
expectHideQueryCellOptionsToBePresented =
  expectPresented $ XPath.anywhere XPaths.hideQueryCellOptions

expectHideMdCellOptionsToBePresented :: Check Unit
expectHideMdCellOptionsToBePresented =
  expectPresented $ XPath.anywhere XPaths.hideMdCellOptions

expectHideExploreCellOptionsToBePresented :: Check Unit
expectHideExploreCellOptionsToBePresented =
  expectPresented $ XPath.anywhere XPaths.hideExploreCellOptions

expectHideSearchCellOptionsToBePresented :: Check Unit
expectHideSearchCellOptionsToBePresented =
  expectPresented $ XPath.anywhere XPaths.hideSearchCellOptions

expectShowQueryCellOptionsToBePresented :: Check Unit
expectShowQueryCellOptionsToBePresented =
  expectPresented $ XPath.anywhere XPaths.showQueryCellOptions

expectShowMdCellOptionsToBePresented :: Check Unit
expectShowMdCellOptionsToBePresented =
  expectPresented $ XPath.anywhere XPaths.showMdCellOptions

expectShowExploreCellOptionsToBePresented :: Check Unit
expectShowExploreCellOptionsToBePresented =
  expectPresented $ XPath.anywhere XPaths.showExploreCellOptions

expectShowSearchCellOptionsToBePresented :: Check Unit
expectShowSearchCellOptionsToBePresented =
  expectPresented $ XPath.anywhere XPaths.showSearchCellOptions

expectExploreInputToBePresented :: Check Unit
expectExploreInputToBePresented =
  expectPresented $ XPath.anywhere XPaths.exploreInput

expectExploreInputToBeHidden :: Check Unit
expectExploreInputToBeHidden =
  expectHidden $ XPath.anywhere XPaths.exploreInput

expectBrowseRootFolderToBePresented :: Check Unit
expectBrowseRootFolderToBePresented =
  expectPresented $ XPath.anywhere XPaths.browseRootFolder

expectRemoveFileToBePresented :: String -> Check Unit
expectRemoveFileToBePresented =
  expectPresented <<< XPath.anywhere <<< XPaths.removeFile

expectCreateNotebookToBePresented :: Check Unit
expectCreateNotebookToBePresented =
  expectPresented $ XPath.anywhere XPaths.createNotebook

expectUntitledNotebookNameInputToBePresented :: Check Unit
expectUntitledNotebookNameInputToBePresented =
  expectPresentedWithProperties [Properties.untitledNotebookValue] $ XPath.anywhere "input"

--findSelectFileInputWithValue :: String -> Check Unit
--findSelectFileInputWithValue = findInputWithPlaceholderAndValue "Select a file"

expectMdFieldToBePresented :: Check Unit
expectMdFieldToBePresented =
  expectPresented $ XPath.anywhere XPaths.mdField

expectMdPlayButtonToBePresented :: Check Unit
expectMdPlayButtonToBePresented =
  expectPresented $ XPath.anywhere XPaths.mdPlayButton

expectExplorePlayButtonToBePresented :: Check Unit
expectExplorePlayButtonToBePresented =
  expectPresented $ XPath.anywhere xPath
  where
  xPath = XPaths.exploreCellTitle `XPath.following` XPaths.play

expectMdQueryPlayButtonToBePresented :: Check Unit
expectMdQueryPlayButtonToBePresented =
  expectPresented $ XPath.anywhere XPaths.mdQueryPlayButton

expectCreateMdQueryCellButtonToBePresented :: Check Unit
expectCreateMdQueryCellButtonToBePresented =
  expectPresented
    $ XPath.anywhere $ XPath.anyWithExactAriaLabel "Insert Query cell after this cell"

expectMdQueryFieldToBePresented :: Check Unit
expectMdQueryFieldToBePresented = expectPresented $ XPath.anywhere xPath
  where
  xPath = XPaths.mdQueryCellTitle `XPath.following` XPaths.aceEditor

expectMdQueryColumnCellsWithIndexToBePresented :: Int -> Check Unit
expectMdQueryColumnCellsWithIndexToBePresented index =
  expectPresented $ XPath.index (XPath.anywhere xPath) index
  where
  xPath = XPaths.mdQueryCellTitle `XPath.following` "tbody/tr/td"

expectFileToBePresented :: String -> Check Unit
expectFileToBePresented name = expectPresented $ XPath.anywhere $ XPath.anyWithExactText name

expectExploreErrorMessageToBePresented :: Check Unit
expectExploreErrorMessageToBePresented =
  expectPresented
    $ XPath.anywhere $ XPaths.exploreCellTitle `XPath.following` XPaths.oneErrorMessage

expectShowExploreMessagesToBePresented :: Check Unit
expectShowExploreMessagesToBePresented =
  expectPresented
    $ XPath.anywhere $ XPaths.exploreCellTitle `XPath.following` XPaths.showMessages

expectNoFileSelectedMessageToBePresented :: Check Unit
expectNoFileSelectedMessageToBePresented =
  expectPresented $ XPath.anywhere XPaths.noFileSelectedMessage

expectExploreFileFieldToBePresented :: Check Unit
expectExploreFileFieldToBePresented =
  expectPresented
    $ XPath.anywhere $ XPaths.exploreCellTitle `XPath.following` XPaths.selectFileField

expectFileDoesNotExistMessageToBePresented :: String -> Check Unit
expectFileDoesNotExistMessageToBePresented =
  expectPresented <<< XPath.anywhere <<< XPaths.fileDoesNotExist

