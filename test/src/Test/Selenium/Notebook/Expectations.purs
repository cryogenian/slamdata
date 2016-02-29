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
import Test.Feature (expectPresentedWithProperties, expectPresented, expectPresented, expectPresented, expectPresented, expectHidden)
import Test.Selenium.XPaths as XPaths
import Test.XPath as XPath
import Test.Selenium.Monad (Check())

expectInitialFileListToBePresentedInOrder :: Check Unit
expectInitialFileListToBePresentedInOrder =
  expectPresented $ XPath.anywhere XPaths.initialFileListInOrderXPath

expectInitialFileListToBeHidden :: Check Unit
expectInitialFileListToBeHidden =
  void $ traverse expectHidden $ map XPath.anywhere XPaths.initialFileListXPaths

expectFileFromFileListToBePresented :: String -> Check Unit
expectFileFromFileListToBePresented =
  expectPresented <<< XPath.anywhere <<< XPath.anyWithExactText

expectEmbedCellOutputToBePresented :: Check Unit
expectEmbedCellOutputToBePresented =
  expectPresented $ XPath.anywhere XPaths.embedCellOutputXPath

expectEmbedCellOutputToBeHidden :: Check Unit
expectEmbedCellOutputToBeHidden =
  expectHidden $ XPath.anywhere XPaths.embedCellOutputXPath

expectInsertQueryAfterThisToBePresented :: Check Unit
expectInsertQueryAfterThisToBePresented =
  expectPresented $ XPath.anywhere XPaths.insertQueryAfterThisXPath

expectInsertSearchAfterThisToBePresented :: Check Unit
expectInsertSearchAfterThisToBePresented =
  expectPresented $ XPath.anywhere XPaths.insertSearchAfterThisXPath

expectInsertVisualizeAfterThisToBePresented :: Check Unit
expectInsertVisualizeAfterThisToBePresented =
  expectPresented $ XPath.anywhere XPaths.insertVisualizeAfterThisXPath

expectInsertDownloadAfterThisToBePresented :: Check Unit
expectInsertDownloadAfterThisToBePresented =
  expectPresented $ XPath.anywhere XPaths.insertDownloadAfterThisXPath

expectInsertQueryAfterThisToBeHidden :: Check Unit
expectInsertQueryAfterThisToBeHidden =
  expectHidden $ XPath.anywhere XPaths.insertQueryAfterThisXPath

expectInsertSearchAfterThisToBeHidden :: Check Unit
expectInsertSearchAfterThisToBeHidden =
  expectHidden $ XPath.anywhere XPaths.insertSearchAfterThisXPath

expectInsertVisualizeAfterThisToBeHidden :: Check Unit
expectInsertVisualizeAfterThisToBeHidden =
  expectHidden $ XPath.anywhere XPaths.insertVisualizeAfterThisXPath

expectInsertDownloadAfterThisToBeHidden :: Check Unit
expectInsertDownloadAfterThisToBeHidden =
  expectHidden $ XPath.anywhere XPaths.insertDownloadAfterThisXPath

expectInsertQueryAfterMdToBePresented :: Check Unit
expectInsertQueryAfterMdToBePresented =
  expectPresented $ XPath.anywhere XPaths.insertQueryAfterMdXPath

expectEmbedCellOutputSnippetToBeHidden :: Check Unit
expectEmbedCellOutputSnippetToBeHidden =
  expectHidden $ XPath.anywhere XPaths.embedCellOutputSnippetXPath

expectEmbedCellOutputTitleToBeHidden :: Check Unit
expectEmbedCellOutputTitleToBeHidden =
  expectHidden $ XPath.anywhere XPaths.embedCellOutputTitleXPath

expectDismissInsertCellMenuToBePresented :: Check Unit
expectDismissInsertCellMenuToBePresented =
  expectPresented $ XPath.anywhere XPaths.dismissInsertCellMenuXPath

expectInsertQueryCellToBePresented :: Check Unit
expectInsertQueryCellToBePresented =
  expectPresented $ XPath.anywhere XPaths.insertQueryCellXPath

expectInsertMdCellToBePresented :: Check Unit
expectInsertMdCellToBePresented =
  expectPresented $ XPath.anywhere XPaths.insertMdCellXPath

expectInsertExploreCellToBePresented :: Check Unit
expectInsertExploreCellToBePresented =
  expectPresented $ XPath.anywhere XPaths.insertExploreCellXPath

expectInsertSearchCellToBePresented :: Check Unit
expectInsertSearchCellToBePresented =
  expectPresented $ XPath.anywhere XPaths.insertSearchCellXPath

expectInsertCellToBePresented :: Check Unit
expectInsertCellToBePresented =
  expectPresented $ XPath.anywhere XPaths.insertCellXPath

expectQueryCellTitlesToBePresented :: Check Unit
expectQueryCellTitlesToBePresented =
  expectPresented $ XPath.anywhere XPaths.queryCellTitleXPath

expectExploreCellTitlesToBePresented :: Check Unit
expectExploreCellTitlesToBePresented =
  expectPresented $ XPath.anywhere XPaths.exploreCellTitleXPath

expectSearchCellTitlesToBePresented :: Check Unit
expectSearchCellTitlesToBePresented =
  expectPresented $ XPath.anywhere XPaths.searchCellTitleXPath

expectMdCellTitlesToBePresented :: Check Unit
expectMdCellTitlesToBePresented =
  expectPresented $ XPath.anywhere XPaths.mdCellTitleXPath

expectCellTitlesToBeHidden :: Check Unit
expectCellTitlesToBeHidden =
  expectHidden $ XPath.anywhere XPaths.cellTitleXPath

expectQueryCellTitlesToBeHidden :: Check Unit
expectQueryCellTitlesToBeHidden =
  expectHidden $ XPath.anywhere XPaths.queryCellTitleXPath

expectExploreCellTitlesToBeHidden :: Check Unit
expectExploreCellTitlesToBeHidden =
  expectHidden $ XPath.anywhere XPaths.exploreCellTitleXPath

expectSearchCellTitlesToBeHidden :: Check Unit
expectSearchCellTitlesToBeHidden =
  expectHidden $ XPath.anywhere XPaths.searchCellTitleXPath

expectMdCellTitlesToBeHidden :: Check Unit
expectMdCellTitlesToBeHidden =
  expectHidden $ XPath.anywhere XPaths.mdCellTitleXPath

expectShowFileListToBePresented :: Check Unit
expectShowFileListToBePresented =
  expectPresented $ XPath.anywhere XPaths.showFileListXPath

expectHideFileListToBePresented :: Check Unit
expectHideFileListToBePresented =
  expectPresented $ XPath.anywhere XPaths.hideFileListXPath

expectDeleteCellOptionsToBePresented :: Check Unit
expectDeleteCellOptionsToBePresented =
  expectPresented $ XPath.anywhere XPaths.deleteCellXPath

expectIndexedQueryCellTitleToBePresented :: Int -> Check Unit
expectIndexedQueryCellTitleToBePresented =
  expectPresented <<< XPath.index (XPath.anywhere XPaths.queryCellTitleXPath)

expectIndexedMdCellTitleToBePresented :: Int -> Check Unit
expectIndexedMdCellTitleToBePresented =
  expectPresented <<< XPath.index (XPath.anywhere XPaths.mdCellTitleXPath)

expectIndexedExploreCellTitleToBePresented :: Int -> Check Unit
expectIndexedExploreCellTitleToBePresented =
  expectPresented <<< XPath.index (XPath.anywhere XPaths.exploreCellTitleXPath)

expectIndexedSearchCellTitleToBePresented :: Int -> Check Unit
expectIndexedSearchCellTitleToBePresented =
  expectPresented <<< XPath.index (XPath.anywhere XPaths.searchCellTitleXPath)

expectHideQueryCellOptionsToBePresented :: Check Unit
expectHideQueryCellOptionsToBePresented =
  expectPresented $ XPath.anywhere XPaths.hideQueryCellOptionsXPath

expectHideMdCellOptionsToBePresented :: Check Unit
expectHideMdCellOptionsToBePresented =
  expectPresented $ XPath.anywhere XPaths.hideMdCellOptionsXPath

expectHideExploreCellOptionsToBePresented :: Check Unit
expectHideExploreCellOptionsToBePresented =
  expectPresented $ XPath.anywhere XPaths.hideExploreCellOptionsXPath

expectHideSearchCellOptionsToBePresented :: Check Unit
expectHideSearchCellOptionsToBePresented =
  expectPresented $ XPath.anywhere XPaths.hideSearchCellOptionsXPath

expectShowQueryCellOptionsToBePresented :: Check Unit
expectShowQueryCellOptionsToBePresented =
  expectPresented $ XPath.anywhere XPaths.showQueryCellOptionsXPath

expectShowMdCellOptionsToBePresented :: Check Unit
expectShowMdCellOptionsToBePresented =
  expectPresented $ XPath.anywhere XPaths.showMdCellOptionsXPath

expectShowExploreCellOptionsToBePresented :: Check Unit
expectShowExploreCellOptionsToBePresented =
  expectPresented $ XPath.anywhere XPaths.showExploreCellOptionsXPath

expectShowSearchCellOptionsToBePresented :: Check Unit
expectShowSearchCellOptionsToBePresented =
  expectPresented $ XPath.anywhere XPaths.showSearchCellOptionsXPath

expectExploreInputToBePresented :: Check Unit
expectExploreInputToBePresented =
  expectPresented $ XPath.anywhere XPaths.exploreInputXPath

expectExploreInputToBeHidden :: Check Unit
expectExploreInputToBeHidden =
  expectHidden $ XPath.anywhere XPaths.exploreInputXPath

expectBrowseRootFolderToBePresented :: Check Unit
expectBrowseRootFolderToBePresented =
  expectPresented $ XPath.anywhere XPaths.browseRootFolderXPath

expectRemoveFileToBePresented :: String -> Check Unit
expectRemoveFileToBePresented =
  expectPresented <<< XPath.anywhere <<< XPaths.removeFileXPath

expectCreateNotebookToBePresented :: Check Unit
expectCreateNotebookToBePresented =
  expectPresented $ XPath.anywhere XPaths.createNotebookXPath

expectUntitledNotebookNameInputToBePresented :: Check Unit
expectUntitledNotebookNameInputToBePresented =
  expectPresentedWithProperties [Tuple "value" (Just "Untitled Notebook")] (XPath.anywhere "input")

--findSelectFileInputWithValue :: String -> Check Unit
--findSelectFileInputWithValue = findInputWithPlaceholderAndValue "Select a file"

expectMdFieldToBePresented :: Check Unit
expectMdFieldToBePresented =
  expectPresented $ XPath.anywhere xPath
  where
  xPath = XPaths.mdCellTitleXPath `XPath.following` XPaths.aceEditorXPath

expectMdPlayButtonToBePresented :: Check Unit
expectMdPlayButtonToBePresented =
  expectPresented $ XPath.anywhere xPath
  where
  xPath = XPaths.mdCellTitleXPath `XPath.following` XPaths.playXPath

expectExplorePlayButtonToBePresented :: Check Unit
expectExplorePlayButtonToBePresented =
  expectPresented $ XPath.anywhere xPath
  where
  xPath = XPaths.exploreCellTitleXPath `XPath.following` XPaths.playXPath

expectMdQueryPlayButtonToBePresented :: Check Unit
expectMdQueryPlayButtonToBePresented =
  expectPresented $ XPath.anywhere xPath
  where
  xPath = XPaths.mdQueryCellTitleXPath `XPath.following` XPaths.playXPath

expectCreateMdQueryCellButtonToBePresented :: Check Unit
expectCreateMdQueryCellButtonToBePresented =
  expectPresented
    $ XPath.anywhere $ XPath.anyWithExactAriaLabel "Insert Query cell after this cell"

expectMdQueryFieldToBePresented :: Check Unit
expectMdQueryFieldToBePresented = expectPresented $ XPath.anywhere xPath
  where
  xPath = XPaths.mdQueryCellTitleXPath `XPath.following` XPaths.aceEditorXPath

expectMdQueryColumnCellsWithIndexToBePresented :: Int -> Check Unit
expectMdQueryColumnCellsWithIndexToBePresented index =
  expectPresented $ XPath.index (XPath.anywhere xPath) index
  where
  xPath = XPaths.mdQueryCellTitleXPath `XPath.following` "tbody/tr/td"

expectFileToBePresented :: String -> Check Unit
expectFileToBePresented name = expectPresented $ XPath.anywhere $ XPath.anyWithExactText name

expectExploreErrorMessageToBePresented :: Check Unit
expectExploreErrorMessageToBePresented =
  expectPresented
    $ XPath.anywhere $ XPaths.exploreCellTitleXPath `XPath.following` XPaths.oneErrorMessageXPath

expectShowExploreMessagesToBePresented :: Check Unit
expectShowExploreMessagesToBePresented =
  expectPresented
    $ XPath.anywhere $ XPaths.exploreCellTitleXPath `XPath.following` XPaths.showMessagesXPath

expectNoFileSelectedMessageToBePresented :: Check Unit
expectNoFileSelectedMessageToBePresented =
  expectPresented $ XPath.anywhere XPaths.noFileSelectedMessageXPath

expectExploreFileFieldToBePresented :: Check Unit
expectExploreFileFieldToBePresented =
  expectPresented
    $ XPath.anywhere $ XPaths.exploreCellTitleXPath `XPath.following` XPaths.selectFileFieldXPath

expectFileDoesNotExistMessageToBePresented :: String -> Check Unit
expectFileDoesNotExistMessageToBePresented =
  expectPresented <<< XPath.anywhere <<< XPaths.fileDoesNotExistXPath

