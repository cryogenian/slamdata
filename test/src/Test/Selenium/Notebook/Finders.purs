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

import Data.Traversable (traverse)
import Data.Maybe (Maybe(..))
import Prelude
import Test.Selenium.Finders (expectPresentedByXPathAndProperty, expectPresentedByXPath, expectPresentedByXPath, expectPresentedByXPath, expectPresentedByXPath, expectHiddenByXPath)
import Test.Selenium.XPaths as XPaths
import Test.XPath as XPath
import Test.Selenium.Monad (Check())

expectInitialFileListToBePresentedInOrder :: Check Unit
expectInitialFileListToBePresentedInOrder =
  expectPresentedByXPath $ XPath.anywhere XPaths.initialFileListInOrderXPath

expectInitialFileListToBeHidden :: Check Unit
expectInitialFileListToBeHidden =
  void $ traverse expectHiddenByXPath $ map XPath.anywhere XPaths.initialFileListXPaths

expectFileFromFileListToBePresented :: String -> Check Unit
expectFileFromFileListToBePresented =
  expectPresentedByXPath <<< XPath.anywhere <<< XPath.anyWithExactText

expectEmbedCellOutputToBePresented :: Check Unit
expectEmbedCellOutputToBePresented =
  expectPresentedByXPath $ XPath.anywhere XPaths.embedCellOutputXPath

expectEmbedCellOutputToBeHidden :: Check Unit
expectEmbedCellOutputToBeHidden =
  expectHiddenByXPath $ XPath.anywhere XPaths.embedCellOutputXPath

expectInsertQueryAfterThisToBePresented :: Check Unit
expectInsertQueryAfterThisToBePresented =
  expectPresentedByXPath $ XPath.anywhere XPaths.insertQueryAfterThisXPath

expectInsertSearchAfterThisToBePresented :: Check Unit
expectInsertSearchAfterThisToBePresented =
  expectPresentedByXPath $ XPath.anywhere XPaths.insertSearchAfterThisXPath

expectInsertVisualizeAfterThisToBePresented :: Check Unit
expectInsertVisualizeAfterThisToBePresented =
  expectPresentedByXPath $ XPath.anywhere XPaths.insertVisualizeAfterThisXPath

expectInsertDownloadAfterThisToBePresented :: Check Unit
expectInsertDownloadAfterThisToBePresented =
  expectPresentedByXPath $ XPath.anywhere XPaths.insertDownloadAfterThisXPath

expectInsertQueryAfterThisToBeHidden :: Check Unit
expectInsertQueryAfterThisToBeHidden =
  expectHiddenByXPath $ XPath.anywhere XPaths.insertQueryAfterThisXPath

expectInsertSearchAfterThisToBeHidden :: Check Unit
expectInsertSearchAfterThisToBeHidden =
  expectHiddenByXPath $ XPath.anywhere XPaths.insertSearchAfterThisXPath

expectInsertVisualizeAfterThisToBeHidden :: Check Unit
expectInsertVisualizeAfterThisToBeHidden =
  expectHiddenByXPath $ XPath.anywhere XPaths.insertVisualizeAfterThisXPath

expectInsertDownloadAfterThisToBeHidden :: Check Unit
expectInsertDownloadAfterThisToBeHidden =
  expectHiddenByXPath $ XPath.anywhere XPaths.insertDownloadAfterThisXPath

expectInsertQueryAfterMdToBePresented :: Check Unit
expectInsertQueryAfterMdToBePresented =
  expectPresentedByXPath $ XPath.anywhere XPaths.insertQueryAfterMdXPath

expectEmbedCellOutputSnippetToBeHidden :: Check Unit
expectEmbedCellOutputSnippetToBeHidden =
  expectHiddenByXPath $ XPath.anywhere XPaths.embedCellOutputSnippetXPath

expectEmbedCellOutputTitleToBeHidden :: Check Unit
expectEmbedCellOutputTitleToBeHidden =
  expectHiddenByXPath $ XPath.anywhere XPaths.embedCellOutputTitleXPath

expectDismissInsertCellMenuToBePresented :: Check Unit
expectDismissInsertCellMenuToBePresented =
  expectPresentedByXPath $ XPath.anywhere XPaths.dismissInsertCellMenuXPath

expectInsertQueryCellToBePresented :: Check Unit
expectInsertQueryCellToBePresented =
  expectPresentedByXPath $ XPath.anywhere XPaths.insertQueryCellXPath

expectInsertMdCellToBePresented :: Check Unit
expectInsertMdCellToBePresented =
  expectPresentedByXPath $ XPath.anywhere XPaths.insertMdCellXPath

expectInsertExploreCellToBePresented :: Check Unit
expectInsertExploreCellToBePresented =
  expectPresentedByXPath $ XPath.anywhere XPaths.insertExploreCellXPath

expectInsertSearchCellToBePresented :: Check Unit
expectInsertSearchCellToBePresented =
  expectPresentedByXPath $ XPath.anywhere XPaths.insertSearchCellXPath

expectInsertCellToBePresented :: Check Unit
expectInsertCellToBePresented =
  expectPresentedByXPath $ XPath.anywhere XPaths.insertCellXPath

expectQueryCellTitlesToBePresented :: Check Unit
expectQueryCellTitlesToBePresented =
  expectPresentedByXPath $ XPath.anywhere XPaths.queryCellTitleXPath

expectExploreCellTitlesToBePresented :: Check Unit
expectExploreCellTitlesToBePresented =
  expectPresentedByXPath $ XPath.anywhere XPaths.exploreCellTitleXPath

expectSearchCellTitlesToBePresented :: Check Unit
expectSearchCellTitlesToBePresented =
  expectPresentedByXPath $ XPath.anywhere XPaths.searchCellTitleXPath

expectMdCellTitlesToBePresented :: Check Unit
expectMdCellTitlesToBePresented =
  expectPresentedByXPath $ XPath.anywhere XPaths.mdCellTitleXPath

expectCellTitlesToBeHidden :: Check Unit
expectCellTitlesToBeHidden =
  expectHiddenByXPath $ XPath.anywhere XPaths.cellTitleXPath

expectQueryCellTitlesToBeHidden :: Check Unit
expectQueryCellTitlesToBeHidden =
  expectHiddenByXPath $ XPath.anywhere XPaths.queryCellTitleXPath

expectExploreCellTitlesToBeHidden :: Check Unit
expectExploreCellTitlesToBeHidden =
  expectHiddenByXPath $ XPath.anywhere XPaths.exploreCellTitleXPath

expectSearchCellTitlesToBeHidden :: Check Unit
expectSearchCellTitlesToBeHidden =
  expectHiddenByXPath $ XPath.anywhere XPaths.searchCellTitleXPath

expectMdCellTitlesToBeHidden :: Check Unit
expectMdCellTitlesToBeHidden =
  expectHiddenByXPath $ XPath.anywhere XPaths.mdCellTitleXPath

expectShowFileListToBePresented :: Check Unit
expectShowFileListToBePresented =
  expectPresentedByXPath $ XPath.anywhere XPaths.showFileListXPath

expectHideFileListToBePresented :: Check Unit
expectHideFileListToBePresented =
  expectPresentedByXPath $ XPath.anywhere XPaths.hideFileListXPath

expectDeleteCellOptionsToBePresented :: Check Unit
expectDeleteCellOptionsToBePresented =
  expectPresentedByXPath $ XPath.anywhere XPaths.deleteCellXPath

expectIndexedQueryCellTitleToBePresented :: Int -> Check Unit
expectIndexedQueryCellTitleToBePresented =
  expectPresentedByXPath <<< XPath.index (XPath.anywhere XPaths.queryCellTitleXPath)

expectIndexedMdCellTitleToBePresented :: Int -> Check Unit
expectIndexedMdCellTitleToBePresented =
  expectPresentedByXPath <<< XPath.index (XPath.anywhere XPaths.mdCellTitleXPath)

expectIndexedExploreCellTitleToBePresented :: Int -> Check Unit
expectIndexedExploreCellTitleToBePresented =
  expectPresentedByXPath <<< XPath.index (XPath.anywhere XPaths.exploreCellTitleXPath)

expectIndexedSearchCellTitleToBePresented :: Int -> Check Unit
expectIndexedSearchCellTitleToBePresented =
  expectPresentedByXPath <<< XPath.index (XPath.anywhere XPaths.searchCellTitleXPath)

expectHideQueryCellOptionsToBePresented :: Check Unit
expectHideQueryCellOptionsToBePresented =
  expectPresentedByXPath $ XPath.anywhere XPaths.hideQueryCellOptionsXPath

expectHideMdCellOptionsToBePresented :: Check Unit
expectHideMdCellOptionsToBePresented =
  expectPresentedByXPath $ XPath.anywhere XPaths.hideMdCellOptionsXPath

expectHideExploreCellOptionsToBePresented :: Check Unit
expectHideExploreCellOptionsToBePresented =
  expectPresentedByXPath $ XPath.anywhere XPaths.hideExploreCellOptionsXPath

expectHideSearchCellOptionsToBePresented :: Check Unit
expectHideSearchCellOptionsToBePresented =
  expectPresentedByXPath $ XPath.anywhere XPaths.hideSearchCellOptionsXPath

expectShowQueryCellOptionsToBePresented :: Check Unit
expectShowQueryCellOptionsToBePresented =
  expectPresentedByXPath $ XPath.anywhere XPaths.showQueryCellOptionsXPath

expectShowMdCellOptionsToBePresented :: Check Unit
expectShowMdCellOptionsToBePresented =
  expectPresentedByXPath $ XPath.anywhere XPaths.showMdCellOptionsXPath

expectShowExploreCellOptionsToBePresented :: Check Unit
expectShowExploreCellOptionsToBePresented =
  expectPresentedByXPath $ XPath.anywhere XPaths.showExploreCellOptionsXPath

expectShowSearchCellOptionsToBePresented :: Check Unit
expectShowSearchCellOptionsToBePresented =
  expectPresentedByXPath $ XPath.anywhere XPaths.showSearchCellOptionsXPath

expectExploreInputToBePresented :: Check Unit
expectExploreInputToBePresented =
  expectPresentedByXPath $ XPath.anywhere XPaths.exploreInputXPath

expectExploreInputToBeHidden :: Check Unit
expectExploreInputToBeHidden =
  expectHiddenByXPath $ XPath.anywhere XPaths.exploreInputXPath

expectBrowseRootFolderToBePresented :: Check Unit
expectBrowseRootFolderToBePresented =
  expectPresentedByXPath $ XPath.anywhere XPaths.browseRootFolderXPath

expectRemoveFileToBePresented :: String -> Check Unit
expectRemoveFileToBePresented =
  expectPresentedByXPath <<< XPath.anywhere <<< XPaths.removeFileXPath

expectCreateNotebookToBePresented :: Check Unit
expectCreateNotebookToBePresented =
  expectPresentedByXPath $ XPath.anywhere XPaths.createNotebookXPath

expectUntitledNotebookNameInputToBePresented :: Check Unit
expectUntitledNotebookNameInputToBePresented =
  expectPresentedByXPathAndProperty "value" (Just "Untitled Notebook") (XPath.anywhere "input")

--findSelectFileInputWithValue :: String -> Check Unit
--findSelectFileInputWithValue = findInputWithPlaceholderAndValue "Select a file"

expectMdFieldToBePresented :: Check Unit
expectMdFieldToBePresented =
  expectPresentedByXPath $ XPath.anywhere xPath
  where
  xPath = XPaths.mdCellTitleXPath `XPath.following` XPaths.aceEditorXPath

expectMdPlayButtonToBePresented :: Check Unit
expectMdPlayButtonToBePresented =
  expectPresentedByXPath $ XPath.anywhere xPath
  where
  xPath = XPaths.mdCellTitleXPath `XPath.following` XPaths.playXPath

expectExplorePlayButtonToBePresented :: Check Unit
expectExplorePlayButtonToBePresented =
  expectPresentedByXPath $ XPath.anywhere xPath
  where
  xPath = XPaths.exploreCellTitleXPath `XPath.following` XPaths.playXPath

expectMdQueryPlayButtonToBePresented :: Check Unit
expectMdQueryPlayButtonToBePresented =
  expectPresentedByXPath $ XPath.anywhere xPath
  where
  xPath = XPaths.mdQueryCellTitleXPath `XPath.following` XPaths.playXPath

expectCreateMdQueryCellButtonToBePresented :: Check Unit
expectCreateMdQueryCellButtonToBePresented =
  expectPresentedByXPath
    $ XPath.anywhere $ XPath.anyWithExactAriaLabel "Insert Query cell after this cell"

expectMdQueryFieldToBePresented :: Check Unit
expectMdQueryFieldToBePresented = expectPresentedByXPath $ XPath.anywhere xPath
  where
  xPath = XPaths.mdQueryCellTitleXPath `XPath.following` XPaths.aceEditorXPath

expectMdQueryColumnCellsWithIndexToBePresented :: Int -> Check Unit
expectMdQueryColumnCellsWithIndexToBePresented index =
  expectPresentedByXPath $ XPath.index (XPath.anywhere xPath) index
  where
  xPath = XPaths.mdQueryCellTitleXPath `XPath.following` "tbody/tr/td"

expectFileToBePresented :: String -> Check Unit
expectFileToBePresented name = expectPresentedByXPath $ XPath.anywhere $ XPath.anyWithExactText name

expectExploreErrorMessageToBePresented :: Check Unit
expectExploreErrorMessageToBePresented =
  expectPresentedByXPath
    $ XPath.anywhere $ XPaths.exploreCellTitleXPath `XPath.following` XPaths.oneErrorMessageXPath

expectShowExploreMessagesToBePresented :: Check Unit
expectShowExploreMessagesToBePresented =
  expectPresentedByXPath
    $ XPath.anywhere $ XPaths.exploreCellTitleXPath `XPath.following` XPaths.showMessagesXPath

expectNoFileSelectedMessageToBePresented :: Check Unit
expectNoFileSelectedMessageToBePresented =
  expectPresentedByXPath $ XPath.anywhere XPaths.noFileSelectedMessageXPath

expectExploreFileFieldToBePresented :: Check Unit
expectExploreFileFieldToBePresented =
  expectPresentedByXPath
    $ XPath.anywhere $ XPaths.exploreCellTitleXPath `XPath.following` XPaths.selectFileFieldXPath

expectFileDoesNotExistMessageToBePresented :: String -> Check Unit
expectFileDoesNotExistMessageToBePresented =
  expectPresentedByXPath <<< XPath.anywhere <<< XPaths.fileDoesNotExistXPath

