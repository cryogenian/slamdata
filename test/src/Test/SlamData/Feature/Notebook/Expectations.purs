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

module Test.SlamData.Feature.Notebook.Expectations where

import Data.Traversable (traverse)
import Prelude
import Test.Feature (expectPresentedWithProperties, expectPresented, expectNotPresented)
import Test.SlamData.Feature.Monad (SlamFeature())
import Test.SlamData.Feature.Properties as Properties
import Test.SlamData.Feature.XPaths as XPaths
import XPath as XPath

expectInitialFileListToBePresentedInOrder :: SlamFeature Unit
expectInitialFileListToBePresentedInOrder =
  expectPresented $ XPath.anywhere XPaths.initialFileListInOrder

expectInitialFileListToBeHidden :: SlamFeature Unit
expectInitialFileListToBeHidden =
  void $ traverse expectNotPresented $ map XPath.anywhere XPaths.initialFileList

expectFileFromFileListToBePresented :: String -> SlamFeature Unit
expectFileFromFileListToBePresented =
  expectPresented <<< XPath.anywhere <<< XPath.anyWithExactText

expectEmbedCellOutputToBePresented :: SlamFeature Unit
expectEmbedCellOutputToBePresented =
  expectPresented $ XPath.anywhere XPaths.embedCellOutput

expectEmbedCellOutputToBeHidden :: SlamFeature Unit
expectEmbedCellOutputToBeHidden =
  expectNotPresented $ XPath.anywhere XPaths.embedCellOutput

expectInsertQueryAfterThisToBePresented :: SlamFeature Unit
expectInsertQueryAfterThisToBePresented =
  expectPresented $ XPath.anywhere XPaths.insertQueryAfterThis

expectInsertSearchAfterThisToBePresented :: SlamFeature Unit
expectInsertSearchAfterThisToBePresented =
  expectPresented $ XPath.anywhere XPaths.insertSearchAfterThis

expectInsertVisualizeAfterThisToBePresented :: SlamFeature Unit
expectInsertVisualizeAfterThisToBePresented =
  expectPresented $ XPath.anywhere XPaths.insertVisualizeAfterThis

expectInsertDownloadAfterThisToBePresented :: SlamFeature Unit
expectInsertDownloadAfterThisToBePresented =
  expectPresented $ XPath.anywhere XPaths.insertDownloadAfterThis

expectInsertQueryAfterThisToBeHidden :: SlamFeature Unit
expectInsertQueryAfterThisToBeHidden =
  expectNotPresented $ XPath.anywhere XPaths.insertQueryAfterThis

expectInsertSearchAfterThisToBeHidden :: SlamFeature Unit
expectInsertSearchAfterThisToBeHidden =
  expectNotPresented $ XPath.anywhere XPaths.insertSearchAfterThis

expectInsertVisualizeAfterThisToBeHidden :: SlamFeature Unit
expectInsertVisualizeAfterThisToBeHidden =
  expectNotPresented $ XPath.anywhere XPaths.insertVisualizeAfterThis

expectInsertDownloadAfterThisToBeHidden :: SlamFeature Unit
expectInsertDownloadAfterThisToBeHidden =
  expectNotPresented $ XPath.anywhere XPaths.insertDownloadAfterThis

expectInsertQueryAfterMdToBePresented :: SlamFeature Unit
expectInsertQueryAfterMdToBePresented =
  expectPresented $ XPath.anywhere XPaths.insertQueryAfterMd

expectEmbedCellOutputSnippetToBeHidden :: SlamFeature Unit
expectEmbedCellOutputSnippetToBeHidden =
  expectNotPresented $ XPath.anywhere XPaths.embedCellOutputSnippet

expectEmbedCellOutputTitleToBeHidden :: SlamFeature Unit
expectEmbedCellOutputTitleToBeHidden =
  expectNotPresented $ XPath.anywhere XPaths.embedCellOutputTitle

expectDismissInsertCellMenuToBePresented :: SlamFeature Unit
expectDismissInsertCellMenuToBePresented =
  expectPresented $ XPath.anywhere XPaths.dismissInsertCellMenu

expectInsertQueryCellToBePresented :: SlamFeature Unit
expectInsertQueryCellToBePresented =
  expectPresented $ XPath.anywhere XPaths.insertQueryCell

expectInsertMdCellToBePresented :: SlamFeature Unit
expectInsertMdCellToBePresented =
  expectPresented $ XPath.anywhere XPaths.insertMdCell

expectInsertExploreCellToBePresented :: SlamFeature Unit
expectInsertExploreCellToBePresented =
  expectPresented $ XPath.anywhere XPaths.insertExploreCell

expectInsertSearchCellToBePresented :: SlamFeature Unit
expectInsertSearchCellToBePresented =
  expectPresented $ XPath.anywhere XPaths.insertSearchCell

expectInsertCellToBePresented :: SlamFeature Unit
expectInsertCellToBePresented =
  expectPresented $ XPath.anywhere XPaths.insertCell

expectQueryCellTitlesToBePresented :: SlamFeature Unit
expectQueryCellTitlesToBePresented =
  expectPresented $ XPath.anywhere XPaths.queryCellTitle

expectExploreCellTitlesToBePresented :: SlamFeature Unit
expectExploreCellTitlesToBePresented =
  expectPresented $ XPath.anywhere XPaths.exploreCellTitle

expectSearchCellTitlesToBePresented :: SlamFeature Unit
expectSearchCellTitlesToBePresented =
  expectPresented $ XPath.anywhere XPaths.searchCellTitle

expectMdCellTitlesToBePresented :: SlamFeature Unit
expectMdCellTitlesToBePresented =
  expectPresented $ XPath.anywhere XPaths.mdCellTitle

expectCellTitlesToBeHidden :: SlamFeature Unit
expectCellTitlesToBeHidden =
  expectNotPresented $ XPath.anywhere XPaths.cellTitle

expectQueryCellTitlesToBeHidden :: SlamFeature Unit
expectQueryCellTitlesToBeHidden =
  expectNotPresented $ XPath.anywhere XPaths.queryCellTitle

expectExploreCellTitlesToBeHidden :: SlamFeature Unit
expectExploreCellTitlesToBeHidden =
  expectNotPresented $ XPath.anywhere XPaths.exploreCellTitle

expectSearchCellTitlesToBeHidden :: SlamFeature Unit
expectSearchCellTitlesToBeHidden =
  expectNotPresented $ XPath.anywhere XPaths.searchCellTitle

expectMdCellTitlesToBeHidden :: SlamFeature Unit
expectMdCellTitlesToBeHidden =
  expectNotPresented $ XPath.anywhere XPaths.mdCellTitle

expectShowFileListToBePresented :: SlamFeature Unit
expectShowFileListToBePresented =
  expectPresented $ XPath.anywhere XPaths.showFileList

expectHideFileListToBePresented :: SlamFeature Unit
expectHideFileListToBePresented =
  expectPresented $ XPath.anywhere XPaths.hideFileList

expectDeleteCellOptionsToBePresented :: SlamFeature Unit
expectDeleteCellOptionsToBePresented =
  expectPresented $ XPath.anywhere XPaths.deleteCell

expectIndexedQueryCellTitleToBePresented :: Int -> SlamFeature Unit
expectIndexedQueryCellTitleToBePresented =
  expectPresented <<< XPath.index (XPath.anywhere XPaths.queryCellTitle)

expectIndexedMdCellTitleToBePresented :: Int -> SlamFeature Unit
expectIndexedMdCellTitleToBePresented =
  expectPresented <<< XPath.index (XPath.anywhere XPaths.mdCellTitle)

expectIndexedExploreCellTitleToBePresented :: Int -> SlamFeature Unit
expectIndexedExploreCellTitleToBePresented =
  expectPresented <<< XPath.index (XPath.anywhere XPaths.exploreCellTitle)

expectIndexedSearchCellTitleToBePresented :: Int -> SlamFeature Unit
expectIndexedSearchCellTitleToBePresented =
  expectPresented <<< XPath.index (XPath.anywhere XPaths.searchCellTitle)

expectHideQueryCellOptionsToBePresented :: SlamFeature Unit
expectHideQueryCellOptionsToBePresented =
  expectPresented $ XPath.anywhere XPaths.hideQueryCellOptions

expectHideMdCellOptionsToBePresented :: SlamFeature Unit
expectHideMdCellOptionsToBePresented =
  expectPresented $ XPath.anywhere XPaths.hideMdCellOptions

expectHideExploreCellOptionsToBePresented :: SlamFeature Unit
expectHideExploreCellOptionsToBePresented =
  expectPresented $ XPath.anywhere XPaths.hideExploreCellOptions

expectHideSearchCellOptionsToBePresented :: SlamFeature Unit
expectHideSearchCellOptionsToBePresented =
  expectPresented $ XPath.anywhere XPaths.hideSearchCellOptions

expectShowQueryCellOptionsToBePresented :: SlamFeature Unit
expectShowQueryCellOptionsToBePresented =
  expectPresented $ XPath.anywhere XPaths.showQueryCellOptions

expectShowMdCellOptionsToBePresented :: SlamFeature Unit
expectShowMdCellOptionsToBePresented =
  expectPresented $ XPath.anywhere XPaths.showMdCellOptions

expectShowExploreCellOptionsToBePresented :: SlamFeature Unit
expectShowExploreCellOptionsToBePresented =
  expectPresented $ XPath.anywhere XPaths.showExploreCellOptions

expectShowSearchCellOptionsToBePresented :: SlamFeature Unit
expectShowSearchCellOptionsToBePresented =
  expectPresented $ XPath.anywhere XPaths.showSearchCellOptions

expectExploreInputToBePresented :: SlamFeature Unit
expectExploreInputToBePresented =
  expectPresented $ XPath.anywhere XPaths.exploreInput

expectExploreInputToBeHidden :: SlamFeature Unit
expectExploreInputToBeHidden =
  expectNotPresented $ XPath.anywhere XPaths.exploreInput

expectBrowseRootFolderToBePresented :: SlamFeature Unit
expectBrowseRootFolderToBePresented =
  expectPresented $ XPath.anywhere XPaths.browseRootFolder

expectRemoveFileToBePresented :: String -> SlamFeature Unit
expectRemoveFileToBePresented =
  expectPresented <<< XPath.anywhere <<< XPaths.removeFile

expectCreateNotebookToBePresented :: SlamFeature Unit
expectCreateNotebookToBePresented =
  expectPresented $ XPath.anywhere XPaths.createNotebook

expectUntitledNotebookNameInputToBePresented :: SlamFeature Unit
expectUntitledNotebookNameInputToBePresented =
  expectPresentedWithProperties [Properties.untitledNotebookValue] $ XPath.anywhere "input"

--findSelectFileInputWithValue :: String -> SlamFeature Unit
--findSelectFileInputWithValue = findInputWithPlaceholderAndValue "Select a file"

expectMdFieldToBePresented :: SlamFeature Unit
expectMdFieldToBePresented =
  expectPresented $ XPath.anywhere XPaths.mdField

expectMdPlayButtonToBePresented :: SlamFeature Unit
expectMdPlayButtonToBePresented =
  expectPresented $ XPath.anywhere XPaths.mdPlayButton

expectExplorePlayButtonToBePresented :: SlamFeature Unit
expectExplorePlayButtonToBePresented =
  expectPresented $ XPath.anywhere xPath
  where
  xPath = XPaths.exploreCellTitle `XPath.following` XPaths.play

expectMdQueryPlayButtonToBePresented :: SlamFeature Unit
expectMdQueryPlayButtonToBePresented =
  expectPresented $ XPath.anywhere XPaths.mdQueryPlayButton

expectCreateMdQueryCellButtonToBePresented :: SlamFeature Unit
expectCreateMdQueryCellButtonToBePresented =
  expectPresented
    $ XPath.anywhere $ XPath.anyWithExactAriaLabel "Insert Query cell after this cell"

expectMdQueryFieldToBePresented :: SlamFeature Unit
expectMdQueryFieldToBePresented = expectPresented $ XPath.anywhere xPath
  where
  xPath = XPaths.mdQueryCellTitle `XPath.following` XPaths.aceEditor

expectMdQueryColumnCellsWithIndexToBePresented :: Int -> SlamFeature Unit
expectMdQueryColumnCellsWithIndexToBePresented index =
  expectPresented $ XPath.index (XPath.anywhere xPath) index
  where
  xPath = XPaths.mdQueryCellTitle `XPath.following` "tbody/tr/td"

expectFileToBePresented :: String -> SlamFeature Unit
expectFileToBePresented name = expectPresented $ XPath.anywhere $ XPath.anyWithExactText name

expectExploreErrorMessageToBePresented :: SlamFeature Unit
expectExploreErrorMessageToBePresented =
  expectPresented
    $ XPath.anywhere $ XPaths.exploreCellTitle `XPath.following` XPaths.oneErrorMessage

expectShowExploreMessagesToBePresented :: SlamFeature Unit
expectShowExploreMessagesToBePresented =
  expectPresented
    $ XPath.anywhere $ XPaths.exploreCellTitle `XPath.following` XPaths.showMessages

expectNoFileSelectedMessageToBePresented :: SlamFeature Unit
expectNoFileSelectedMessageToBePresented =
  expectPresented $ XPath.anywhere XPaths.noFileSelectedMessage

expectExploreFileFieldToBePresented :: SlamFeature Unit
expectExploreFileFieldToBePresented =
  expectPresented
    $ XPath.anywhere $ XPaths.exploreCellTitle `XPath.following` XPaths.selectFileField

expectFileDoesNotExistMessageToBePresented :: String -> SlamFeature Unit
expectFileDoesNotExistMessageToBePresented =
  expectPresented <<< XPath.anywhere <<< XPaths.fileDoesNotExist

