module Test.Selenium.XPaths where

import Data.List (List(), fromFoldable)
import Data.String (joinWith)
import Prelude
import Test.XPath as XPath
import Test.Selenium.Notebook.Data (fileFromInitialFileList, initialFileList)

aceEditorXPath :: String
aceEditorXPath = "*[contains(@class, 'ace_editor') and not(contains(@class, 'ace_autocomplete'))]"

playXPath :: String
playXPath = XPath.anyWithExactAriaLabel "Play"

fileFromInitialFileListXPath :: String
fileFromInitialFileListXPath =
  XPath.anyWithExactText fileFromInitialFileList

initialFileListXPaths :: List String
initialFileListXPaths =
  map XPath.anyWithExactText initialFileList

initialFileListInOrderXPath :: String
initialFileListInOrderXPath =
  XPath.inOrder $ map XPath.anyWithExactText initialFileList

embedCellOutputXPath :: String
embedCellOutputXPath =
  XPath.anyWithExactText "Embed cell output"

embedCellOutputTitleXPath :: String
embedCellOutputTitleXPath =
  XPath.anyWithExactText "Embed cell"

embedCellOutputSnippetXPath :: String
embedCellOutputSnippetXPath =
  XPath.anyWithText "<script type=\"text/javascript\">"

exploreInputXPath :: String
exploreInputXPath =
  exploreCellTitleXPath `XPath.following` XPath.inputWithExactPlaceholder "Select a file"

insertQueryAfterThisXPath :: String
insertQueryAfterThisXPath =
  XPath.anyWithExactAriaLabel "Insert Query cell after this cell"

insertSearchAfterThisXPath :: String
insertSearchAfterThisXPath =
  XPath.anyWithExactAriaLabel "Insert Search cell after this cell"

insertVisualizeAfterThisXPath :: String
insertVisualizeAfterThisXPath =
  XPath.anyWithExactAriaLabel "Insert Visualize cell after this cell"

insertDownloadAfterThisXPath :: String
insertDownloadAfterThisXPath =
  XPath.anyWithExactAriaLabel "Insert Download cell after this cell"

insertQueryAfterMdXPath :: String
insertQueryAfterMdXPath =
  mdCellTitleXPath `XPath.following` insertQueryAfterThisXPath

showFileListXPath :: String
showFileListXPath =
  XPath.anyWithExactAriaLabel "Show file list"

hideFileListXPath :: String
hideFileListXPath =
  XPath.anyWithExactAriaLabel "Hide file list"

deleteCellXPath :: String
deleteCellXPath =
  XPath.anyWithExactAriaLabel "Delete cell"

queryCellTitleXPath :: String
queryCellTitleXPath =
  XPath.anyWithExactText "Query"

mdCellTitleXPath :: String
mdCellTitleXPath =
  XPath.anyWithExactText "Markdown"

exploreCellTitleXPath :: String
exploreCellTitleXPath =
  XPath.anyWithExactText "Explore"

searchCellTitleXPath :: String
searchCellTitleXPath =
  XPath.anyWithExactText "Search"

mdQueryCellTitleXPath :: String
mdQueryCellTitleXPath =
 mdCellTitleXPath `XPath.following` queryCellTitleXPath

dismissInsertCellMenuXPath :: String
dismissInsertCellMenuXPath =
  XPath.anyWithExactAriaLabel "Dismiss insert cell menu"

insertCellXPath :: String
insertCellXPath =
  XPath.anyWithExactAriaLabel "Insert cell"

cellTitleXPath :: String
cellTitleXPath =
  joinWith "|" [queryCellTitleXPath, mdCellTitleXPath, exploreCellTitleXPath, searchCellTitleXPath]

insertQueryCellXPath :: String
insertQueryCellXPath =
  XPath.anyWithExactAriaLabel "Insert Query cell"

insertMdCellXPath :: String
insertMdCellXPath =
  XPath.anyWithExactAriaLabel "Insert Markdown cell"

insertExploreCellXPath :: String
insertExploreCellXPath =
  XPath.anyWithExactAriaLabel "Insert Explore cell"

insertSearchCellXPath :: String
insertSearchCellXPath =
  XPath.anyWithExactAriaLabel "Insert Search cell"

hideCellOptionsXPath :: String
hideCellOptionsXPath =
  XPath.anyWithExactAriaLabel "Hide cell options"

showCellOptionsXPath :: String
showCellOptionsXPath =
  XPath.anyWithExactAriaLabel "Show cell options"

hideQueryCellOptionsXPath :: String
hideQueryCellOptionsXPath =
  queryCellTitleXPath `XPath.following` hideCellOptionsXPath

hideMdCellOptionsXPath :: String
hideMdCellOptionsXPath =
  mdCellTitleXPath `XPath.following` hideCellOptionsXPath

hideExploreCellOptionsXPath :: String
hideExploreCellOptionsXPath =
  exploreCellTitleXPath `XPath.following` hideCellOptionsXPath

hideSearchCellOptionsXPath :: String
hideSearchCellOptionsXPath =
  searchCellTitleXPath `XPath.following` hideCellOptionsXPath

showQueryCellOptionsXPath :: String
showQueryCellOptionsXPath =
  queryCellTitleXPath `XPath.following` showCellOptionsXPath

showMdCellOptionsXPath :: String
showMdCellOptionsXPath =
  mdCellTitleXPath `XPath.following` showCellOptionsXPath

showExploreCellOptionsXPath :: String
showExploreCellOptionsXPath =
  exploreCellTitleXPath `XPath.following` showCellOptionsXPath

showSearchCellOptionsXPath :: String
showSearchCellOptionsXPath =
  searchCellTitleXPath `XPath.following` showCellOptionsXPath

browseRootFolderXPath :: String
browseRootFolderXPath = XPath.anyWithAriaLabel "Browse root folder"

createNotebookXPath :: String
createNotebookXPath =
  XPath.anyWithExactAriaLabel "Create notebook"

removeFileXPath :: String -> String
removeFileXPath name =
  XPath.anyWithExactText name `XPath.following` XPath.anyWithExactAriaLabel "Remove"

oneErrorMessageXPath :: String
oneErrorMessageXPath =
  XPath.anyWithText "1 error during evaluation."

showMessagesXPath :: String
showMessagesXPath =
  XPath.anyWithExactAriaLabel "Show messages"

noFileSelectedMessageXPath :: String
noFileSelectedMessageXPath =
  XPath.anyWithExactText "No file selected"

fileDoesNotExistXPath :: String -> String
fileDoesNotExistXPath filename = XPath.anyWithExactText $ "File " ++ filename ++ " does not exist"

selectFileFieldXPath :: String
selectFileFieldXPath = XPath.inputWithPlaceholder "Select a file"

finishedMessageXPath :: String
finishedMessageXPath = XPath.anyWithText "Finished"
