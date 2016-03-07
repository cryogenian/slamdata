module Test.Selenium.XPaths where

import Data.List (List(), fromFoldable)
import Data.String (joinWith)
import Prelude
import Test.XPath as XPath
import Test.Selenium.Notebook.Data as Data

aceEditor :: String
aceEditor = "*[contains(@class, 'ace_editor') and not(contains(@class, 'ace_autocomplete'))]"

play :: String
play = XPath.anyWithExactAriaLabel "Play"

fileFromInitialFileList :: String
fileFromInitialFileList =
  XPath.anyWithExactText Data.fileFromInitialFileList

initialFileList :: List String
initialFileList =
  map XPath.anyWithExactText Data.initialFileList

initialFileListInOrder :: String
initialFileListInOrder =
  XPath.inOrder $ map XPath.anyWithExactText Data.initialFileList

embedCellOutput :: String
embedCellOutput =
  XPath.anyWithExactText "Embed cell output"

embedCellOutputTitle :: String
embedCellOutputTitle =
  XPath.anyWithExactText "Embed cell"

embedCellOutputSnippet :: String
embedCellOutputSnippet =
  XPath.anyWithText "<script type=\"text/javascript\">"

exploreInput :: String
exploreInput =
  exploreCellTitle `XPath.following` XPath.inputWithExactPlaceholder "Select a file"

insertQueryAfterThis :: String
insertQueryAfterThis =
  XPath.anyWithExactAriaLabel "Insert Query cell after this cell"

insertSearchAfterThis :: String
insertSearchAfterThis =
  XPath.anyWithExactAriaLabel "Insert Search cell after this cell"

insertVisualizeAfterThis :: String
insertVisualizeAfterThis =
  XPath.anyWithExactAriaLabel "Insert Visualize cell after this cell"

insertDownloadAfterThis :: String
insertDownloadAfterThis =
  XPath.anyWithExactAriaLabel "Insert Download cell after this cell"

insertQueryAfterMd :: String
insertQueryAfterMd =
  mdCellTitle `XPath.following` insertQueryAfterThis

showFileList :: String
showFileList =
  XPath.anyWithExactAriaLabel "Show file list"

hideFileList :: String
hideFileList =
  XPath.anyWithExactAriaLabel "Hide file list"

deleteCell :: String
deleteCell =
  XPath.anyWithExactAriaLabel "Delete cell"

queryCellTitle :: String
queryCellTitle =
  XPath.anyWithExactText "Query"

mdCellTitle :: String
mdCellTitle =
  XPath.anyWithExactText "Markdown"

exploreCellTitle :: String
exploreCellTitle =
  XPath.anyWithExactText "Explore"

searchCellTitle :: String
searchCellTitle =
  XPath.anyWithExactText "Search"

mdQueryCellTitle :: String
mdQueryCellTitle =
 mdCellTitle `XPath.following` queryCellTitle

dismissInsertCellMenu :: String
dismissInsertCellMenu =
  XPath.anyWithExactAriaLabel "Dismiss insert cell menu"

insertCell :: String
insertCell =
  XPath.anyWithExactAriaLabel "Insert cell"

cellTitle :: String
cellTitle =
  joinWith "|" [queryCellTitle, mdCellTitle, exploreCellTitle, searchCellTitle]

insertQueryCell :: String
insertQueryCell =
  XPath.anyWithExactAriaLabel "Insert Query cell"

insertMdCell :: String
insertMdCell =
  XPath.anyWithExactAriaLabel "Insert Markdown cell"

insertExploreCell :: String
insertExploreCell = XPath.anyWithExactAriaLabel "Insert Explore cell"

insertSearchCell :: String
insertSearchCell =
  XPath.anyWithExactAriaLabel "Insert Search cell"

hideCellOptions :: String
hideCellOptions =
  XPath.anyWithExactAriaLabel "Hide cell options"

showCellOptions :: String
showCellOptions =
  XPath.anyWithExactAriaLabel "Show cell options"

hideQueryCellOptions :: String
hideQueryCellOptions =
  queryCellTitle `XPath.following` hideCellOptions

hideMdCellOptions :: String
hideMdCellOptions =
  mdCellTitle `XPath.following` hideCellOptions

hideExploreCellOptions :: String
hideExploreCellOptions =
  exploreCellTitle `XPath.following` hideCellOptions

hideSearchCellOptions :: String
hideSearchCellOptions =
  searchCellTitle `XPath.following` hideCellOptions

showQueryCellOptions :: String
showQueryCellOptions =
  queryCellTitle `XPath.following` showCellOptions

showMdCellOptions :: String
showMdCellOptions =
  mdCellTitle `XPath.following` showCellOptions

showExploreCellOptions :: String
showExploreCellOptions =
  exploreCellTitle `XPath.following` showCellOptions

showSearchCellOptions :: String
showSearchCellOptions =
  searchCellTitle `XPath.following` showCellOptions

browseRootFolder :: String
browseRootFolder = XPath.anyWithAriaLabel "Browse root folder"

createNotebook :: String
createNotebook =
  XPath.anyWithExactAriaLabel "Create notebook"

removeFile :: String -> String
removeFile name =
  XPath.anyWithExactText name `XPath.following` XPath.anyWithExactAriaLabel "Remove"

oneErrorMessage :: String
oneErrorMessage =
  XPath.anyWithText "1 error during evaluation."

showMessages :: String
showMessages =
  XPath.anyWithExactAriaLabel "Show messages"

noFileSelectedMessage :: String
noFileSelectedMessage =
  XPath.anyWithExactText "No file selected"

fileDoesNotExist :: String -> String
fileDoesNotExist filename = XPath.anyWithExactText $ "File " ++ filename ++ " does not exist"

selectFileField :: String
selectFileField = XPath.inputWithPlaceholder "Select a file"

finishedMessage :: String
finishedMessage = XPath.anyWithText "Finished"

mdFinishedMessage :: String
mdFinishedMessage = mdCellTitle `XPath.following` finishedMessage

mdField :: String
mdField = mdCellTitle `XPath.following` aceEditor

mdPlayButton :: String
mdPlayButton = mdCellTitle `XPath.following` play

mdQueryPlayButton :: String
mdQueryPlayButton = mdQueryCellTitle `XPath.following` play

accessMountDatabase :: String
accessMountDatabase = XPath.anyWithExactAriaLabel "Mount database"

mountName :: String
mountName = "input" `XPath.withLabelWithExactText` "Name"

mountType :: String
mountType = "select" `XPath.withLabelWithExactText` "Mount type"

mountPort :: String
mountPort = "input" `XPath.withLabelWithExactText` "Port"

mountHost :: String
mountHost = "input" `XPath.withLabelWithExactText` "Host"

mountDatabase :: String
mountDatabase = "input" `XPath.withLabelWithExactText` "Database"

mountButton :: String
mountButton = "button" `XPath.nodeWithExactText` "Mount"
