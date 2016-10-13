module Test.SlamData.Feature.XPaths where

import SlamData.Prelude
import XPath as XPath
import Test.SlamData.Feature.Data as Data

aceEditor ∷ String
aceEditor = "*[contains(@class, 'ace_editor') and not(contains(@class, 'ace_autocomplete'))]"

playButton ∷ String
playButton = XPath.anyWithExactAriaLabel "Play"

renameButton ∷ String
renameButton = XPath.anyWithExactText "Rename"

renameDeck ∷ String
renameDeck = XPath.anyWithExactAriaLabel "Rename deck"

refreshButton ∷ String
refreshButton = XPath.anyWithExactAriaLabel "Refresh"

uploadFile ∷ String
uploadFile =
  "input" `XPath.withLabel` XPath.anyWithExactAriaLabel "Upload file"

selectADestinationFolder ∷ String
selectADestinationFolder = XPath.anyWithExactAriaLabel "Select a destination folder"

fileFromInitialFileList ∷ String
fileFromInitialFileList =
  XPath.anyWithExactText Data.fileFromInitialFileList

initialFileList ∷ Array String
initialFileList =
  map XPath.anyWithExactText Data.initialFileList

initialFileListInOrder ∷ String
initialFileListInOrder =
  XPath.inOrder $ map XPath.anyWithExactText Data.initialFileList

eChart ∷ String → String
eChart string =
  ("pre" `XPath.nodeWithExactText` string)

displayMarkdownCardHeader ∷ String
displayMarkdownCardHeader =
  XPath.anyWithExactAriaLabel "Show Markdown card"

troubleshootCardHeader ∷ String
troubleshootCardHeader =
  XPath.anyWithExactAriaLabel "Troubleshoot card"

tableCardHeader ∷ String
tableCardHeader =
  XPath.anyWithExactAriaLabel "Show Table card"

fileSearchInput ∷ String
fileSearchInput =
  XPath.anyWithExactAriaLabel "File search field"

searchStringInput ∷ String
searchStringInput =
  XPath.inputWithExactPlaceholder "Search string"

saveDestinationInput ∷ String
saveDestinationInput =
  XPath.anyWithExactAriaLabel "Cache file destination"

saveSubmitButton ∷ String
saveSubmitButton =
  XPath.anyWithExactAriaLabel "Confirm file destination"


insertCacheCard ∷ String
insertCacheCard =
  XPath.anyWithExactAriaLabel "Insert Cache card"

insertMdCard ∷ String
insertMdCard =
  XPath.anyWithExactAriaLabel "Insert Setup Markdown card"

insertSearchCard ∷ String
insertSearchCard =
  XPath.anyWithExactAriaLabel "Insert Search card"

insertChartOptionsCard ∷ String
insertChartOptionsCard =
  XPath.anyWithExactAriaLabel "Insert Setup Chart card"

insertDownloadOptionsCard ∷ String
insertDownloadOptionsCard =
  XPath.anyWithExactAriaLabel "Insert Setup Download card"

insertTroubleshootCard ∷ String
insertTroubleshootCard =
  XPath.anyWithExactAriaLabel "Insert Troubleshoot card"

insertChartCard ∷ String
insertChartCard =
  XPath.anyWithExactAriaLabel "Insert Show Chart card"

insertDisplayMarkdownCard ∷ String
insertDisplayMarkdownCard =
  XPath.anyWithExactAriaLabel "Insert Show Markdown card"

selectBuildChart ∷ String
selectBuildChart =
  XPath.anyWithExactAriaLabel "Select Setup Chart card category"

insertPivotCard ∷ String
insertPivotCard =
  XPath.anyWithExactAriaLabel "Insert Setup Pivot card"

insertBuildBarChartCard ∷ String
insertBuildBarChartCard =
  XPath.anyWithExactAriaLabel "Insert Setup Bar card"

showFileList ∷ String
showFileList =
  XPath.anyWithExactAriaLabel "Show file list"

hideFileList ∷ String
hideFileList =
  XPath.anyWithExactAriaLabel "Hide file list"

deleteCard ∷ String
deleteCard =
  XPath.anyWithExactAriaLabel "Delete card"

dismissInsertCardMenu ∷ String
dismissInsertCardMenu =
  XPath.anyWithExactAriaLabel "Dismiss insert card menu"

insertCard ∷ String
insertCard =
  XPath.anyWithExactAriaLabel "Insert card"

insertQueryCard ∷ String
insertQueryCard =
  XPath.anyWithExactAriaLabel "Insert Query card"

insertOpenCard ∷ String
insertOpenCard = XPath.anyWithExactAriaLabel "Insert Open card"

insertVariablesCard ∷ String
insertVariablesCard =
  XPath.anyWithExactAriaLabel "Insert Setup Variables card"

insertTableCard ∷ String
insertTableCard =
  XPath.anyWithExactAriaLabel "Insert Show Table card"

hideCardOptions ∷ String
hideCardOptions =
  XPath.anyWithExactAriaLabel "Hide card options"

showCardOptions ∷ String
showCardOptions =
  XPath.anyWithExactAriaLabel "Show card options"

browseRootFolder ∷ String
browseRootFolder = XPath.anyWithAriaLabel "Browse root folder"

createWorkspace ∷ String
createWorkspace =
  XPath.anyWithExactAriaLabel "Create workspace"

createFolder ∷ String
createFolder =
  XPath.anyWithExactAriaLabel "Create folder"

removeFile ∷ String → String
removeFile name =
  XPath.anyWithExactText name `XPath.following` XPath.anyWithExactAriaLabel "Remove"

shareFile ∷ String → String
shareFile name =
  XPath.anyWithExactText name `XPath.following` XPath.anyWithExactAriaLabel "Share"

downloadFile ∷ String → String
downloadFile name =
  XPath.anyWithExactText name `XPath.following` XPath.anyWithExactAriaLabel "Download"

moveFile ∷ String → String
moveFile name =
  XPath.anyWithExactText name `XPath.following` XPath.anyWithExactAriaLabel "Move / rename"

oneErrorMessage ∷ String
oneErrorMessage =
  XPath.anyWithText "1 error during evaluation."

noFileSelectedMessage ∷ String
noFileSelectedMessage =
  XPath.anyWithExactText "No file selected"

fileDoesNotExist ∷ String → String
fileDoesNotExist filename =
  XPath.anyWithExactText $ "File " <> filename <> " does not exist"

selectFileField ∷ String
selectFileField = XPath.inputWithPlaceholder "Select a file"

finishedMessage ∷ String
finishedMessage = XPath.anyWithText "Finished"

mdField ∷ String
mdField = aceEditor

accessMountDatabase ∷ String
accessMountDatabase = XPath.anyWithExactAriaLabel "Mount database"

accessConfigureMount ∷ String
accessConfigureMount = XPath.anyWithExactAriaLabel "Configure mount"

mountName ∷ String
mountName = "input" `XPath.withLabelWithExactText` "Name"

mountType ∷ String
mountType = "select" `XPath.withLabelWithExactText` "Mount type"

mountPort ∷ String
mountPort = "input" `XPath.withLabelWithExactText` "Port"

mountHost ∷ String
mountHost = "input" `XPath.withLabelWithExactText` "Host"

mountDatabase ∷ String
mountDatabase = "input" `XPath.withLabelWithExactText` "Database"

mountButton ∷ String
mountButton = "button" `XPath.nodeWithExactText` "Mount"

downloadButton ∷ String
downloadButton = "button" `XPath.nodeWithExactText` "Download"

cancelButton ∷ String
cancelButton = "button" `XPath.nodeWithExactText` "Cancel"

inputWithLabelAndType ∷ String → String → String
inputWithLabelAndType labelText inputType =
  "input"
    `XPath.withLabelWithExactText` labelText
    `XPath.nodeWithExactAttribute "type"` inputType

nthFile ∷ String
nthFile = "*" `XPath.nodeWithAriaLabel` "Select "

selectFile ∷ String → String
selectFile filename = "*" `XPath.nodeWithExactAriaLabel` ("Select " <> filename)

deselectFile ∷ String → String
deselectFile filename = "*" `XPath.nodeWithExactAriaLabel` ("Deselect " <> filename)

accessFile ∷ String → String
accessFile filename =
  XPath.anyWithExactText filename
    `XPath.withPredicate` ("preceding::" <> XPath.anyWithAriaLabel "Sort files by name")

accessBreadcrumb ∷ String → String
accessBreadcrumb name =
  XPath.anyWithExactText name
    `XPath.withPredicate` ("following::" <> XPath.anyWithAriaLabel "Sort files by name")

cardHeading ∷ String
cardHeading =
  XPath.withPredicate "*"
    $ XPath.anyOfThesePredicates
    $ XPath.ariaLabel
    <$> [ "Query card"
        , "Setup Markdown card"
        , "Open card"
        , "Search card"
        , "Setup Variables card"
        , "Setup Chart card"
        , "Setup Download card"
        ]

tableHeading ∷ String
tableHeading =
  XPath.withPredicate "*"
    $ XPath.ariaLabel "Table card"

sharingUrl ∷ String
sharingUrl = "*" `XPath.nodeWithExactAriaLabel` "Sharing URL"

publishingUrl ∷ String
publishingUrl = "*" `XPath.nodeWithAriaLabel` "Published deck URL"

copySharingUrl ∷ String
copySharingUrl = "*" `XPath.nodeWithExactText` "Copy"

showHiddenFiles ∷ String
showHiddenFiles = "*" `XPath.nodeWithExactAriaLabel` "Show hidden files"

hideHiddenFiles ∷ String
hideHiddenFiles = "*" `XPath.nodeWithExactAriaLabel` "Hide hidden files"

variablesCardVariableName ∷ String
variablesCardVariableName =
  XPath.anyWithExactAriaLabel "Variable name"


variablesCardVariableTypeFor ∷ String → String
variablesCardVariableTypeFor name =
  XPath.anyWithExactAriaLabel
    $ "Type of \"" <> name <> "\" variable"


variablesCardDefaultValueFor ∷ String → String
variablesCardDefaultValueFor name =
  XPath.anyWithExactAriaLabel
    $ "Default value for \"" <> name <> "\" variable"

chartCategorySelector ∷ String
chartCategorySelector = XPath.anyWithExactAriaLabel "Category"

chartStackSelector ∷ String
chartStackSelector = XPath.anyWithExactAriaLabel "Stack"

chartMeasureSelector ∷ String
chartMeasureSelector = XPath.anyWithExactAriaLabel "Measure"

chartSwitchToBar ∷ String
chartSwitchToBar = XPath.anyWithExactSrc "img/bar.svg"

chartContainer ∷ String
chartContainer = "div[@_echarts_instance_]"

trashCardAction ∷ String
trashCardAction =
  XPath.anyWithExactAriaLabel "Delete card"

publishDeckAction ∷ String
publishDeckAction =
  XPath.anyWithExactAriaLabel "Publish deck"

embedDeckAction ∷ String
embedDeckAction =
  XPath.anyWithExactAriaLabel "Embed deck"

shareDeckAction ∷ String
shareDeckAction =
  XPath.anyWithExactAriaLabel "Share deck"

resourceOpened ∷ String → String
resourceOpened fn =
  XPath.anyWithExactAriaLabel $ "Selected resource: " ⊕ fn

headerGripper ∷ String
headerGripper =
  XPath.anyWithExactAriaLabel "Show header"

previousCardGripper :: String
previousCardGripper =
  XPath.any
    `XPath.withPredicate` XPath.ariaLabel "Access previous card"

nextCardGripper :: String
nextCardGripper =
  XPath.any
    `XPath.withPredicate` XPath.ariaLabel "Access next card"

enabledPreviousCardGripper :: String
enabledPreviousCardGripper =
  previousCardGripper
    <> "[ancestor-or-self::*[not(@aria-disabled = 'true')]]"

enabledNextCardGripper :: String
enabledNextCardGripper =
  nextCardGripper
    <> "[ancestor-or-self::*[not(@aria-disabled = 'true')]]"

deck :: String
deck =
  XPath.anyWithExactAriaLabel $ "Deck"
