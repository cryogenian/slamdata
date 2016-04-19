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

embedCardOutput ∷ String
embedCardOutput =
  XPath.anyWithExactText "Embed card output"

embedCardOutputTitle ∷ String
embedCardOutputTitle =
  XPath.anyWithExactText "Embed card"

mdCardTitle ∷ String
mdCardTitle =
  XPath.anyWithExactText "Markdown"

queryCardTitle ∷ String
queryCardTitle =
  XPath.anyWithExactText "Query"

formCardTitle ∷ String
formCardTitle =
  XPath.anyWithExactText "Form"


embedCardOutputSnippet ∷ String
embedCardOutputSnippet =
  XPath.anyWithText "<script type=\"text/javascript\">"


formCardHeader ∷ String
formCardHeader =
  XPath.anyWithExactAriaLabel "Form card"

exploreInput ∷ String
exploreInput =
  XPath.inputWithExactPlaceholder "Select a file"

fileSearchInput ∷ String
fileSearchInput =
  XPath.anyWithExactAriaLabel "File search field"

searchStringInput ∷ String
searchStringInput =
  XPath.inputWithExactPlaceholder "Input search string"

saveDestinationInput ∷ String
saveDestinationInput =
  XPath.anyWithExactAriaLabel "Output file destination"

saveSubmitButton ∷ String
saveSubmitButton =
  XPath.anyWithExactAriaLabel "Confirm saving file"

insertExploreCardAsNextAction ∷ String
insertExploreCardAsNextAction =
  XPath.anyWithExactAriaLabel "Insert Explore card"

insertSaveCard ∷ String
insertSaveCard =
  XPath.anyWithExactAriaLabel "Insert Save card"

insertMdCardAsNextAction ∷ String
insertMdCardAsNextAction =
  XPath.anyWithExactAriaLabel "Insert Markdown card"

insertQueryCardAsNextAction ∷ String
insertQueryCardAsNextAction =
  XPath.anyWithExactAriaLabel "Insert Query card"

insertSearchCardAsNextAction ∷ String
insertSearchCardAsNextAction =
  XPath.anyWithExactAriaLabel "Insert Search card"

insertVisualizeCardAsNextAction ∷ String
insertVisualizeCardAsNextAction =
  XPath.anyWithExactAriaLabel "Insert Visualize card"

insertDownloadCardAsNextAction ∷ String
insertDownloadCardAsNextAction =
  XPath.anyWithExactAriaLabel "Insert Download card"

insertAPIResultsCardAsNextAction ∷ String
insertAPIResultsCardAsNextAction =
  XPath.anyWithExactAriaLabel "Insert API Results card"

insertChartCardAsNextAction ∷ String
insertChartCardAsNextAction =
  XPath.anyWithExactAriaLabel "Insert Chart card"

insertFormCardAsNextAction ∷ String
insertFormCardAsNextAction =
  XPath.anyWithExactAriaLabel "Insert Form card"

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

insertMdCard ∷ String
insertMdCard =
  XPath.anyWithExactAriaLabel "Insert Markdown card"

insertExploreCard ∷ String
insertExploreCard = XPath.anyWithExactAriaLabel "Insert Explore card"

insertSearchCard ∷ String
insertSearchCard =
  XPath.anyWithExactAriaLabel "Insert Search card"

insertApiCard ∷ String
insertApiCard =
  XPath.anyWithExactAriaLabel "Insert API card"

insertJTableCardAsNextAction ∷ String
insertJTableCardAsNextAction =
  XPath.anyWithExactAriaLabel "Insert Table card"

hideCardOptions ∷ String
hideCardOptions =
  XPath.anyWithExactAriaLabel "Hide card options"

showCardOptions ∷ String
showCardOptions =
  XPath.anyWithExactAriaLabel "Show card options"

browseRootFolder ∷ String
browseRootFolder = XPath.anyWithAriaLabel "Browse root folder"

createNotebook ∷ String
createNotebook =
  XPath.anyWithExactAriaLabel "Create notebook"

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

showMessages ∷ String
showMessages =
  XPath.anyWithExactAriaLabel "Show messages"

noFileSelectedMessage ∷ String
noFileSelectedMessage =
  XPath.anyWithExactText "No file selected"

fileDoesNotExist ∷ String → String
fileDoesNotExist filename =
  XPath.anyWithExactText $ "File " ++ filename ++ " does not exist"

selectFileField ∷ String
selectFileField = XPath.inputWithPlaceholder "Select a file"

finishedMessage ∷ String
finishedMessage = XPath.anyWithText "Finished"

mdField ∷ String
mdField = aceEditor

accessMountDatabase ∷ String
accessMountDatabase = XPath.anyWithExactAriaLabel "Mount database"

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
selectFile filename = "*" `XPath.nodeWithExactAriaLabel` ("Select " ++ filename)

deselectFile ∷ String → String
deselectFile filename = "*" `XPath.nodeWithExactAriaLabel` ("Deselect " ++ filename)

accessFile ∷ String → String
accessFile filename =
  XPath.anyWithExactText filename
    `XPath.withPredicate` ("preceding::" ++ XPath.anyWithAriaLabel "Sort files by name")

accessBreadcrumb ∷ String → String
accessBreadcrumb name =
  XPath.anyWithExactText name
    `XPath.withPredicate` ("following::" ++ XPath.anyWithAriaLabel "Sort files by name")

cardHeading ∷ String
cardHeading =
  XPath.withPredicate "*"
    $ XPath.anyOfThesePredicates
    $ XPath.withAriaLabel
    <$> [ "Query card"
        , "Markdown card"
        , "Explore card"
        , "Search card"
        , "API card"
        , "Visualize card"
        , "Download card"
        ]

jtableHeading ∷ String
jtableHeading =
  XPath.withPredicate "*"
    $ XPath.withAriaLabel "Table card"

sharingUrl ∷ String
sharingUrl = "*" `XPath.nodeWithExactAriaLabel` "Sharing URL"

copySharingUrl ∷ String
copySharingUrl = "*" `XPath.nodeWithExactText` "Copy"

showHiddenFiles ∷ String
showHiddenFiles = "*" `XPath.nodeWithExactAriaLabel` "Show hidden files"

hideHiddenFiles ∷ String
hideHiddenFiles = "*" `XPath.nodeWithExactAriaLabel` "Hide hidden files"

apiCardVariableName ∷ String
apiCardVariableName =
  XPath.anyWithExactAriaLabel "API variable name"


apiCardVariableTypeFor ∷ String → String
apiCardVariableTypeFor name =
  XPath.anyWithExactAriaLabel
    $ "Type of \"" <> name <> "\" API variable"


apiCardDefaultValueFor ∷ String → String
apiCardDefaultValueFor name =
  XPath.anyWithExactAriaLabel
    $ "Default value for \"" <> name <> "\" API variable"

chartCategorySelector ∷ String
chartCategorySelector = XPath.anyWithExactAriaLabel "Category"

chartSeriesOneSelector ∷ String
chartSeriesOneSelector = XPath.anyWithExactAriaLabel "Second Series"

chartMeasureOneSelector ∷ String
chartMeasureOneSelector = XPath.anyWithExactAriaLabel "First Measure"

chartSwitchToBar ∷ String
chartSwitchToBar = XPath.anyWithExactSrc "img/bar.svg"

chartContainer ∷ String
chartContainer = "div[@_echarts_instance_]"

trashCardAction ∷ String
trashCardAction =
  XPath.anyWithExactAriaLabel "Trash card"

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
