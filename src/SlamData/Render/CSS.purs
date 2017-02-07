{-
Copyright 2016 SlamData, Inc.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
-}

module SlamData.Render.CSS where

import Halogen.HTML.Core (ClassName(..))

version ∷ ClassName
version = ClassName "version"

results ∷ ClassName
results = ClassName "results"

header ∷ ClassName
header = ClassName "header"

logo ∷ ClassName
logo = ClassName "logo"

navIcon ∷ ClassName
navIcon = ClassName "nav-icon"

navLogo ∷ ClassName
navLogo = ClassName "nav-logo"

content ∷ ClassName
content = ClassName "content"

invisible ∷ ClassName
invisible = ClassName "sd-invisible"

fileListField ∷ ClassName
fileListField = ClassName "file-list-field"

fileListGroup ∷ ClassName
fileListGroup = ClassName "file-list-group"

dialogDownload ∷ ClassName
dialogDownload = ClassName "dialog-download"

dialogMount ∷ ClassName
dialogMount = ClassName "dialog-mount"

renameDialogForm ∷ ClassName
renameDialogForm = ClassName "rename-dialog-form"

mountMongoDB ∷ ClassName
mountMongoDB = ClassName "mount-mongodb"

mountCouchbase ∷ ClassName
mountCouchbase = ClassName "mount-couchbase"

mountMarkLogic ∷ ClassName
mountMarkLogic = ClassName "mount-marklogic"

mountSpark ∷ ClassName
mountSpark = ClassName "mount-spark"

mountName ∷ ClassName
mountName = ClassName "mount-name"

mountHostList ∷ ClassName
mountHostList = ClassName "mount-host-list"

mountUserInfo ∷ ClassName
mountUserInfo = ClassName "mount-userinfo"

mountHost ∷ ClassName
mountHost = ClassName "mount-host"

mountProps ∷ ClassName
mountProps = ClassName "mount-props"

mountPath ∷ ClassName
mountPath = ClassName "mount-path"

mountFormat ∷ ClassName
mountFormat = ClassName "mount-format"

mountPropsScrollbox ∷ ClassName
mountPropsScrollbox = ClassName "mount-props-scrollbox"

mountProgressSpinner ∷ Boolean → ClassName
mountProgressSpinner true = ClassName "mount-progress-spinner"
mountProgressSpinner false = ClassName "mount-progress-spinner-hidden"

cardInput ∷ ClassName
cardInput = ClassName "card-input"

aceContainer ∷ ClassName
aceContainer = ClassName "ace-container"

cardFailures ∷ ClassName
cardFailures = ClassName "card-failures"

pageSize ∷ ClassName
pageSize = ClassName "page-size"

collapsed ∷ ClassName
collapsed = ClassName "collapsed"

loadingMessage ∷ ClassName
loadingMessage = ClassName "loading-message"

embedBox ∷ ClassName
embedBox = ClassName "embed-box"

downloadSource ∷ ClassName
downloadSource = ClassName "download-source"

downloadTarget ∷ ClassName
downloadTarget = ClassName "download-target"

downloadTargetBox ∷ ClassName
downloadTargetBox = ClassName "download-target-box"

downloadCSVDelimiters ∷ ClassName
downloadCSVDelimiters = ClassName "download-csv-delimiters"

downloadArrayMode ∷ ClassName
downloadArrayMode = ClassName "download-array-mode"

downloadJSONOptions ∷ ClassName
downloadJSONOptions = ClassName "download-json-options"

refreshButton ∷ ClassName
refreshButton = ClassName "refresh-button"

shareButton ∷ ClassName
shareButton = ClassName "share-button"

aggregation ∷ ClassName
aggregation = ClassName "aggregation"

deleteDeckIcon ∷ ClassName
deleteDeckIcon = ClassName "delete-deck-icon"

actionIcon ∷ ClassName
actionIcon = ClassName "action-icon"

chartOutput ∷ ClassName
chartOutput = ClassName "chart-output"

glyphiconInactive ∷ ClassName
glyphiconInactive = ClassName "glyphicon-inactive"

downloadCardEditor ∷ ClassName
downloadCardEditor = ClassName "download-card-editor"

downloadTypeSelector ∷ ClassName
downloadTypeSelector = ClassName "download-type-selector"

downloadConfiguration ∷ ClassName
downloadConfiguration = ClassName "download-configuration"

permissionsCheckboxes ∷ ClassName
permissionsCheckboxes = ClassName "permissions-checkboxes"

tokenGeneratorForm ∷ ClassName
tokenGeneratorForm = ClassName "token-generator-form"

cancelInputRunIcon ∷ ClassName
cancelInputRunIcon = ClassName "cancel-input-run-icon"

userShareForm ∷ ClassName
userShareForm = ClassName "user-share-form"

nextActionCard ∷ ClassName
nextActionCard = ClassName "next-action-card"

sharePermissionsDialog ∷ ClassName
sharePermissionsDialog = ClassName "share-permissions-dialog"

sharePermissionsCheckboxes ∷ ClassName
sharePermissionsCheckboxes = ClassName "share-permissions-checkboxes"

sharePermissionsResourceMark ∷ ClassName
sharePermissionsResourceMark = ClassName "share-permissions-resource-mark"

sharePermissionsContent ∷ ClassName
sharePermissionsContent = ClassName "share-permissions-content"

sharePermissionsButtons ∷ ClassName
sharePermissionsButtons = ClassName "share-permissions-buttons"

glyphImage ∷ ClassName
glyphImage = ClassName "glyph-image"

saveCardButton ∷ ClassName
saveCardButton = ClassName "save-card-button"

chartGlyph ∷ ClassName
chartGlyph = ClassName "chart-glyph"

codeGlyph ∷ ClassName
codeGlyph = ClassName "code-glyph"

deckBackSide ∷ ClassName
deckBackSide = ClassName "sd-deck-backside"

openCard ∷ ClassName
openCard = ClassName "open-card"

openCardMenu ∷ ClassName
openCardMenu = ClassName "open-card-menu"

loading ∷ ClassName
loading = ClassName "loading"

pending ∷ ClassName
pending = ClassName "pending"

cardSlider ∷ ClassName
cardSlider = ClassName "sd-card-slider"

card :: ClassName
card = ClassName "sd-card"

cardSliding ∷ ClassName
cardSliding = ClassName "sd-card-sliding"

cardTransitioning ∷ ClassName
cardTransitioning = ClassName "sd-card-transitioning"

cardActive ∷ ClassName
cardActive = ClassName "sd-card-active"

cardGripper ∷ ClassName
cardGripper = ClassName "sd-card-gripper"

cardGripperLast ∷ ClassName
cardGripperLast = ClassName "sd-card-gripper-last"

form ∷ ClassName
form = ClassName "sd-form"

formButton ∷ ClassName
formButton = ClassName "sd-form-button"
