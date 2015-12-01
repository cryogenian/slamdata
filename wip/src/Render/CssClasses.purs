{-
Copyright 2015 SlamData, Inc.

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

module Render.CssClasses where

import Halogen.HTML.Core (className, ClassName())

version :: ClassName
version = className "version"

selected :: ClassName
selected = className "selected"

phantom :: ClassName
phantom = className "phantom"

searchInput :: ClassName
searchInput = className "search-input"

searchClear :: ClassName
searchClear = className "search-clear"

searchPath :: ClassName
searchPath = className "search-path"

searchPathActive :: ClassName
searchPathActive = className "search-path-active"

searchAffix :: ClassName
searchAffix = className "search-affix"

searchPathBody :: ClassName
searchPathBody = className "search-path-body"

searchAffixEmpty :: ClassName
searchAffixEmpty = className "search-affix-empty"

results :: ClassName
results = className "results"

header :: ClassName
header = className "header"

headerMenu :: ClassName
headerMenu = className "header-menu"

logo :: ClassName
logo = className "logo"

navCont :: ClassName
navCont = className "nav-cont"

navIcon :: ClassName
navIcon = className "nav-icon"

navLogo :: ClassName
navLogo = className "nav-logo"

search :: ClassName
search = className "search"

notebookName :: ClassName
notebookName = className "notebook-name"

content :: ClassName
content = className "content"

toolbarSort :: ClassName
toolbarSort = className "toolbar-sort"

toolbarMenu :: ClassName
toolbarMenu = className "toolbar-menu"

itemIcon :: ClassName
itemIcon = className "item-icon"

itemToolbar :: ClassName
itemToolbar = className "item-toolbar"

itemContent :: ClassName
itemContent = className "item-content"

itemHidden :: ClassName
itemHidden = className "item-hidden"

fileListField :: ClassName
fileListField = className "file-list-field"

fileListGroup :: ClassName
fileListGroup = className "file-list-group"

notebookNav :: ClassName
notebookNav = className "notebook-nav"

dialogDownload :: ClassName
dialogDownload = className "dialog-download"

dialogMount :: ClassName
dialogMount = className "dialog-mount"

renameDialogForm :: ClassName
renameDialogForm = className "rename-dialog-form"

mountURI :: ClassName
mountURI = className "mount-uri"

mountName :: ClassName
mountName = className "mount-name"

mountHostList :: ClassName
mountHostList = className "mount-host-list"

mountUserInfo :: ClassName
mountUserInfo = className "mount-userinfo"

mountHost :: ClassName
mountHost = className "mount-host"

mountProps :: ClassName
mountProps = className "mount-props"

mountPath :: ClassName
mountPath = className "mount-path"

mountPropsScrollbox :: ClassName
mountPropsScrollbox = className "mount-props-scrollbox"

mountProgressSpinner :: Boolean -> ClassName
mountProgressSpinner true = className "mount-progress-spinner"
mountProgressSpinner false = className "mount-progress-spinner-hidden"

notebookContent :: ClassName
notebookContent = className "notebook-content"

notebookViewHack :: ClassName
notebookViewHack = className "notebook-view-hack"

notebookCell :: ClassName
notebookCell = className "notebook-cell"

cellHeader :: ClassName
cellHeader = className "cell-header"

cellIcon :: ClassName
cellIcon = className "cell-icon"

cellName :: ClassName
cellName = className "cell-name"

cellInput :: ClassName
cellInput = className "cell-input"

cellOutput :: ClassName
cellOutput = className "cell-output"

cellOutputLabel :: ClassName
cellOutputLabel = className "cell-output-label"

cellOutputResult :: ClassName
cellOutputResult = className "cell-output-result"

cellNextActions :: ClassName
cellNextActions = className "cell-next-actions"

newCellMenu :: ClassName
newCellMenu = className "new-cell-menu"

aceContainer :: ClassName
aceContainer = className "ace-container"

playButton :: ClassName
playButton = className "play-button"

stopButton :: ClassName
stopButton = className "stop-button"

cellControls :: ClassName
cellControls = className "cell-controls"

statusText :: ClassName
statusText = className "status-text"

cellFailures :: ClassName
cellFailures = className "cell-failures"

cellMessages :: ClassName
cellMessages = className "cell-messages"

cellEvalLine :: ClassName
cellEvalLine = className "cell-eval-line"

exploreCellEditor :: ClassName
exploreCellEditor = className "explore-cell-editor"

pagination :: ClassName
pagination = className "pagination"

pageInput :: ClassName
pageInput = className "page-input"

pageSize :: ClassName
pageSize = className "page-size"

markdownOutput :: ClassName
markdownOutput = className "markdown-output"

searchCellInput :: ClassName
searchCellInput = className "search-cell-input"

searchCellButton :: ClassName
searchCellButton = className "search-cell-button"

nextCellList :: ClassName
nextCellList = className "next-cell-list"

echartsContainer :: ClassName
echartsContainer = className "echarts-container"

chartConfigureForm :: ClassName
chartConfigureForm = className "chart-configure-form"

vizCellEditor :: ClassName
vizCellEditor = className "viz-cell-editor"

vizChartTypeSelector :: ClassName
vizChartTypeSelector = className "viz-chart-type-selector"

vizChartConfiguration :: ClassName
vizChartConfiguration = className "viz-chart-configuration"

collapsed :: ClassName
collapsed = className "collapsed"

scrollbox :: ClassName
scrollbox = className "scrollbox"

loadingMessage :: ClassName
loadingMessage = className "loading-message"

withAggregation :: ClassName
withAggregation = className "with-aggregation"

aggregation :: ClassName
aggregation = className "aggregation"

embedBox :: ClassName
embedBox = className "embed-box"

downloadSource :: ClassName
downloadSource = className "download-source"

downloadTarget :: ClassName
downloadTarget = className "download-target"

downloadTargetBox :: ClassName
downloadTargetBox = className "download-target-box"

downloadCSVDelimiters :: ClassName
downloadCSVDelimiters = className "download-csv-delimiters"

downloadArrayMode :: ClassName
downloadArrayMode = className "download-array-mode"

downloadJSONOptions :: ClassName
downloadJSONOptions = className "download-json-options"

refreshButton :: ClassName
refreshButton = className "refresh-button"

shareButton :: ClassName
shareButton = className "share-button"

hiddenFileInput :: ClassName
hiddenFileInput = className "hidden-file-input"

chartConfigureHeight :: ClassName
chartConfigureHeight = className "chart-configure-height"

chartConfigureWidth :: ClassName
chartConfigureWidth = className "chart-configure-width"


chartCategory :: ClassName
chartCategory = className "chart-category"

chartMeasureOne :: ClassName
chartMeasureOne = className "chart-measure-one"

chartMeasureTwo :: ClassName
chartMeasureTwo = className "chart-measure-two"

chartDimension :: ClassName
chartDimension = className "chart-dimension"

chartSeriesOne :: ClassName
chartSeriesOne = className "chart-series-one"

chartSeriesTwo :: ClassName
chartSeriesTwo = className "chart-series-two"

pieChartIcon :: ClassName
pieChartIcon = className "pie-chart-icon"

barChartIcon :: ClassName
barChartIcon = className "bar-chart-icon"

lineChartIcon :: ClassName
lineChartIcon = className "line-chart-icon"

chartEditor :: ClassName
chartEditor = className "chart-editor"

chartOutput :: ClassName
chartOutput = className "chart-output"

glyphiconInactive :: ClassName
glyphiconInactive = className "glyphicon-inactive"


axisLabelParam :: ClassName
axisLabelParam = className "axis-label-param"

chartSizeParam :: ClassName
chartSizeParam = className "chart-size-param"
