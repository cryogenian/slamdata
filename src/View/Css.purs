module View.Css where

import Halogen.HTML.Attributes (className, ClassName())

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

fileListField :: ClassName
fileListField = className "file-list-field"

fileListGroup :: ClassName
fileListGroup = className "file-list-group"

notebookName :: ClassName
notebookName = className "notebook-name"

notebookNav :: ClassName
notebookNav = className "notebook-nav"

notebookAddCellButton :: ClassName
notebookAddCellButton = className "notebook-add-cell-button"

notebookAddCellMenu :: ClassName
notebookAddCellMenu = className "notebook-add-cell-menu"

dialogMount :: ClassName
dialogMount = className "dialog-mount"

renameDialogForm :: ClassName
renameDialogForm = className "rename-dialog-form"

mountURI :: ClassName
mountURI = className "mount-uri"

mountHostList :: ClassName
mountHostList = className "mount-host-list"

mountUserInfo :: ClassName
mountUserInfo = className "mount-userinfo"

mountHost :: ClassName
mountHost = className "mount-host"

mountProps :: ClassName
mountProps = className "mount-props"

mountPropsScrollbox :: ClassName
mountPropsScrollbox = className "mount-props-scrollbox"

notebookContent :: ClassName
notebookContent = className "notebook-content"

notebookCell :: ClassName
notebookCell = className "notebook-cell"

cellInput :: ClassName
cellInput = className "cell-input"

cellOutput :: ClassName
cellOutput = className "cell-output"

cellNextActions :: ClassName
cellNextActions = className "cell-next-actions"

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

cellEvalLine :: ClassName
cellEvalLine = className "cell-eval-line"

exploreCellEditor :: ClassName
exploreCellEditor = className "explore-cell-editor"

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
