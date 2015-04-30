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

directoryListGroup :: ClassName
directoryListGroup = className "directory-list-group"

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

mountURI :: ClassName
mountURI = className "mount-uri"

mountHostList :: ClassName
mountHostList = className "mount-host-list"

mountHost :: ClassName
mountHost = className "mount-host"

mountProps :: ClassName
mountProps = className "mount-props"

mountPropsScrollbox :: ClassName
mountPropsScrollbox = className "mount-props-scrollbox"
