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

module SlamData.FileSystem.Search.Component.CSS where

import Halogen.HTML.Core (className, ClassName)

search ∷ ClassName
search = className "search"

searchInput ∷ ClassName
searchInput = className "search-input"

searchPath ∷ ClassName
searchPath = className "search-path"

searchPathActive ∷ ClassName
searchPathActive = className "search-path-active"

searchAffix ∷ ClassName
searchAffix = className "search-affix"

searchPathBody ∷ ClassName
searchPathBody = className "search-path-body"

searchAffixEmpty ∷ ClassName
searchAffixEmpty = className "search-affix-empty"

searchEmpty ∷ ClassName
searchEmpty = className "search-empty"

searchClearButton ∷ ClassName
searchClearButton = className "search-clear-button"

searchIcon ∷ ClassName
searchIcon = className "search-icon"
