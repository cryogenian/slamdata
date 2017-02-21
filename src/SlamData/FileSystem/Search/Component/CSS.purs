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

import Halogen.HTML.Core (ClassName(..))

search ∷ ClassName
search = ClassName "search"

searchInput ∷ ClassName
searchInput = ClassName "search-input"

searchPath ∷ ClassName
searchPath = ClassName "search-path"

searchPathActive ∷ ClassName
searchPathActive = ClassName "search-path-active"

searchAffix ∷ ClassName
searchAffix = ClassName "search-affix"

searchPathBody ∷ ClassName
searchPathBody = ClassName "search-path-body"

searchAffixEmpty ∷ ClassName
searchAffixEmpty = ClassName "search-affix-empty"

searchEmpty ∷ ClassName
searchEmpty = ClassName "search-empty"

searchClearButton ∷ ClassName
searchClearButton = ClassName "search-clear-button"

searchIcon ∷ ClassName
searchIcon = ClassName "search-icon"
