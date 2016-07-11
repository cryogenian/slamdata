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

module SlamData.FileSystem.Listing.Item.Component.CSS where

import Halogen.HTML.Core (className, ClassName)

selected ∷ ClassName
selected = className "selected"

phantom ∷ ClassName
phantom = className "phantom"

itemIcon ∷ ClassName
itemIcon = className "item-icon"

itemToolbar ∷ ClassName
itemToolbar = className "item-toolbar"

itemContent ∷ ClassName
itemContent = className "item-content"

itemHidden ∷ ClassName
itemHidden = className "item-hidden"

fileAction ∷ ClassName
fileAction = className "file-action"
