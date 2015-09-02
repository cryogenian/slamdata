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

module Model.File.Item where

import Prelude (Ordering())
import Model.Resource (Resource(..), resourcePath, resourceName, sortResource)
import Model.File.Sort (Sort())
import Optic.Core

data Item
  = Item Resource
  | SelectedItem Resource
  | PhantomItem Resource

itemResource :: Item -> Resource
itemResource (Item r) = r
itemResource (SelectedItem r) = r
itemResource (PhantomItem r) = r

sortItem :: Boolean -> Sort -> Item -> Item -> Ordering
sortItem isSearching sort a b =
  sortResource (sortProjection isSearching) sort (itemResource a) (itemResource b)
  where
  sortProjection true = resourcePath
  sortProjection _ = resourceName
