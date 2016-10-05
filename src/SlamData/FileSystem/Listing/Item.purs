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

module SlamData.FileSystem.Listing.Item where

import SlamData.Prelude

import SlamData.Common.Sort (Sort)
import SlamData.FileSystem.Resource (Resource, resourcePath, resourceName, sortResource)

data Item
  = Item Resource
  | SelectedItem Resource
  | ActionsPresentedItem Resource
  | PhantomItem Resource

itemResource ∷ Item → Resource
itemResource (Item r) = r
itemResource (SelectedItem r) = r
itemResource (ActionsPresentedItem r) = r
itemResource (PhantomItem r) = r

sortItem ∷ Boolean → Sort → Item → Item → Ordering
sortItem isSearching sort a b =
  sortResource (sortProjection isSearching) sort (itemResource a) (itemResource b)
  where
  sortProjection true = resourcePath
  sortProjection _ = resourceName

instance eqItem ∷ Eq Item where
  eq (Item r) (Item r') = r ≡ r'
  eq (SelectedItem r) (SelectedItem r') = r ≡ r'
  eq (ActionsPresentedItem r) (ActionsPresentedItem r') = r ≡ r'
  eq (PhantomItem r) (PhantomItem r') = r ≡ r'
  eq _ _ = false

instance ordItem ∷ Ord Item where
  compare (SelectedItem r) (SelectedItem r') = compare r r'
  compare (SelectedItem _) _ = GT
  compare _ (SelectedItem _) = LT
  compare (PhantomItem r) (PhantomItem r') = compare r r'
  compare (PhantomItem _) _ = LT
  compare _ (PhantomItem _) = GT
  compare (ActionsPresentedItem r) (ActionsPresentedItem r') = compare r r'
  compare _ (ActionsPresentedItem _) = EQ
  compare (ActionsPresentedItem _) _ = EQ
  compare (Item r) (Item r') = compare r r'
