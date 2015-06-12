module Model.File.Item where

import Model.Resource (Resource(..), resourcePath, resourceName, sortResource)
import Model.File.Sort (Sort())
import Optic.Core (LensP(), lens)

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
