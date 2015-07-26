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
