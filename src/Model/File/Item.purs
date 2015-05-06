module Model.File.Item where

import Model.Resource
import Model.Sort
import Optic.Core 

type Item =
  { selected :: Boolean
  , hovered :: Boolean
  , phantom :: Boolean 
  , resource :: Resource
  }


resourceL :: LensP Item Resource
resourceL = lens _.resource (_{resource = _})

wrap :: Resource -> Item
wrap r =
  { selected: false
  , hovered: false
  , phantom: false
  , resource: r}

initFile :: Item
initFile = wrap newFile

initDirectory :: Item
initDirectory = wrap newDirectory

initNotebook :: Item
initNotebook = wrap newNotebook

sortItem :: Boolean -> Sort -> Item -> Item -> Ordering
sortItem isSearching sort a b =
    sortResource (sortProjection isSearching) sort a.resource b.resource 
    where
    sortProjection true = resourcePath
    sortProjection _ = resourceName
