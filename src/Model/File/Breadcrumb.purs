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

module Model.File.Breadcrumb where

import Prelude
import Data.List (List(..), fromList)
import Data.Maybe (Maybe(..), maybe)
import Data.Path.Pathy (rootDir, parentDir, dirName, runDirName)
import Model.Path (DirPath())

-- | Model for a breadcrumb entry.
type Breadcrumb =
  { name :: String
  , link :: DirPath
  }

-- | The `Breadcrumb` for an empty path.
rootBreadcrumb :: Breadcrumb
rootBreadcrumb =
  { name: "Home"
  , link: rootDir
  }

-- | Make `Breadcrumb` entries for each directory up to the specified path.
mkBreadcrumbs :: DirPath -> Array Breadcrumb
mkBreadcrumbs path = fromList $ go Nil path
  where
  go :: List Breadcrumb -> DirPath -> List Breadcrumb
  go result p =
    let result' = Cons { name: maybe "" runDirName (dirName p), link: p } result
    in case parentDir p of
      Just dir -> go result' dir
      Nothing -> Cons rootBreadcrumb result
