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
