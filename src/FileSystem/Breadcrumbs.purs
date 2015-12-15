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

module FileSystem.Breadcrumbs where

import Prelude

import Data.Foldable (foldl)
import Data.Identity (Identity(..))
import Data.List (List(..), reverse)
import Data.Maybe (maybe, Maybe(..))
import Data.Path.Pathy (rootDir, runDirName, dirName, parentDir)

import Halogen
import Halogen.HTML as H
import Halogen.HTML.Properties as P
import Halogen.Themes.Bootstrap3 as B

import FileSystem.Common (Slam())
import Model.Common (browseURL)
import Model.Salt (Salt())
import Model.Sort (Sort())
import Utils.Path (DirPath())

type State =
  { breadcrumbs :: List Breadcrumb
  , sort :: Sort
  , salt :: Salt
  }

type Breadcrumb =
  { name :: String
  , link :: DirPath
  }

rootBreadcrumb :: Breadcrumb
rootBreadcrumb =
  { name: "Home"
  , link: rootDir
  }

mkBreadcrumbs :: DirPath -> Sort -> Salt -> State
mkBreadcrumbs path sort salt =
  { breadcrumbs: reverse $ go Nil path
  , sort: sort
  , salt: salt
  }
  where
  go :: List Breadcrumb -> DirPath -> List Breadcrumb
  go result p =
    let result' = Cons { name: maybe "" runDirName (dirName p)
                       , link: p
                       } result
    in case parentDir p of
      Just dir -> go result' dir
      Nothing -> Cons rootBreadcrumb result

type Query = Identity

comp :: Component State Query Slam
comp = component render eval

render :: State -> ComponentHTML Query
render r =
  H.ol [ P.classes [ B.breadcrumb, B.colXs7 ] ]
  $ foldl (\views model -> view model <> views) [ ] r.breadcrumbs
  where
  view b =
    [ H.li_
        [ H.a
            [ P.href (browseURL Nothing r.sort r.salt b.link) ]
            [ H.text b.name ]
        ]
    ]

eval :: Eval Query State Query Slam
eval (Identity next) = pure next
