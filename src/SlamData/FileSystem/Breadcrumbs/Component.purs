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

module SlamData.FileSystem.Breadcrumbs.Component
  ( State
  , Breadcrumb
  , rootBreadcrumb
  , mkBreadcrumbs
  , render
  ) where

import SlamData.Prelude

import Data.List (List(..), (:), reverse)
import Data.Path.Pathy (rootDir, runDirName, dirName, parentDir)

import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

import SlamData.Common.Sort (Sort)
import SlamData.FileSystem.Routing (browseURL)
import SlamData.FileSystem.Routing.Salt (Salt)

import Utils.Path (DirPath)

type State =
  { breadcrumbs ∷ List Breadcrumb
  , sort ∷ Sort
  , salt ∷ Salt
  }

type Breadcrumb =
  { name ∷ String
  , link ∷ DirPath
  }

rootBreadcrumb ∷ Breadcrumb
rootBreadcrumb =
  { name: "Home"
  , link: rootDir
  }

mkBreadcrumbs ∷ DirPath → Sort → Salt → State
mkBreadcrumbs path sort salt =
  { breadcrumbs: reverse $ go Nil path
  , sort: sort
  , salt: salt
  }
  where
  go ∷ List Breadcrumb → DirPath → List Breadcrumb
  go res p =
    let res' = { name: maybe "" runDirName (dirName p), link: p } : res
    in case parentDir p of
      Just dir → go res' dir
      Nothing → rootBreadcrumb : res

type Input =
  { path ∷ DirPath
  , sort ∷ Sort
  , salt ∷ Salt
  }

render ∷ ∀ q i. Input → HH.HTML q i
render { path, sort, salt } =
  HH.ol
    [ HP.classes [ HH.ClassName "breadcrumb" ] ]
    $ foldl (\views model → view model <> views) [ ] r.breadcrumbs
  where
  r = mkBreadcrumbs path sort salt
  view b =
    [ HH.li_
        [ HH.a
            [ HP.href (browseURL Nothing r.sort r.salt b.link) ]
            [ HH.text b.name ]
        ]
    ]
