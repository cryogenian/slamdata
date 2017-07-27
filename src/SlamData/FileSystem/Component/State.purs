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

module SlamData.FileSystem.Component.State where

import SlamData.Prelude

import Data.Path.Pathy (rootDir)
import SlamData.Common.Sort (Sort(..))
import SlamData.FileSystem.Dialog as DialogT
import SlamData.FileSystem.Routing.Salt (Salt(..))
import SlamData.License as License
import Utils.Path (DirPath)

type State =
  { path ∷ DirPath
  , salt ∷ Salt
  , sort ∷ Sort
  , presentMountHint ∷ Boolean
  , version ∷ Maybe String
  , isMount ∷ Boolean
  , isUnconfigured ∷ Boolean
  , showHiddenFiles ∷ Boolean
  , presentIntroVideo ∷ Boolean
  , dialog ∷ Maybe DialogT.Definition
  , licenseProblem ∷ Maybe License.LicenseProblem
  }

initialState ∷ State
initialState =
  { path: rootDir
  , salt: Salt ""
  , presentMountHint: false
  , sort: Asc
  , version: Nothing
  , isMount: false
  , isUnconfigured: false
  , showHiddenFiles: false
  , presentIntroVideo: false
  , dialog: Nothing
  , licenseProblem: Nothing
  }
