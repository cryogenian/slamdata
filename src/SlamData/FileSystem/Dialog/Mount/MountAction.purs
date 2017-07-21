{-
Copyright 2017 SlamData, Inc.

Licensed under the Apache License, Version 2.0 (the "License");
embedu may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
-}

module SlamData.FileSystem.Dialog.Mount.MountAction where

import SlamData.Prelude

import Quasar.Types (AnyPath)
import Quasar.Mount as QM
import SlamData.Monad (Slam)

data MountAction
  = SaveMount AnyPath QM.MountConfig (String → Slam Unit)
  | DeleteMount AnyPath
