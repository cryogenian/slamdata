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

module SlamData.Quasar.Mount
  ( mountInfo
  , saveMount
  , module QM
  , module Quasar.Error
  ) where

import SlamData.Prelude

import Quasar.Advanced.QuasarAF as QF
import Quasar.Error (QError)
import Quasar.Mount as QM
import Quasar.Types (AnyPath)

import SlamData.Quasar.Class (class QuasarDSL, liftQuasar)

mountInfo
  ∷ ∀ m
  . Monad m
  ⇒ QuasarDSL m
  ⇒ AnyPath
  → m (Either QError QM.MountConfig)
mountInfo path = runExceptT do
  ExceptT $ liftQuasar $ QF.getMount path

saveMount
  ∷ ∀ m
  . QuasarDSL m
  ⇒ AnyPath
  → QM.MountConfig
  → m (Either QError Unit)
saveMount path config =
  liftQuasar $ QF.updateMount path config
