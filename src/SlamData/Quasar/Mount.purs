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
  , viewInfo
  , saveMount
  , module Quasar.Error
  ) where

import SlamData.Prelude

import Data.Path.Pathy as P

import Quasar.Advanced.QuasarAF as QF
import Quasar.Error (QError)
import Quasar.Mount as QM
import Quasar.Mount.MongoDB as QMountMDB
import Quasar.Mount.View as QMountV
import Quasar.Types (DirPath, FilePath)

import SlamData.Quasar.Error (throw)
import SlamData.Quasar.Class (class QuasarDSL, liftQuasar)

mountInfo
  ∷ ∀ m
  . (Monad m, QuasarDSL m)
  ⇒ DirPath
  → m (Either QError QMountMDB.Config)
mountInfo path = runExceptT do
  result ← ExceptT $ liftQuasar $ QF.getMount (Left path)
  case result of
    QM.MongoDBConfig config → pure config
    _ → throw $ P.printPath path <> " is not a MongoDB mount point"

viewInfo
  ∷ ∀ m
  . (Monad m, QuasarDSL m)
  ⇒ FilePath
  → m (Either QError QMountV.Config)
viewInfo path = runExceptT do
  result ← ExceptT $ liftQuasar $ QF.getMount (Right path)
  case result of
    QM.ViewConfig config → pure config
    _ → throw $ P.printPath path <> " is not an SQL² view"

saveMount
  ∷ ∀ m
  . QuasarDSL m
  ⇒ DirPath
  → QMountMDB.Config
  → m (Either QError Unit)
saveMount path config =
  liftQuasar $ QF.updateMount (Left path) (QM.MongoDBConfig config)
