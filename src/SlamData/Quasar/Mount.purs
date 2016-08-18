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

import Control.Monad.Aff.Free (class Affable)
import Control.Monad.Except.Trans (ExceptT(..), runExceptT)

import Data.Path.Pathy as P

import Quasar.Advanced.QuasarAF as QF
import Quasar.Error (QError)
import Quasar.Mount as QM
import Quasar.Mount.MongoDB as QMountMDB
import Quasar.Mount.View as QMountV
import Quasar.Types (DirPath, FilePath)

import SlamData.Quasar.Aff (QEff, runQuasarF, Wiring)
import SlamData.Quasar.Error (throw)

mountInfo
  ∷ ∀ r eff m
  . (Monad m, Affable (QEff eff) m)
  ⇒ Wiring r
  → DirPath
  → m (Either QError QMountMDB.Config)
mountInfo wiring path = runExceptT do
  result ← ExceptT $ runQuasarF wiring $ QF.getMount (Left path)
  case result of
    QM.MongoDBConfig config → pure config
    _ → throw $ P.printPath path <> " is not a MongoDB mount point"

viewInfo
  ∷ ∀ r eff m
  . (Monad m, Affable (QEff eff) m)
  ⇒ Wiring r
  → FilePath
  → m (Either QError QMountV.Config)
viewInfo wiring path = runExceptT do
  result ← ExceptT $ runQuasarF wiring $ QF.getMount (Right path)
  case result of
    QM.ViewConfig config → pure config
    _ → throw $ P.printPath path <> " is not an SQL² view"

saveMount
  ∷ ∀ r eff m
  . Affable (QEff eff) m
  ⇒ Wiring r
  → DirPath
  → QMountMDB.Config
  → m (Either QError Unit)
saveMount wiring path config =
  runQuasarF wiring $ QF.updateMount (Left path) (QM.MongoDBConfig config)
