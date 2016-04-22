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
  ) where

import SlamData.Prelude

import Control.Monad.Aff.Free (class Affable)
import Control.Monad.Eff.Exception as Exn
import Control.Monad.Error.Class as Err
import Control.Monad.Except.Trans (ExceptT(..), runExceptT)

import Data.Path.Pathy as P

import Quasar.Advanced.QuasarAF as QF
import Quasar.Error (lowerQError)
import Quasar.Mount as QM
import Quasar.Mount.MongoDB as QMountMDB
import Quasar.Mount.View as QMountV
import Quasar.Types (DirPath, FilePath)

import SlamData.Quasar.Aff (QEff, runQuasarF)

mountInfo
  ∷ ∀ eff m
  . (Monad m, Affable (QEff eff) m)
  ⇒ DirPath
  → m (Either Exn.Error QMountMDB.Config)
mountInfo path = runExceptT do
  result ← ExceptT $ runQuasarF $ lmap lowerQError <$>
    QF.getMount (Left path)
  case result of
    QM.MongoDBConfig config → pure config
    _ → Err.throwError $ Exn.error $
      P.printPath path <> " is not a MongoDB mount point"

viewInfo
  ∷ ∀ eff m
  . (Monad m, Affable (QEff eff) m)
  ⇒ FilePath
  → m (Either Exn.Error QMountV.Config)
viewInfo path = runExceptT do
  result ← ExceptT $ runQuasarF $ lmap lowerQError <$>
    QF.getMount (Right path)
  case result of
    QM.ViewConfig config → pure config
    _ → Err.throwError $ Exn.error $ P.printPath path <> " is not an SQL² view"

saveMount
  ∷ ∀ eff m
  . Affable (QEff eff) m
  ⇒ DirPath
  → QMountMDB.Config
  → m (Either Exn.Error Unit)
saveMount path config =
  runQuasarF $ lmap lowerQError <$>
    QF.updateMount (Left path) (QM.MongoDBConfig config)
