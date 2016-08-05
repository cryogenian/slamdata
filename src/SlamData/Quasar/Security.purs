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

module SlamData.Quasar.Security where

import SlamData.Prelude

import Control.Monad.Aff.Free (class Affable)
import Control.Monad.Aff.AVar (AVar)
import Control.Monad.Aff.Bus (Bus, Cap)
import Control.Monad.Eff.Exception as Exn

import Quasar.Advanced.QuasarAF as QF
import Quasar.Error (lowerQError)
import Quasar.Types as QT
import Quasar.Advanced.Types as QTA

import SlamData.Quasar.Aff (QEff, runQuasarF)
import SlamData.Quasar.Auth.Reauthentication (EIdToken)

sharePermission
  ∷ ∀ eff r m
  . (Monad m, Affable (QEff eff) m)
  ⇒ (Bus (write ∷ Cap | r) (AVar EIdToken))
  → QTA.ShareRequestR
  → m (Exn.Error ⊹ (Array QTA.PermissionR))
sharePermission requestNewIdTokenBus req = do
  runQuasarF requestNewIdTokenBus $ lmap lowerQError
    <$> QF.sharePermission req

groupInfo
  ∷ ∀ eff r m
  . (Monad m, Affable (QEff eff) m)
  ⇒ (Bus (write ∷ Cap | r) (AVar EIdToken))
  → QT.FilePath
  → m (Exn.Error ⊹ QTA.GroupInfoR)
groupInfo requestNewIdTokenBus groupPath =
  runQuasarF requestNewIdTokenBus $ lmap lowerQError
    <$> QF.groupInfo groupPath

createToken
  ∷ ∀ eff r m
  . (Monad m, Affable (QEff eff) m)
  ⇒ (Bus (write ∷ Cap | r) (AVar EIdToken))
  → Maybe QTA.TokenName
  → Array QTA.ActionR
  → m (Exn.Error ⊹ QTA.TokenR)
createToken requestNewIdTokenBus mbName actions =
  runQuasarF requestNewIdTokenBus $ lmap lowerQError
    <$> QF.createToken mbName actions

tokenList
  ∷ ∀ eff r m
  . (Monad m, Affable (QEff eff) m)
  ⇒ (Bus (write ∷ Cap | r) (AVar EIdToken))
  → m (Exn.Error ⊹ (Array QTA.TokenR))
tokenList requestNewIdTokenBus =
  runQuasarF requestNewIdTokenBus $ lmap lowerQError
    <$> QF.tokenList

deleteToken
  ∷ ∀ eff r m
  . (Monad m, Affable (QEff eff) m)
  ⇒ (Bus (write ∷ Cap | r) (AVar EIdToken))
  → QTA.TokenId
  → m (Exn.Error ⊹ Unit)
deleteToken requestNewIdTokenBus tid =
  runQuasarF requestNewIdTokenBus $ lmap lowerQError
    <$> QF.deleteToken tid

tokenInfo
  ∷ ∀ eff r m
  . (Monad m, Affable (QEff eff) m)
  ⇒ (Bus (write ∷ Cap | r) (AVar EIdToken))
  → QTA.TokenId
  → m (Exn.Error ⊹ QTA.TokenR)
tokenInfo requestNewIdTokenBus tid =
  runQuasarF requestNewIdTokenBus $ lmap lowerQError
    <$> QF.tokenInfo tid

permissionList
  ∷ ∀ eff r m
  . (Monad m, Affable (QEff eff) m)
  ⇒ (Bus (write ∷ Cap | r) (AVar EIdToken))
  → Boolean
  → m (Exn.Error ⊹ (Array QTA.PermissionR))
permissionList requestNewIdTokenBus isTransitive =
  runQuasarF requestNewIdTokenBus $ lmap lowerQError
    <$> QF.permissionList isTransitive

deletePermission
  ∷ ∀ eff r m
  . (Monad m, Affable (QEff eff) m)
  ⇒ (Bus (write ∷ Cap | r) (AVar EIdToken))
  → QTA.PermissionId
  → m (Exn.Error ⊹ Unit)
deletePermission requestNewIdTokenBus pid =
  runQuasarF requestNewIdTokenBus $ lmap lowerQError
    <$> QF.deletePermission pid
