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
import Control.Monad.Eff.Exception as Exn

import Quasar.Advanced.QuasarAF as QF
import Quasar.Error (lowerQError)
import Quasar.Types as QT
import Quasar.Advanced.Types as QTA

import SlamData.Quasar.Aff (QEff, runQuasarF)

sharePermission
  ∷ ∀ eff m
  . (Monad m, Affable (QEff eff) m)
  ⇒ QTA.ShareRequestR
  → m (Exn.Error ⊹ (Array QTA.PermissionR))
sharePermission req = do
  runQuasarF $ lmap lowerQError
    <$> QF.sharePermission req

groupInfo
  ∷ ∀ eff m
  . (Monad m, Affable (QEff eff) m)
  ⇒ QT.FilePath
  → m (Exn.Error ⊹ QTA.GroupInfoR)
groupInfo groupPath =
  runQuasarF $ lmap lowerQError
    <$> QF.groupInfo groupPath

createToken
  ∷ ∀ eff m
  . (Monad m, Affable (QEff eff) m)
  ⇒ Maybe QTA.TokenName
  → Array QTA.ActionR
  → m (Exn.Error ⊹ QTA.TokenR)
createToken mbName actions =
  runQuasarF $ lmap lowerQError
    <$> QF.createToken mbName actions

tokenList
  ∷ ∀ eff m
  . (Monad m, Affable (QEff eff) m)
  ⇒ m (Exn.Error ⊹ (Array QTA.TokenR))
tokenList =
  runQuasarF $ lmap lowerQError
    <$> QF.tokenList

deleteToken
  ∷ ∀ eff m
  . (Monad m, Affable (QEff eff) m)
  ⇒ QTA.TokenId
  → m (Exn.Error ⊹ Unit)
deleteToken tid =
  runQuasarF $ lmap lowerQError
    <$> QF.deleteToken tid

tokenInfo
  ∷ ∀ eff m
  . (Monad m, Affable (QEff eff) m)
  ⇒ QTA.TokenId
  → m (Exn.Error ⊹ QTA.TokenR)
tokenInfo tid =
  runQuasarF $ lmap lowerQError
    <$> QF.tokenInfo tid

permissionList
  ∷ ∀ eff m
  . (Monad m, Affable (QEff eff) m)
  ⇒ Boolean
  → m (Exn.Error ⊹ (Array QTA.PermissionR))
permissionList isTransitive =
  runQuasarF $ lmap lowerQError
    <$> QF.permissionList isTransitive

deletePermission
  ∷ ∀ eff m
  . (Monad m, Affable (QEff eff) m)
  ⇒ QTA.PermissionId
  → m (Exn.Error ⊹ Unit)
deletePermission pid =
  runQuasarF $ lmap lowerQError
    <$> QF.deletePermission pid
