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

import SlamData.Quasar.Aff (QEff, runQuasarF, Wiring)

sharePermission
  ∷ ∀ r eff m
  . (Monad m, Affable (QEff eff) m)
  ⇒ Wiring r
  → QTA.ShareRequestR
  → m (Exn.Error ⊹ (Array QTA.PermissionR))
sharePermission wiring req = do
  runQuasarF wiring $ lmap lowerQError
    <$> QF.sharePermission req

groupInfo
  ∷ ∀ r eff m
  . (Monad m, Affable (QEff eff) m)
  ⇒ Wiring r
  → QT.FilePath
  → m (Exn.Error ⊹ QTA.GroupInfoR)
groupInfo wiring groupPath =
  runQuasarF wiring $ lmap lowerQError
    <$> QF.groupInfo groupPath

createToken
  ∷ ∀ r eff m
  . (Monad m, Affable (QEff eff) m)
  ⇒ Wiring r
  → Maybe QTA.TokenName
  → Array QTA.ActionR
  → m (Exn.Error ⊹ QTA.TokenR)
createToken wiring mbName actions =
  runQuasarF wiring $ lmap lowerQError
    <$> QF.createToken mbName actions

tokenList
  ∷ ∀ r eff m
  . (Monad m, Affable (QEff eff) m)
  ⇒ Wiring r
  → m (Exn.Error ⊹ (Array QTA.TokenR))
tokenList wiring =
  runQuasarF wiring $ lmap lowerQError
    <$> QF.tokenList

deleteToken
  ∷ ∀ r eff m
  . (Monad m, Affable (QEff eff) m)
  ⇒ Wiring r
  → QTA.TokenId
  → m (Exn.Error ⊹ Unit)
deleteToken wiring tid =
  runQuasarF wiring $ lmap lowerQError
    <$> QF.deleteToken tid

tokenInfo
  ∷ ∀ r eff m
  . (Monad m, Affable (QEff eff) m)
  ⇒ Wiring r
  → QTA.TokenId
  → m (Exn.Error ⊹ QTA.TokenR)
tokenInfo wiring tid =
  runQuasarF wiring $ lmap lowerQError
    <$> QF.tokenInfo tid

permissionList
  ∷ ∀ r eff m
  . (Monad m, Affable (QEff eff) m)
  ⇒ Wiring r
  → Boolean
  → m (Exn.Error ⊹ (Array QTA.PermissionR))
permissionList wiring isTransitive =
  runQuasarF wiring $ lmap lowerQError
    <$> QF.permissionList isTransitive

deletePermission
  ∷ ∀ r eff m
  . (Monad m, Affable (QEff eff) m)
  ⇒ Wiring r
  → QTA.PermissionId
  → m (Exn.Error ⊹ Unit)
deletePermission wiring pid =
  runQuasarF wiring $ lmap lowerQError
    <$> QF.deletePermission pid
