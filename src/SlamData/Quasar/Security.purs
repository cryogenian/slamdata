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

import Control.Monad.Eff.Exception as Exn

import Quasar.Advanced.QuasarAF as QF
import Quasar.Error (lowerQError)
import Quasar.Types as QT
import Quasar.Advanced.Types as QTA

import SlamData.Quasar.Class (class QuasarDSL, liftQuasar)

sharePermission
  ∷ ∀ m
  . QuasarDSL m
  ⇒ QTA.ShareRequestR
  → m (Exn.Error ⊹ (Array QTA.PermissionR))
sharePermission req = do
  liftQuasar $ lmap lowerQError
    <$> QF.sharePermission req

groupInfo
  ∷ ∀ m
  . QuasarDSL m
  ⇒ QT.FilePath
  → m (Exn.Error ⊹ QTA.GroupInfoR)
groupInfo groupPath =
  liftQuasar $ lmap lowerQError
    <$> QF.groupInfo groupPath

createToken
  ∷ ∀ m
  . QuasarDSL m
  ⇒ Maybe QTA.TokenName
  → Array QTA.ActionR
  → m (Exn.Error ⊹ QTA.TokenR)
createToken mbName actions =
  liftQuasar $ lmap lowerQError
    <$> QF.createToken mbName actions

tokenList
  ∷ ∀ m
  . QuasarDSL m
  ⇒ m (Exn.Error ⊹ (Array QTA.TokenR))
tokenList =
  liftQuasar $ lmap lowerQError
    <$> QF.tokenList

deleteToken
  ∷ ∀ m
  . QuasarDSL m
  ⇒ QTA.TokenId
  → m (Exn.Error ⊹ Unit)
deleteToken tid =
  liftQuasar $ lmap lowerQError
    <$> QF.deleteToken tid

tokenInfo
  ∷ ∀ m
  . QuasarDSL m
  ⇒ QTA.TokenId
  → m (Exn.Error ⊹ QTA.TokenR)
tokenInfo tid =
  liftQuasar $ lmap lowerQError
    <$> QF.tokenInfo tid

permissionList
  ∷ ∀ m
  . QuasarDSL m
  ⇒ Boolean
  → m (Exn.Error ⊹ (Array QTA.PermissionR))
permissionList isTransitive =
  liftQuasar $ lmap lowerQError
    <$> QF.permissionList isTransitive

deletePermission
  ∷ ∀ m
  . QuasarDSL m
  ⇒ QTA.PermissionId
  → m (Exn.Error ⊹ Unit)
deletePermission pid =
  liftQuasar $ lmap lowerQError
    <$> QF.deletePermission pid

authorityList
  ∷ ∀ m
  . QuasarDSL m
  ⇒ m (Exn.Error ⊹ Array QTA.PermissionR)
authorityList =
  liftQuasar $ lmap lowerQError
    <$> QF.authorityList
