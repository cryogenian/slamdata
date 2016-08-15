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

module SlamData.Workspace.Notification
  ( DetailedError
  , loadDeckFail
  , loadParentFail
  , saveDeckFail
  , saveMirrorFail
  , deleteDeckFail
  , setRootFail
  , module N
  ) where

import SlamData.Prelude

import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Bus as Bus
import Control.Monad.Aff.Free (class Affable, fromAff)

import SlamData.Analytics.Event as AE
import SlamData.GlobalError as GE
import SlamData.Notification as N
import SlamData.Quasar.Error as QE
import SlamData.Workspace.Wiring (Wiring)

type DetailedError =
  ∀ m eff
  . (Bind m, Affable (avar ∷ AVAR | eff) m)
  ⇒ QE.QError
  → Wiring
  → m Unit

notifyError
  ∷ ∀ m eff
  . (Bind m, Affable (avar ∷ AVAR | eff) m)
  ⇒ String
  → AE.Event
  → QE.QError
  → Wiring
  → m Unit
notifyError msg event err wiring = do
  case GE.fromQError err of
    Left msg -> do
      N.error_ msg (Just msg) Nothing wiring.notify
      AE.track event wiring.analytics
    Right ge ->
      fromAff $ Bus.write ge wiring.globalError

loadDeckFail ∷ DetailedError
loadDeckFail =
  notifyError "Failed to load your deck." AE.ErrorLoadingDeck

loadParentFail ∷ DetailedError
loadParentFail =
  notifyError "Failed to load a parent deck." AE.ErrorLoadingDeck

saveDeckFail ∷ DetailedError
saveDeckFail =
  notifyError "Failed to save your deck." AE.ErrorSavingDeck

saveMirrorFail ∷ DetailedError
saveMirrorFail =
  notifyError "Failed to save a mirrored card." AE.ErrorSavingMirror

deleteDeckFail ∷ DetailedError
deleteDeckFail =
  notifyError "Failed to delete your deck." AE.ErrorDeletingDeck

setRootFail ∷ DetailedError
setRootFail =
  notifyError "Failed to update your workspace root." AE.ErrorUpdatingRoot
