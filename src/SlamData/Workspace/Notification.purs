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

import SlamData.GlobalError as GE
import SlamData.Notification as N
import SlamData.Quasar.Error as QE

type DetailedError =
  ∀ m
  . (Monad m, N.NotifyDSL m, GE.GlobalErrorDSL m)
  ⇒ QE.QError
  → m Unit

notifyError
  ∷ ∀ m
  . (Monad m, N.NotifyDSL m, GE.GlobalErrorDSL m)
  ⇒ String
  → QE.QError
  → m Unit
notifyError msg err = do
  case GE.fromQError err of
    Left details ->
      N.error msg (Just (N.Details details)) Nothing Nothing
    Right ge ->
      GE.raiseGlobalError ge

loadDeckFail ∷ DetailedError
loadDeckFail =
  notifyError "Failed to load your deck."

loadParentFail ∷ DetailedError
loadParentFail =
  notifyError "Failed to load a parent deck."

saveDeckFail ∷ DetailedError
saveDeckFail =
  notifyError "Failed to save your deck."

saveMirrorFail ∷ DetailedError
saveMirrorFail =
  notifyError "Failed to save a mirrored card."

deleteDeckFail ∷ DetailedError
deleteDeckFail =
  notifyError "Failed to delete your deck."

setRootFail ∷ DetailedError
setRootFail =
  notifyError "Failed to update your workspace root."
