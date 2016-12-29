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

module SlamData.Workspace.Deck.Dialog.Share.Model where

import SlamData.Prelude

import Data.List (List)
import Data.Path.Pathy ((</>))
import Data.Path.Pathy as Pt

import Quasar.Advanced.Types as QT
import SlamData.Workspace.Deck.DeckId (DeckId)

import Utils.Path (DirPath, FilePath)

data ShareResume
  = View
  | Edit

derive instance eqShareResume ∷ Eq ShareResume

type SharingInput =
  { workspacePath ∷ DirPath
  , deckId ∷ DeckId
  , sources ∷ List FilePath
  , caches ∷ List FilePath
  }

printShareResume ∷ ShareResume → String
printShareResume View = "View"
printShareResume Edit = "Edit"

sharingActions ∷ SharingInput → ShareResume → Array QT.ActionR
sharingActions {workspacePath, sources, caches} View =
  (workspacePath </> Pt.dir ".tmp" # QT.Dir ⋙ \resource → do
      operation ← [ QT.Read, QT.Modify, QT.Delete, QT.Add ]
      accessType ← [ QT.Structural, QT.Content, QT.Mount ]
      pure { operation, resource, accessType }
  )
  ⊕ (workspacePath </> Pt.file "index" # QT.File ⋙ \resource →
      pure { operation: QT.Read, resource, accessType: QT.Content }
    )
  ⊕ (flip foldMap sources $ QT.File ⋙ \resource → do
        accessType ← [ QT.Structural, QT.Content ]
        pure { operation: QT.Read , accessType, resource }
    )
  ⊕ (flip foldMap caches $ QT.File ⋙ \resource → do
        operation ← [ QT.Read, QT.Add, QT.Delete, QT.Modify ]
        accessType ← [ QT.Structural, QT.Content ]
        pure { operation, accessType, resource }
    )
sharingActions {workspacePath, sources, caches} Edit =
  (workspacePath # QT.Dir ⋙ \resource → do
    operation ← [ QT.Add, QT.Delete, QT.Read, QT.Modify ]
    accessType ← [ QT.Content, QT.Structural, QT.Mount ]
    pure { operation, accessType, resource }
  )
  ⊕ (flip foldMap (sources <> caches) $ QT.File ⋙ \resource → do
    operation ← [ QT.Read, QT.Delete, QT.Add, QT.Modify ]
    accessType ← [ QT.Content, QT.Structural, QT.Mount ]
    pure { operation, accessType, resource }
  )
