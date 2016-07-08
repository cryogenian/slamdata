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

import Quasar.Advanced.Types as QT

import SlamData.Workspace.Deck.DeckId (DeckId)

import Utils.Path (DirPath, FilePath)

data ShareResume
  = View
  | Edit

derive instance eqShareResume ∷ Eq ShareResume

type SharingInput =
  { deckId ∷ DeckId
  , workspacePath ∷ DirPath
  , sources ∷ List FilePath
  , caches ∷ List FilePath
  }

printShareResume ∷ ShareResume → String
printShareResume View = "View"
printShareResume Edit = "Edit"

sharingActions ∷ SharingInput → ShareResume → Array QT.ActionR
sharingActions { workspacePath } View =
  [ { operation: QT.Read
    , resource: QT.Dir workspacePath
    , accessType: QT.Structural
    }
  , { operation: QT.Read
    , resource: QT.Dir workspacePath
    , accessType: QT.Content
    }
  ]
sharingActions { workspacePath } Edit =
  [ { operation: QT.Add
    , resource: QT.Dir workspacePath
    , accessType: QT.Structural
    }
  , { operation: QT.Add
    , resource: QT.Dir workspacePath
    , accessType: QT.Content
    }
  , { operation: QT.Read
    , resource: QT.Dir workspacePath
    , accessType: QT.Structural
    }
  , { operation: QT.Read
    , resource: QT.Dir workspacePath
    , accessType: QT.Content
    }
  , { operation: QT.Modify
    , resource: QT.Dir workspacePath
    , accessType: QT.Structural
    }
  , { operation: QT.Modify
    , resource: QT.Dir workspacePath
    , accessType: QT.Content
    }
  , { operation: QT.Delete
    , resource: QT.Dir workspacePath
    , accessType: QT.Structural
    }
  , { operation: QT.Delete
    , resource: QT.Dir workspacePath
    , accessType: QT.Content
    }
  ]
