module SlamData.Workspace.Deck.Dialog.Share.Model where

import SlamData.Prelude

import Data.Path.Pathy ((</>))
import Data.Path.Pathy as Pt

import Quasar.Advanced.Types as QT

import Utils.Path (DirPath, FilePath)

data ShareResume
  = View
  | Edit

derive instance eqShareResume ∷ Eq ShareResume

type SharingInput =
  { deckPath ∷ DirPath
  , sources ∷ Array FilePath
  , caches ∷ Array FilePath
  }

printShareResume ∷ ShareResume → String
printShareResume View = "View"
printShareResume Edit = "Edit"

sharingActions ∷ SharingInput → ShareResume → Array QT.ActionR
sharingActions i@{deckPath, sources, caches} View =
  (flip foldMap  (Pt.peel deckPath) $ fst ⋙ (_  </> Pt.dir ".tmp") ⋙ QT.Dir ⋙ \resource → do
      operation ← [ QT.Read ]
      accessType ← [ QT.Structural, QT.Content ]
      pure { operation, resource, accessType }
  )
  ⊕ (flip foldMap (Pt.peel deckPath) $ fst ⋙ (_ </> Pt.file "index") ⋙ QT.File ⋙ \resource →
      pure { operation: QT.Read, resource, accessType: QT.Content }
    )
  ⊕ (let
        resource = QT.File $ deckPath </> Pt.file "index"
     in do
       accessType ← [ QT.Structural, QT.Content ]
       pure { operation: QT.Read, resource, accessType }
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
sharingActions {deckPath, sources, caches} Edit =
  (flip foldMap (Pt.peel deckPath) $ fst ⋙ QT.Dir ⋙ \resource → do
    operation ← [ QT.Add, QT.Delete, QT.Read, QT.Modify ]
    accessType ← [ QT.Content, QT.Structural, QT.Mount ]
    pure { operation, accessType, resource }
  )
  ⊕ (flip foldMap (sources <> caches) $ QT.File ⋙ \resource → do
    operation ← [ QT.Read, QT.Delete, QT.Add, QT.Modify ]
    accessType ← [ QT.Content, QT.Structural, QT.Mount ]
    pure { operation, accessType, resource }
  )
