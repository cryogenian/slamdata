module SlamData.Workspace.Deck.Dialog.Share.Model where

import SlamData.Prelude

import Data.Path.Pathy as Pt

import Quasar.Advanced.Types as QT

import Utils.Path (DirPath)

data ShareResume
  = View
  | Edit

derive instance eqShareResume ∷ Eq ShareResume

printShareResume ∷ ShareResume → String
printShareResume View = "View"
printShareResume Edit = "Edit"

sharingActions ∷ DirPath → ShareResume → Array QT.ActionR
sharingActions deckPath View =
  case fst <$> Pt.peel deckPath of
    Nothing → [ ]
    Just wp →
      [ { operation: QT.Read
        , resource: QT.Dir wp
        , accessType: QT.Structural
        }
      , { operation: QT.Read
        , resource: QT.Dir wp
        , accessType: QT.Content
        }
      ]
sharingActions deckPath Edit =
  case fst <$> Pt.peel deckPath of
    Nothing → [ ]
    Just wp →
      [ { operation: QT.Add
        , resource: QT.Dir wp
        , accessType: QT.Structural
        }
      , { operation: QT.Add
        , resource: QT.Dir wp
        , accessType: QT.Content
        }
      , { operation: QT.Read
        , resource: QT.Dir wp
        , accessType: QT.Structural
        }
      , { operation: QT.Read
        , resource: QT.Dir wp
        , accessType: QT.Content
        }
      , { operation: QT.Modify
        , resource: QT.Dir wp
        , accessType: QT.Structural
        }
      , { operation: QT.Modify
        , resource: QT.Dir wp
        , accessType: QT.Content
        }
      , { operation: QT.Delete
        , resource: QT.Dir wp
        , accessType: QT.Structural
        }
      , { operation: QT.Delete
        , resource: QT.Dir wp
        , accessType: QT.Content
        }
      ]
