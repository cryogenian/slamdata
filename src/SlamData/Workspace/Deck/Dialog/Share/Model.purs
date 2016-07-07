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
  (deckPath </> Pt.dir ".tmp"  # QT.Dir ⋙ \resource →
    [ { operation: QT.Read
      , resource
      , accessType: QT.Structural
      }
    , { operation: QT.Read
      , resource
      , accessType: QT.Content
      }
    , { operation: QT.Modify
      , resource
      , accessType: QT.Structural
      }
    ]
  )
  ⊕ (foldMap (QT.File ⋙ \resource →
               [{ operation: QT.Read
                , resource
                , accessType: QT.Content
                }
               , { operation: QT.Read
                 , resource
                 , accessType: QT.Structural
                 }]) sources)
  ⊕ (foldMap (QT.File ⋙ \resource →
                             [{operation: QT.Read
                             , resource
                             , accessType: QT.Content
                             }
                            , { operation: QT.Read
                              , resource
                              , accessType: QT.Structural
                              }
                            , { operation: QT.Add
                              , resource
                              , accessType: QT.Content
                              }
                            , { operation: QT.Add
                              , resource
                              , accessType: QT.Structural
                              }
                            , { operation: QT.Modify
                              , resource
                              , accessType: QT.Content
                              }
                            , { operation: QT.Delete
                              , resource
                              , accessType: QT.Content
                              }
                            , { operation: QT.Delete
                              , resource
                              , accessType: QT.Structural
                              }                                                                                                   , { operation: QT.Modify
                              , resource
                              , accessType: QT.Structural
                              }]) caches)


sharingActions i@{deckPath} Edit =
  Debug.Trace.traceAny i \_ →
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
