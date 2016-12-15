module SlamData.Workspace.Card.SetupFormInput.TextLike.Eval
  ( eval
  , module SlamData.Workspace.Card.SetupFormInput.TextLike.Model
  ) where

import SlamData.Prelude

import SlamData.Quasar.Class (class QuasarDSL)
import SlamData.Quasar.Error as QE
import SlamData.Workspace.Card.CardType.FormInputType (FormInputType)
import SlamData.Workspace.Card.Port as Port
import SlamData.Workspace.Card.Eval.CardEvalT as CET
import SlamData.Workspace.Card.SetupFormInput.TextLike.Model (Model)

eval
  ∷ ∀ m
  . (Monad m, QuasarDSL m)
  ⇒ Model
  → Port.TaggedResourcePort
  → FormInputType
  → CET.CardEvalT m Port.Port
eval Nothing _ _ =
  QE.throw "Please select value"
eval (Just conf) taggedResource formInputType =
  pure
    $ Port.SetupTextLikeFormInput
        { name: Nothing
        , taggedResource
        , formInputType
        , cursor: conf.value
        }
