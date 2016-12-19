module SlamData.Workspace.Card.SetupFormInput.TextLike.Eval
  ( eval
  , module SlamData.Workspace.Card.SetupFormInput.TextLike.Model
  ) where

import SlamData.Prelude

import Control.Monad.State (class MonadState)
import Control.Monad.Throw (class MonadThrow)

import SlamData.Quasar.Class (class QuasarDSL)
import SlamData.Workspace.Card.CardType.FormInputType (FormInputType)
import SlamData.Workspace.Card.Port as Port
import SlamData.Workspace.Card.Eval.Monad as CEM
import SlamData.Workspace.Card.SetupFormInput.TextLike.Model (Model)

eval
  ∷ ∀ m
  . ( MonadState CEM.CardState m
    , MonadThrow CEM.CardError m
    , QuasarDSL m
    )
  ⇒ Model
  → Port.TaggedResourcePort
  → FormInputType
  → m Port.Port
eval Nothing _ _ =
  CEM.throw "Please select value"
eval (Just conf) taggedResource formInputType =
  pure
    $ Port.SetupTextLikeFormInput
        { name: Nothing
        , taggedResource
        , formInputType
        , cursor: conf.value
        }
