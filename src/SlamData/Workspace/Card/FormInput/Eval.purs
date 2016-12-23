module SlamData.Workspace.Card.FormInput.Eval
  ( evalTextLike
  , evalLabeled
  , module SlamData.Workspace.Card.FormInput.Model
  ) where

import SlamData.Prelude

import Control.Monad.State (class MonadState)
import Control.Monad.Throw (class MonadThrow)

import SlamData.Quasar.Class (class QuasarDSL)
import SlamData.Workspace.Card.Port as Port
import SlamData.Workspace.Card.Eval.Monad as CEM
import SlamData.Workspace.Card.FormInput.Model (Model)

evalLabeled
  ∷ ∀ m
  . ( MonadState CEM.CardState m
    , MonadThrow CEM.CardError m
    , QuasarDSL m
    )
  ⇒ Model
  → Port.SetupLabeledFormInputPort
  → m Port.Port
evalLabeled m p =
  pure $ Port.TaggedResource p.taggedResource

evalTextLike
  ∷ ∀ m
  . ( MonadState CEM.CardState m
    , MonadThrow CEM.CardError m
    , QuasarDSL m
    )
  ⇒ Model
  → Port.SetupTextLikeFormInputPort
  → m Port.Port
evalTextLike m p =
  pure $ Port.TaggedResource p.taggedResource
