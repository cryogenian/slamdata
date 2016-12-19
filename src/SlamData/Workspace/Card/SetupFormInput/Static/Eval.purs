module SlamData.Workspace.Card.SetupFormInput.Static.Eval
  ( eval
  , module SlamData.Workspace.Card.SetupFormInput.Static.Model
  ) where

import SlamData.Prelude

import Control.Monad.State (class MonadState)
import Control.Monad.Throw (class MonadThrow)

import SlamData.Quasar.Class (class QuasarDSL)
import SlamData.Workspace.Card.Port as Port
import SlamData.Workspace.Card.Eval.Monad as CEM
import SlamData.Workspace.Card.SetupFormInput.Static.Model (Model)

eval
  ∷ ∀ m
  . ( MonadState CEM.CardState m
    , MonadThrow CEM.CardError m
    , QuasarDSL m
    )
  ⇒ Model
  → Port.TaggedResourcePort
  → m Port.Port
eval Nothing _ =
  CEM.throw "Please select value"
eval (Just conf) taggedResource = do
  let
    -- Here we're going to take head of taggedResource using conf.value
    -- and propagate it to metric.
    fiPort =
      { value: "" --Just conf.value
      , label: Nothing
      , taggedResource
      }
  pure $ Port.Metric fiPort
