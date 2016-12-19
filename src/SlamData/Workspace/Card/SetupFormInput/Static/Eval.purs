module SlamData.Workspace.Card.SetupFormInput.Static.Eval
  ( eval
  , module SlamData.Workspace.Card.SetupFormInput.Static.Model
  ) where

import SlamData.Prelude

import Control.Monad.State (class MonadState)
import Control.Monad.Throw (class MonadThrow)

import Quasar.Types (FilePath)

import SlamData.Quasar.Class (class QuasarDSL)
import SlamData.Workspace.Card.CardType.FormInputType (FormInputType(Static))
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
  → FilePath
  → m Port.Port
eval Nothing _ =
  CEM.throw "Please select value"
eval (Just conf) resource = do
  let
    fiPort =
      { formInputType: Static
      , name: Nothing
      , value: Just conf.value
      , label: Nothing
      }
  pure $ Port.FormInputParams fiPort
