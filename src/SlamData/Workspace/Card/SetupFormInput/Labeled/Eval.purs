module SlamData.Workspace.Card.SetupFormInput.Labeled.Eval
  ( eval
  , module SlamData.Workspace.Card.SetupFormInput.Labeled.Model
  ) where

import SlamData.Prelude

import Control.Monad.State (class MonadState)
import Control.Monad.Throw (class MonadThrow)

import Data.Map as Map
import Data.Set as Set

import SlamData.Quasar.Class (class QuasarDSL)
import SlamData.Workspace.Card.CardType.FormInputType (FormInputType)
import SlamData.Workspace.Card.Port as Port
import SlamData.Workspace.Card.Eval.Monad as CEM
import SlamData.Workspace.Card.SetupFormInput.Labeled.Model (Model)

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
    $ Port.SetupLabeledFormInput
        { name: conf.name
        , valueLabelMap: Map.empty
        , selectedValues: Set.empty
        , taggedResource
        , formInputType
        , cursor: conf.value
        }
