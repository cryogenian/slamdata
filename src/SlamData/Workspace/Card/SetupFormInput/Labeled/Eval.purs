module SlamData.Workspace.Card.SetupFormInput.Labeled.Eval
  ( eval
  , module SlamData.Workspace.Card.SetupFormInput.Labeled.Model
  ) where

import SlamData.Prelude

import Data.Map as Map
import Data.Set as Set

import SlamData.Quasar.Class (class QuasarDSL)
import SlamData.Quasar.Error as QE
import SlamData.Workspace.Card.CardType.FormInputType (FormInputType)
import SlamData.Workspace.Card.Port as Port
import SlamData.Workspace.Card.Eval.CardEvalT as CET
import SlamData.Workspace.Card.SetupFormInput.Labeled.Model (Model)

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
    $ Port.SetupLabeledFormInput
        { name: Nothing
        , valueLabelMap: Map.empty
        , selectedValues: Set.empty
        , taggedResource
        , formInputType
        , cursor: conf.value
        }
