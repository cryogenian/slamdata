module SlamData.Workspace.Card.Setups.DimensionPicker.CommonState where

import SlamData.Prelude

import SlamData.Workspace.LevelOfDetails(LevelOfDetails(..))
import SlamData.Workspace.Card.Setups.Inputs (PickerOptions)

type CommonState a sel r =
  ( levelOfDetails ∷ LevelOfDetails
  , picker ∷ Maybe (PickerOptions a sel)
  | r)

initial ∷ ∀ a sel. Record (CommonState a sel ())
initial =
  { levelOfDetails: High
  , picker: Nothing
  }

showPicker
  ∷ ∀ r a sel
  . (Const Unit a → sel (Const Unit))
  → Array a
  → Record (CommonState a sel r)
  → Record (CommonState a sel r)
showPicker f options =
  _ { picker = Just { options, select: f (Const unit) } }
