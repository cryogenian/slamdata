{-
Copyright 2016 SlamData, Inc.

Licensed under the Apache License, Version 2.0 (the "License");
nyou may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
-}

module SlamData.Workspace.Card.Setups.DimensionPicker.CommonState where

import SlamData.Prelude

import SlamData.Workspace.LevelOfDetails(LevelOfDetails(..))
import SlamData.Workspace.Card.Setups.Inputs (PickerOptions, NewPickerOptions)

type CommonState a sel r =
  ( levelOfDetails ∷ LevelOfDetails
  , picker ∷ Maybe (PickerOptions a sel)
  | r)

initial ∷ ∀ picker. Record (levelOfDetails ∷ LevelOfDetails, picker ∷ Maybe picker)
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

type NewCommonState option action r =
  ( levelOfDetails ∷ LevelOfDetails
  , picker ∷ Maybe (NewPickerOptions option action)
  | r)

newShowPicker
  ∷ ∀ r option action
  . action
  → (Record (NewCommonState option action r) → Array option)
  → Record (NewCommonState option action r)
  → Record (NewCommonState option action r)
newShowPicker action getOptions st =
  let
    options = getOptions st
  in
    st{ picker = Just { options, action } }
