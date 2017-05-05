{-
Copyright 2017 SlamData, Inc.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
-}

module SlamData.Workspace.Card.Setups.FormInput.Labeled.Error where

import SlamData.Prelude
import SlamData.Workspace.Card.CardType.FormInputType (FormInputType)

data FormInputLabeledError
  = FILabeledNoAxisError FormInputType
  | FILabeledEmptyResourceError FormInputType
  | FILabeledTooManyEntries
    { formInputType ∷ FormInputType
    , maximum ∷ Int
    , entryCount ∷ Int
    }
  | FILabeledTooManySelected
    { formInputType ∷ FormInputType
    , maximum ∷ Int
    , selectedCount ∷ Int
    }
  | FILabeledNonUniqueLabelError FormInputType (Maybe String)

instance showFormInputLabeledError ∷ Show FormInputLabeledError where
  show = case _ of
    FILabeledNoAxisError fit → "(FILabeledNoAxisError " <> show fit <> ")"
    FILabeledEmptyResourceError fit → "(FILabeledEmptyResourceError " <> show fit <> ")"
    FILabeledTooManyEntries { formInputType, maximum, entryCount } →
      "(FILabeledTooManyEntries "
      <> "{ formInputType: " <> show formInputType
      <> ", maximum: " <> show maximum
      <> ", entryCount: " <> show entryCount
      <> " })"
    FILabeledTooManySelected { formInputType, maximum, selectedCount } →
      "(FILabeledTooManySelected "
      <> "{ formInputType: " <> show formInputType
      <> ", maximum: " <> show maximum
      <> ", selectedCount: " <> show selectedCount
      <> " })"
    FILabeledNonUniqueLabelError fit label →
      "(FILabeledNonUniqueLabelError " <> show fit <> " " <> show label <> ")"
