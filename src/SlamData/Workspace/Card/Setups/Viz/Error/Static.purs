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

module SlamData.Workspace.Card.Setups.FormInput.Static.Error where

import SlamData.Prelude
import Utils (throwVariantError)

data FormInputStaticError
  = FIStaticNoAxis
  | FIStaticMissingAxis String

instance showFormInputStaticError ∷ Show FormInputStaticError where
  show = case _ of
    FIStaticNoAxis → "FIStaticNoAxis"
    FIStaticMissingAxis axis → "(FIStaticMissingAxis " <> show axis <> ")"

throwFormInputStaticError ∷ forall v m a. MonadThrow (Variant (formInputStatic ∷ FormInputStaticError | v)) m ⇒ FormInputStaticError → m a
throwFormInputStaticError = throwVariantError (SProxy :: SProxy "formInputStatic")
