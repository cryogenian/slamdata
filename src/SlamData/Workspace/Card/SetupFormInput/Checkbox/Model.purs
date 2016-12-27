{-
Copyright 2016 SlamData, Inc.

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

module SlamData.Workspace.Card.SetupFormInput.Checkbox.Model
  ( encode
  , decode
  , module SlamData.Workspace.Card.SetupFormInput.Labeled.Model
  ) where

import SlamData.Prelude

import Data.Argonaut (Json, decodeJson, (~>), (:=), (.?), isNull, jsonNull)

import SlamData.Workspace.Card.SetupFormInput.Labeled.Model
  ( Model
  , initialModel
  , eqModel
  , genModel
  )
import SlamData.Workspace.Card.SetupFormInput.Labeled.Model as Lbl

encode ∷ Model → Json
encode = case _ of
  Nothing → jsonNull
  Just r →
    "formInputType" := "checkbox"
    ~> Lbl.encode r

decode ∷ Json → String ⊹ Model
decode js
  | isNull js = pure Nothing
  | otherwise = map Just $ do
    obj ← decodeJson js
    fiType ← obj .? "formInputType"
    unless (fiType ≡ "checkbox")
      $ throwError "This is not checkbox form input setup"
    Lbl.decode obj
