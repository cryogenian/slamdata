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

module SlamData.Workspace.Card.FormInput.Model where

import SlamData.Prelude

import Data.Argonaut (Json, decodeJson, (~>), (:=), (.?))

import Test.StrongCheck.Arbitrary (arbitrary)
import Test.StrongCheck.Gen as Gen

import SlamData.Workspace.Card.FormInput.LabeledRenderer.Model as LR
import SlamData.Workspace.Card.FormInput.TextLikeRenderer.Model as TLR

data Model
  = TextLike TLR.Model
  | Labeled LR.Model

initialModel ∷ Model
initialModel =
  TextLike TLR.initialModel

eqModel ∷ Model → Model → Boolean
eqModel (TextLike r) (TextLike rr) =
  TLR.eqModel r rr
eqModel (Labeled r) (Labeled rr) =
  LR.eqModel r rr
eqModel _ _ =
  false

genModel ∷ Gen.Gen Model
genModel = do
  isTextLike ← arbitrary
  case isTextLike of
    true →
      map TextLike TLR.genModel
    false → do
      map Labeled LR.genModel

encode ∷ Model → Json
encode (TextLike r) =
  "modelType" := "textLike"
  ~> TLR.encode r
encode (Labeled r) =
  "modelType" := "labeled"
  ~> LR.encode r

decode ∷ Json → String ⊹ Model
decode = decodeJson >=> \obj → do
  mType ← obj .? "modelType"
  case mType of
    "textLike" → do
      map TextLike $ TLR.decode obj
    "labeled" → do
      map Labeled $ LR.decode obj
    _ →
      Left "Incorrect form input model type"
