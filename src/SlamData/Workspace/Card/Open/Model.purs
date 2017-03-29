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

module SlamData.Workspace.Card.Open.Model where

import SlamData.Prelude
import Data.Argonaut (Json, class EncodeJson, encodeJson, class DecodeJson, decodeJson, (~>), (:=), (.?), jsonEmptyObject)
import SlamData.FileSystem.Resource as R
import SlamData.Workspace.Card.Port.VarMap as V
import Test.StrongCheck.Arbitrary (class Arbitrary, arbitrary)
import Test.StrongCheck.Gen as Gen

data Open
  = Resource R.Resource
  | Variable V.Var

type Model = Maybe Open

encode ∷ Model → Json
encode = encodeJson

decode ∷ Json → Either String Model
decode json = decodeJson json <|> (Just ∘ Resource <$> decodeJson json)

genModel ∷ Gen.Gen Model
genModel = arbitrary

derive instance eqOpen ∷ Eq Open
derive instance ordOpen ∷ Ord Open

instance encodeOpen ∷ EncodeJson Open where
  encodeJson = case _ of
    Resource r → "type" := "resource" ~> "value" := r ~> jsonEmptyObject
    Variable v → "type" := "variable" ~> "value" := v ~> jsonEmptyObject

instance decodeOpen ∷ DecodeJson Open where
  decodeJson json = do
    obj ← decodeJson json
    obj .? "type" >>= case _ of
      "resource" → Resource <$> obj .? "value"
      "variable" → Variable <$> obj .? "value"
      ty → throwError $ "Invalid Open Card type: " <> ty

instance arbitraryOpen ∷ Arbitrary Open where
  arbitrary = Gen.chooseInt 1 2 >>= case _ of
    1 → Resource <$> arbitrary
    _ → Variable <$> arbitrary
