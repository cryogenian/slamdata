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

module SlamData.Workspace.Card.SetupFormInput.Static.Semantic where

import SlamData.Prelude

import Data.Argonaut (class EncodeJson, class DecodeJson, decodeJson, jsonEmptyObject, (~>), (:=), (.?))

import SlamData.Form.Select (class OptionVal, Select(..))

import Test.StrongCheck.Arbitrary (class Arbitrary)
import Test.StrongCheck.Gen (allInArray)

data Semantic
  = Normal
  | Italic
  | Bold
  | Heading1
  | Heading2
  | Heading3
  | Heading4

printSemantic ∷ Semantic → String
printSemantic = case _ of
  Normal → "normal"
  Italic → "italic"
  Bold → "bold"
  Heading1 → "heading1"
  Heading2 → "heading2"
  Heading3 → "heading3"
  Heading4 → "heading4"


parseSemantic ∷ String → String ⊹ Semantic
parseSemantic = case _ of
  "normal" → pure Normal
  "italic" → pure Italic
  "bold" → pure Bold
  "heading1" → pure Heading1
  "heading2" → pure Heading2
  "heading3" → pure Heading3
  "heading4" → pure Heading4
  _ → throwError "This is not SlamData.Workspace.Card.SetupFormInput.Static.Semantic"

derive instance eqSetupFormInputStaticSemantic ∷ Eq Semantic
derive instance ordSetupFormInputStaticSemantic ∷ Ord Semantic

instance arbitrarySetupFormInputStaticSemantic ∷ Arbitrary Semantic where
  arbitrary = allInArray [ Normal, Italic, Bold, Heading1, Heading2, Heading3, Heading4]

instance encodeJsonSetupFormInputStaticSemantic ∷ EncodeJson Semantic where
  encodeJson j =
    "objectType" := "setup form input semantic"
    ~> "value" := printSemantic j
    ~> jsonEmptyObject

instance decodeJsonSetupFormInputStaticSemantic ∷ DecodeJson Semantic where
  decodeJson js = do
    obj ← decodeJson js
    objectType ← obj .? "objectType"
    unless (objectType ≡ "setup form input semantic")
      $ throwError "This is not encoded SlamData.Workspace.Card.SetupFormInput.Static.Semantic"
    value ← obj .? "value"
    parseSemantic value

instance optionValSetupFormInputStaticSemanticOptionVal ∷ OptionVal Semantic where
  stringVal = case _ of
    Normal → "Normal"
    Italic → "Italic"
    Bold → "Bold"
    Heading1 → "Heading1"
    Heading2 → "Heading2"
    Heading3 → "Heading3"
    Heading4 → "Heading4"

semanticSelect ∷ Select Semantic
semanticSelect =
  Select { options: [ Normal, Italic, Bold, Heading1, Heading2, Heading3, Heading4 ]
         , value: Just Heading1
         }
