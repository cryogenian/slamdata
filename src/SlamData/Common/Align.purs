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

module SlamData.Common.Align where

import SlamData.Prelude

import Data.Argonaut (class EncodeJson, class DecodeJson, decodeJson, jsonEmptyObject, (~>), (:=), (.?))

import SlamData.Form.Select (class OptionVal, Select(..))

import Test.StrongCheck.Arbitrary (class Arbitrary)
import Test.StrongCheck.Gen (allInArray)

data Align = LeftAlign | CenterAlign | RightAlign

printAlign ∷ Align → String
printAlign = case _ of
  LeftAlign → "left"
  CenterAlign → "center"
  RightAlign → "right"

parseAlign ∷ String → String ⊹ Align
parseAlign = case _ of
  "left" → pure LeftAlign
  "right" → pure RightAlign
  "center" → pure CenterAlign
  _ → throwError "This is not SlamData.Common.Align"

derive instance eqAlign ∷ Eq Align
derive instance ordAlign ∷ Ord Align

instance arbitraryAlign ∷ Arbitrary Align where
  arbitrary = allInArray [ LeftAlign, CenterAlign, RightAlign ]

instance encodeJsonAlign ∷ EncodeJson Align where
  encodeJson = case _ of
    LeftAlign →
      "objectType" := "align"
      ~> "value" := "left"
      ~> jsonEmptyObject
    CenterAlign →
      "objectType" := "align"
      ~> "value" := "center"
      ~> jsonEmptyObject
    RightAlign →
      "objectType" := "align"
      ~> "value" := "right"
      ~> jsonEmptyObject

instance decodeJsonAlign ∷ DecodeJson Align where
  decodeJson js = do
    obj ← decodeJson js
    objectType ← obj .? "objectType"
    unless (objectType ≡ "align")
      $ throwError "This is not encoded SlamData.Common.Align"
    value ← obj .? "value"
    parseAlign value

instance optionValAlign ∷ OptionVal Align where
  stringVal = printAlign

alignSelect ∷ Select Align
alignSelect =
  Select { options: [ LeftAlign, CenterAlign, RightAlign ]
         , value: Just LeftAlign
         }
