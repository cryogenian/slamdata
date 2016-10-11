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

module SlamData.Common.Sort where

import SlamData.Prelude

import Data.Argonaut (class EncodeJson, class DecodeJson, decodeJson, jsonEmptyObject, (~>), (:=), (.?))

import SlamData.Form.Select (class OptionVal, Select(..))

import Test.StrongCheck.Arbitrary (class Arbitrary, arbitrary)

data Sort = Asc | Desc

notSort ∷ Sort → Sort
notSort Asc = Desc
notSort _ = Asc

sort2string ∷ Sort → String
sort2string Asc = "asc"
sort2string Desc = "desc"

string2sort ∷ String → String ⊹ Sort
string2sort "asc" = Right Asc
string2sort "desc" = Right Desc
string2sort _ = Left "incorrect sort string"

derive instance eqSort ∷ Eq Sort
derive instance ordSort ∷ Ord Sort

instance arbitrarySort ∷ Arbitrary Sort where
  arbitrary = arbitrary <#> if _ then Asc else Desc

instance encodeJsonSort ∷ EncodeJson Sort where
  encodeJson = case _ of
    Asc →
      "objectType" := "sort"
      ~> "value" := "asc"
      ~> jsonEmptyObject
    Desc →
      "objectType" := "sort"
      ~> "value" := "desc"
      ~> jsonEmptyObject

instance decodeJsonSort ∷ DecodeJson Sort where
  decodeJson js = do
    obj ← decodeJson js
    objectType ← obj .? "objectType"
    unless (objectType ≡ "sort")
      $ throwError "This is not encoded SlamData.Common.Sort"
    value ← obj .? "value"
    case value of
      "asc" → pure Asc
      "desc" → pure Desc
      _ → throwError "This is not encoded SlamData.Common.Sort"

instance optionValSort ∷ OptionVal Sort where
  stringVal = case _ of
    Asc → "ascending"
    Desc → "descending"

sortSelect ∷ Select Sort
sortSelect =
  Select { options: [ Asc, Desc ]
         , value: Just Asc
         }
