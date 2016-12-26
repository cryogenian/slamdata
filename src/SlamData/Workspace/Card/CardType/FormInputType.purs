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

module SlamData.Workspace.Card.CardType.FormInputType
  ( FormInputType(..)
  , parseFormInputType
  , printFormInputType
  , formInputName
  , formInputLightIconSrc
  , formInputDarkIconSrc
  , allFormInputTypes
  , maximumCountOfEntries
  , maximumCountOfSelectedValues
  ) where

import SlamData.Prelude

import Data.Argonaut (fromString, class EncodeJson, class DecodeJson, decodeJson)

import Test.StrongCheck.Arbitrary as SC
import Test.StrongCheck.Gen as Gen

-- This data type is actually sum of three: LabeledLike, TextLike and Static
-- It's a bit inconvenient to use this like LabeledLike ⊹ TextLike ⊹ Static though.
-- (And unfortunately isn't very elegant to use this like ADT :( )
data FormInputType
  = Dropdown
  | Static
  | Text
  | Numeric
  | Checkbox
  | Radio
  | Date
  | Time
  | Datetime

allFormInputTypes ∷ Array FormInputType
allFormInputTypes =
  [ Dropdown
  , Static
  , Text
  , Numeric
  , Checkbox
  , Radio
  , Date
  , Time
  , Datetime
  ]

parseFormInputType ∷ String → String ⊹ FormInputType
parseFormInputType = case _ of
  "dropdown" → pure Dropdown
  "static" → pure Static
  "text" → pure Text
  "numeric" → pure Numeric
  "checkbox" → pure Checkbox
  "radio" → pure Radio
  "date" → pure Date
  "time" → pure Time
  "datetime" → pure Datetime
  _ → Left "incorrect formInputType"

printFormInputType ∷ FormInputType → String
printFormInputType = case _ of
  Dropdown → "dropdown"
  Static → "static"
  Text → "text"
  Numeric → "numeric"
  Checkbox → "checkbox"
  Radio → "radio"
  Date → "date"
  Time → "time"
  Datetime → "datetime"

formInputName ∷ FormInputType → String
formInputName = case _ of
  Dropdown → "Dropdown"
  Static → "Static Text"
  Text → "Text Input"
  Numeric → "Numeric Input"
  Checkbox → "Checkbox Group"
  Radio → "Radio Group"
  Date → "Date Input"
  Time → "Time Input"
  Datetime → "Date/Time Input"

derive instance eqFormInputType ∷ Eq FormInputType
derive instance ordFormInputType ∷ Ord FormInputType

instance encodeJsonFormInputType ∷ EncodeJson FormInputType where
  encodeJson = fromString ∘ printFormInputType

instance decodeJsonFormInputType ∷ DecodeJson FormInputType where
  decodeJson = decodeJson >=> parseFormInputType

instance arbitraryFormInputType ∷ SC.Arbitrary FormInputType where
  arbitrary = Gen.allInArray allFormInputTypes

formInputLightIconSrc ∷ FormInputType → String
formInputLightIconSrc = case _ of
  Dropdown → "img/formInputs/light/dropdown.svg"
  Static → "img/formInputs/light/static.svg"
  Text → "img/formInputs/light/text.svg"
  Numeric → "img/formInputs/light/numeric.svg"
  Checkbox → "img/formInputs/light/checkbox.svg"
  Radio → "img/formInputs/light/radio.svg"
  Date → "img/formInputs/light/date.svg"
  Time → "img/formInputs/light/time.svg"
  Datetime → "img/formInputs/light/datetime.svg"

formInputDarkIconSrc ∷ FormInputType → String
formInputDarkIconSrc = case _ of
  Dropdown → "img/formInputs/dark/dropdown.svg"
  Static → "img/formInputs/dark/static.svg"
  Text → "img/formInputs/dark/text.svg"
  Numeric → "img/formInputs/dark/numeric.svg"
  Checkbox → "img/formInputs/dark/checkbox.svg"
  Radio → "img/formInputs/dark/radio.svg"
  Date → "img/formInputs/dark/date.svg"
  Time → "img/formInputs/dark/time.svg"
  Datetime → "img/formInputs/dark/datetime.svg"

-- If there is more records in JArray don't even try to display it in ShowFormInput
maximumCountOfEntries ∷ FormInputType → Int
maximumCountOfEntries = case _ of
  Dropdown → 100
  Radio → 100
  Checkbox → 100
  _ → top

maximumCountOfSelectedValues ∷ FormInputType → Int
maximumCountOfSelectedValues = case _ of
  Dropdown → 1
  Radio → 1
  _ → top
