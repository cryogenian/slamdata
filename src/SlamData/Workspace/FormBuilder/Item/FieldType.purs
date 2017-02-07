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

module SlamData.Workspace.FormBuilder.Item.FieldType
  ( FieldType(..)
  , allFieldTypes
  , _FieldTypeDisplayName
  , fieldTypeToInputType
  ) where

import SlamData.Prelude

import Data.Argonaut as J
import Data.Argonaut.Decode as JD
import Data.Argonaut.Encode as JE
import Data.Lens as Lens
import Data.List as L

import Halogen.HTML.Properties as HP

import Test.StrongCheck.Arbitrary as SC
import Test.StrongCheck.Gen as Gen

data FieldType
  = StringFieldType
  | BooleanFieldType
  | NumericFieldType
  | DateTimeFieldType
  | DateFieldType
  | TimeFieldType
  | IntervalFieldType
  | ObjectIdFieldType
  | ArrayFieldType
  | ObjectFieldType
  | SqlExprFieldType
  | SqlIdentifierFieldType

derive instance genericFieldType ∷ Generic FieldType
derive instance eqFieldType ∷ Eq FieldType
derive instance ordFieldType ∷ Ord FieldType

instance arbitraryFieldType ∷ SC.Arbitrary FieldType where
  arbitrary =
    Gen.elements
      StringFieldType
      (L.fromFoldable allFieldTypes)


allFieldTypes ∷ Array FieldType
allFieldTypes =
  [ StringFieldType
  , DateTimeFieldType
  , DateFieldType
  , TimeFieldType
  , IntervalFieldType
  , BooleanFieldType
  , NumericFieldType
  , ObjectIdFieldType
  , ArrayFieldType
  , ObjectFieldType
  , SqlExprFieldType
  , SqlIdentifierFieldType
  ]

_FieldTypeDisplayName ∷ Lens.Prism' String FieldType
_FieldTypeDisplayName = Lens.prism inj proj
  where
    inj StringFieldType = "Text"
    inj DateTimeFieldType = "DateTime"
    inj DateFieldType = "Date"
    inj TimeFieldType = "Time"
    inj IntervalFieldType = "Interval"
    inj BooleanFieldType = "Boolean"
    inj NumericFieldType = "Numeric"
    inj ObjectIdFieldType = "Object ID"
    inj ArrayFieldType = "Array"
    inj ObjectFieldType = "Object"
    inj SqlExprFieldType = "SQL² Expression"
    inj SqlIdentifierFieldType = "SQL² Identifier"

    proj "Text" = Right StringFieldType
    proj "DateTime" = Right DateTimeFieldType
    proj "Date" = Right DateFieldType
    proj "Time" = Right TimeFieldType
    proj "Interval" = Right IntervalFieldType
    proj "Boolean" = Right BooleanFieldType
    proj "Numeric" = Right NumericFieldType
    proj "Object ID" = Right ObjectIdFieldType
    proj "Array" = Right ArrayFieldType
    proj "Object" = Right ObjectFieldType
    proj "SQL² Identifier" = Right SqlIdentifierFieldType
    proj "SQL² Expression" = Right SqlExprFieldType
    proj t = Left t

fieldTypeToInputType
  ∷ FieldType
  → HP.InputType
fieldTypeToInputType ty =
  case ty of
    StringFieldType → HP.InputText
    DateTimeFieldType → HP.InputDatetimeLocal
    DateFieldType → HP.InputDate
    TimeFieldType → HP.InputTime
    IntervalFieldType → HP.InputText
    BooleanFieldType → HP.InputCheckbox
    NumericFieldType → HP.InputNumber
    ObjectIdFieldType → HP.InputText
    ArrayFieldType → HP.InputText
    ObjectFieldType → HP.InputText
    SqlExprFieldType → HP.InputText
    SqlIdentifierFieldType → HP.InputText

-- PLEASE REMEMBER: do not change these tags!
instance encodeJsonFieldType ∷ JE.EncodeJson FieldType where
  encodeJson ty =
    J.fromString
      case ty of
        StringFieldType → "string"
        DateTimeFieldType → "datetime"
        DateFieldType → "date"
        TimeFieldType → "time"
        IntervalFieldType → "interval"
        BooleanFieldType → "boolean"
        NumericFieldType → "numeric"
        ObjectIdFieldType → "objectId"
        ArrayFieldType → "array"
        ObjectFieldType → "object"
        SqlExprFieldType → "query"
        SqlIdentifierFieldType → "sql-identifier"

instance decodeJsonFieldType ∷ JD.DecodeJson FieldType where
  decodeJson =
    J.decodeJson >=> proj
    where
      proj "string" = Right StringFieldType
      proj "date" = Right DateFieldType
      proj "time" = Right TimeFieldType
      proj "interval" = Right IntervalFieldType
      proj "datetime" = Right DateTimeFieldType
      proj "boolean" = Right BooleanFieldType
      proj "numeric" = Right NumericFieldType
      proj "objectId" = Right ObjectIdFieldType
      proj "orderedSet" = Right ArrayFieldType -- for compatibility
      proj "array" = Right ArrayFieldType
      proj "object" = Right ObjectFieldType
      proj "query" = Right SqlExprFieldType
      proj "sql-identifier" = Right SqlIdentifierFieldType
      proj _ = Left "invalid FieldType"
