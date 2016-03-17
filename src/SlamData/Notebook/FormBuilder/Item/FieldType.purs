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

module SlamData.Notebook.FormBuilder.Item.FieldType
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

import Halogen.HTML.Properties.Indexed as HP

data FieldType
  = StringFieldType
  | BooleanFieldType
  | NumericFieldType
  | DateTimeFieldType
  | DateFieldType
  | TimeFieldType
  | IntervalFieldType
  | ObjectIdFieldType
  | OrderedSetFieldType
  | ArrayFieldType
  | ObjectFieldType
  | QueryFieldType

derive instance genericFieldType :: Generic FieldType
instance eqFieldType :: Eq FieldType where eq = gEq

allFieldTypes :: Array FieldType
allFieldTypes =
  [ StringFieldType
  , DateTimeFieldType
  , DateFieldType
  , TimeFieldType
  , IntervalFieldType
  , BooleanFieldType
  , NumericFieldType
  , ObjectIdFieldType
  , OrderedSetFieldType
  , ArrayFieldType
  , ObjectFieldType
  , QueryFieldType
  ]

_FieldTypeDisplayName :: Lens.PrismP String FieldType
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
    inj OrderedSetFieldType = "Ordered Set"
    inj ArrayFieldType = "Array"
    inj ObjectFieldType = "Object"
    inj QueryFieldType = "SQL² Query"

    proj "Text" = Right StringFieldType
    proj "DateTime" = Right DateTimeFieldType
    proj "Date" = Right DateFieldType
    proj "Time" = Right TimeFieldType
    proj "Interval" = Right IntervalFieldType
    proj "Boolean" = Right BooleanFieldType
    proj "Numeric" = Right NumericFieldType
    proj "Object ID" = Right ObjectIdFieldType
    proj "Ordered Set" = Right OrderedSetFieldType
    proj "Array" = Right ArrayFieldType
    proj "Object" = Right ObjectFieldType
    proj "SQL² Query" = Right QueryFieldType
    proj t = Left t

fieldTypeToInputType
  :: FieldType
  -> HP.InputType
fieldTypeToInputType ty =
  case ty of
    StringFieldType -> HP.InputText
    DateTimeFieldType -> HP.InputDatetimeLocal
    DateFieldType -> HP.InputDate
    TimeFieldType -> HP.InputTime
    IntervalFieldType -> HP.InputText
    BooleanFieldType -> HP.InputCheckbox
    NumericFieldType -> HP.InputNumber
    ObjectIdFieldType -> HP.InputText
    OrderedSetFieldType -> HP.InputText
    ArrayFieldType -> HP.InputText
    ObjectFieldType -> HP.InputText
    QueryFieldType -> HP.InputText

instance encodeJsonFieldType :: JE.EncodeJson FieldType where
  encodeJson ty =
    J.fromString $
      case ty of
        StringFieldType -> "string"
        DateTimeFieldType -> "datetime"
        DateFieldType -> "date"
        TimeFieldType -> "time"
        IntervalFieldType -> "interval"
        BooleanFieldType -> "boolean"
        NumericFieldType -> "numeric"
        ObjectIdFieldType -> "objectId"
        OrderedSetFieldType -> "orderedSet"
        ArrayFieldType -> "array"
        ObjectFieldType -> "object"
        QueryFieldType -> "query"

instance decodeJsonFieldType :: JD.DecodeJson FieldType where
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
      proj "orderedSet" = Right OrderedSetFieldType
      proj "array" = Right ArrayFieldType
      proj "object" = Right ObjectFieldType
      proj "query" = Right QueryFieldType
      proj _ = Left "invalid FieldType"

