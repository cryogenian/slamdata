{-
Copyright 2015 SlamData, Inc.

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

module Notebook.FormBuilder.Item.FieldType
  ( FieldType(..)
  , allFieldTypes
  , _FieldTypeDisplayName
  , fieldTypeToInputType
  ) where

import Prelude
import Control.Bind ((>=>))
import Data.Argonaut as J
import Data.Argonaut.Encode as J -- imported to work around psc <0.8 reexport bug
import Data.Argonaut.Decode as J -- imported to work around psc <0.8 reexport bug
import Data.Either as E
import Data.Generic as G
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

derive instance genericFieldType :: G.Generic FieldType
instance eqFieldType :: Eq FieldType where eq = G.gEq

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

    proj "Text" = E.Right StringFieldType
    proj "DateTime" = E.Right DateTimeFieldType
    proj "Date" = E.Right DateFieldType
    proj "Time" = E.Right TimeFieldType
    proj "Interval" = E.Right IntervalFieldType
    proj "Boolean" = E.Right BooleanFieldType
    proj "Numeric" = E.Right NumericFieldType
    proj "Object ID" = E.Right ObjectIdFieldType
    proj "Ordered Set" = E.Right OrderedSetFieldType
    proj "Array" = E.Right ArrayFieldType
    proj "Object" = E.Right ObjectFieldType
    proj "SQL² Query" = E.Right QueryFieldType
    proj t = E.Left t

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

instance encodeJsonFieldType :: J.EncodeJson FieldType where
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

instance decodeJsonFieldType :: J.DecodeJson FieldType where
  decodeJson =
    J.decodeJson >=> proj
    where
      proj "string" = E.Right StringFieldType
      proj "date" = E.Right DateFieldType
      proj "time" = E.Right TimeFieldType
      proj "interval" = E.Right IntervalFieldType
      proj "datetime" = E.Right DateTimeFieldType
      proj "boolean" = E.Right BooleanFieldType
      proj "numeric" = E.Right NumericFieldType
      proj "objectId" = E.Right ObjectIdFieldType
      proj "orderedSet" = E.Right OrderedSetFieldType
      proj "array" = E.Right ArrayFieldType
      proj "object" = E.Right ObjectFieldType
      proj "query" = E.Right QueryFieldType
      proj _ = E.Left "invalid FieldType"

