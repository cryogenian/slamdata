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

module SlamData.Workspace.FormBuilder.Item.Model
  ( Model
  , _name
  , _fieldType
  , _defaultValue
  , initialModel
  , genModel
  , encode
  , decode
  , EqModel(..)
  , runEqModel
  , defaultValueToVarMapValue
  , module SlamData.Workspace.FormBuilder.Item.FieldType
  ) where

import SlamData.Prelude

import Data.Argonaut ((~>), (:=), (.?))
import Data.Argonaut as J
import Data.Formatter.DateTime as Fd
import Data.List as L
import Data.Lens (Lens', lens)
import Data.String as Str

import SlamData.Workspace.Card.Port.VarMap as Port
import SlamData.Workspace.FormBuilder.Item.FieldType (FieldType(..), _FieldTypeDisplayName, allFieldTypes, fieldTypeToInputType)

import SqlSquare as Sql

import Test.StrongCheck.Arbitrary as SC
import Test.StrongCheck.Gen as Gen

import Utils (hush)

type Model =
  { name ∷ String
  , fieldType ∷ FieldType
  , defaultValue ∷ Maybe String
  }

genModel ∷ Gen.Gen Model
genModel = do
  name ← SC.arbitrary
  fieldType ← SC.arbitrary
  defaultValue ← SC.arbitrary
  pure { name, fieldType, defaultValue }

_name ∷ Lens' Model String
_name = lens _.name _ { name = _ }

_fieldType ∷ Lens' Model FieldType
_fieldType = lens _.fieldType _ { fieldType = _ }

_defaultValue ∷ Lens' Model (Maybe String)
_defaultValue = lens _.defaultValue _ { defaultValue = _ }

newtype EqModel = EqModel Model

runEqModel
  ∷ EqModel
  → Model
runEqModel (EqModel m) =
  m

derive instance eqEqModel ∷ Eq EqModel

initialModel ∷ Model
initialModel =
  { name : ""
  , fieldType : StringFieldType
  , defaultValue : Nothing
  }

encode
  ∷ Model
  → J.Json
encode st =
  "name" := st.name
  ~> "fieldType" := st.fieldType
  ~> "defaultValue" := st.defaultValue
  ~> J.jsonEmptyObject

decode
  ∷ J.Json
  → Either String Model
decode =
  J.decodeJson >=> \obj → do
    name ← obj .? "name"
    fieldType ← obj .? "fieldType"
    let fixValue = if fieldType == DateFieldType then fixupDate else id
    defaultValue ← map fixValue <$> obj .? "defaultValue"
    pure { name, fieldType, defaultValue }

defaultValueToVarMapValue
  ∷ FieldType
  → String
  → Maybe Port.VarMapValue
defaultValueToVarMapValue ty str = map Port.VarMapValue $  case ty of
    StringFieldType →
      Just $ Sql.string str
    DateTimeFieldType → do
      guard
        $ isRight
        $ Fd.unformatDateTime "YYYY-MM-DDTHH:mm:ssZ"
        $ tweak str
      pure
        $ Sql.invokeFunction "TIMESTAMP"
        $ L.singleton
        $ Sql.string str
    DateFieldType → do
      guard
        $ isRight
        $ Fd.unformatDateTime "YYYY-MM-DD" str
      pure
        $ Sql.invokeFunction "DATE"
        $ L.singleton
        $ Sql.string str
    TimeFieldType → do
      guard
        $ isRight
        $ Fd.unformatDateTime "HH:mm:ss" str
      pure
        $ Sql.invokeFunction "TIME"
        $ L.singleton
        $ Sql.string str
    IntervalFieldType →
      pure
        $ Sql.invokeFunction "INTERVAL"
        $ L.singleton
        $ Sql.string str
    ObjectIdFieldType →
      pure
        $ Sql.invokeFunction "OID"
        $ L.singleton
        $ Sql.string str

    SqlExprFieldType →
      hush $ Sql.parse str
    SqlIdentifierFieldType →
      pure $ Sql.ident str
    _ | str == "" → Nothing
    _ → hush $ Sql.parse str

tweak ∷ String → String
tweak s
  | Str.length s ≡ 19 = s <> "Z"
  | Str.charAt 10 s ≡ Just ' ' = tweak (Str.take 10 s <> "T" <> Str.drop 11 s)
  | otherwise = s

-- Truncate value to only include YYYY-MM-DD part, in case of Quasar mongo
-- connector issue that cannot represent dates distinct from datetimes.
fixupDate ∷ String → String
fixupDate = Str.take 10
