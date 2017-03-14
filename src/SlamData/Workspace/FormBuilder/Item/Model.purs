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
import Data.DateTime as DT
import Data.Json.Extended as EJSON
import Data.Json.Extended.Signature.Parse as EJP
import Data.Lens (Lens', lens)
import Data.String as Str

import SlamData.Workspace.Card.Port.VarMap as Port
import SlamData.Workspace.FormBuilder.Item.FieldType (FieldType(..), _FieldTypeDisplayName, allFieldTypes, fieldTypeToInputType)

import Text.Parsing.Parser (runParser) as P
import Text.Parsing.Parser.Combinators (try) as P

import Test.StrongCheck.Arbitrary as SC
import Test.StrongCheck.Gen as Gen

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
defaultValueToVarMapValue ty str =
  case ty of
    StringFieldType → Just $ Port.Literal $ EJSON.string str
    DateTimeFieldType → Port.Literal <$> parseTimestamp str
    DateFieldType → Port.Literal <$> parseDate str
    TimeFieldType → Port.Literal <$> parseTime str
    IntervalFieldType → Just $ Port.Literal $ EJSON.interval str
    ObjectIdFieldType → Just $ Port.Literal $ EJSON.objectId str
    SqlExprFieldType → Just $ Port.QueryExpr $ str
    SqlIdentifierFieldType → Just $ Port.QueryExpr $ "`" ⊕ str ⊕ "`"
    _ | str == "" → Nothing
    _ →
      P.runParser str EJSON.parseEJson
        # either (\_ → Nothing) (Port.Literal >>> Just)

parseTimestamp ∷ String → Maybe EJSON.EJson
parseTimestamp str =
  case P.runParser (tweak str) EJP.parseTimestamp of
    Left _ → Nothing
    Right dt → Just $ EJSON.timestamp dt
  where
  tweak ∷ String → String
  tweak s
    | Str.length s == 19 = s <> "Z"
    | Str.charAt 10 s == Just ' ' = tweak (Str.take 10 s <> "T" <> Str.drop 11 s)
    | otherwise = s

parseTime ∷ String → Maybe EJSON.EJson
parseTime str =
  either (const Nothing) (Just ∘ EJSON.time) $
    P.runParser str (P.try fromTimestamp <|> EJP.parseTime)
  where
  fromTimestamp = DT.time <$> EJP.parseTimestamp

parseDate ∷ String → Maybe EJSON.EJson
parseDate str =
  either (const Nothing) (Just ∘ EJSON.date) $
    P.runParser str (P.try fromTimestamp <|> EJP.parseDate)
  where
  fromTimestamp = DT.date <$> EJP.parseTimestamp

-- Truncate value to only include YYYY-MM-DD part, in case of Quasar mongo
-- connector issue that cannot represent dates distinct from datetimes.
fixupDate :: String -> String
fixupDate = Str.take 10
