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

module Notebook.FormBuilder.Item.Model
  ( Model()
  , _name
  , _fieldType
  , _defaultValue
  , initialModel
  , encode
  , decode
  , EqModel(..)
  , runEqModel
  , module Notebook.FormBuilder.Item.FieldType
  , defaultValueToVarMapValue
  , emptyValueOfFieldType
  ) where

import Prelude
import Control.Bind ((>=>))
import Data.Argonaut ((~>), (:=), (.?))
import Data.Argonaut as J
import Data.Either as E
import Data.Lens
import Data.Maybe as M
import Data.SQL2.Literal as SQL2
import Data.StrMap as SM
import Notebook.Cell.Port.VarMap as Port
import Notebook.FormBuilder.Item.FieldType
import Text.Parsing.Parser as P

type Model =
  { name :: String
  , fieldType :: FieldType
  , defaultValue :: M.Maybe String
  }

_name :: LensP Model String
_name = lens _.name _ { name = _ }

_fieldType :: LensP Model FieldType
_fieldType = lens _.fieldType _ { fieldType = _ }

_defaultValue :: LensP Model (M.Maybe String)
_defaultValue = lens _.defaultValue _ { defaultValue = _ }

newtype EqModel = EqModel Model

runEqModel
  :: EqModel
  -> Model
runEqModel (EqModel m) =
  m

instance eqEqModel :: Eq EqModel where
  eq (EqModel m1) (EqModel m2) =
    m1.name == m2.name
      && m1.fieldType == m2.fieldType
      && m1.defaultValue == m2.defaultValue


initialModel :: Model
initialModel =
  { name : ""
  , fieldType : StringFieldType
  , defaultValue : M.Nothing
  }

encode
  :: Model
  -> J.Json
encode st =
  "name" := st.name
  ~> "fieldType" := st.fieldType
  ~> "defaultValue" := st.defaultValue
  ~> J.jsonEmptyObject

decode
  :: J.Json
  -> E.Either String Model
decode =
  J.decodeJson >=> \obj ->
    { name : _, fieldType : _, defaultValue : _ }
      <$> obj .? "name"
      <*> obj .? "fieldType"
      <*> obj .? "defaultValue"

emptyValueOfFieldType
  :: FieldType
  -> Port.VarMapValue
emptyValueOfFieldType tau =
  case tau of
    StringFieldType -> Port.Literal $ SQL2.string ""
    BooleanFieldType -> Port.Literal $ SQL2.boolean true
    NumericFieldType -> Port.Literal $ SQL2.decimal 0.0
    DateTimeFieldType -> Port.Literal $ SQL2.dateTime ""
    DateFieldType -> Port.Literal $ SQL2.date ""
    TimeFieldType -> Port.Literal $ SQL2.time ""
    IntervalFieldType -> Port.Literal $ SQL2.interval ""
    ObjectIdFieldType -> Port.Literal $ SQL2.objectId ""
    OrderedSetFieldType -> Port.Literal $ SQL2.orderedSet []
    ArrayFieldType -> Port.Literal $ SQL2.array []
    ObjectFieldType -> Port.Literal $ SQL2.object SM.empty
    QueryFieldType -> Port.QueryExpr ""

defaultValueToVarMapValue
  :: FieldType
  -> String
  -> M.Maybe Port.VarMapValue
defaultValueToVarMapValue ty str =
  case ty of
    StringFieldType -> M.Just $ Port.Literal $ SQL2.string str
    DateTimeFieldType -> M.Just $ Port.Literal $ SQL2.dateTime str
    DateFieldType -> M.Just $ Port.Literal $ SQL2.date str
    TimeFieldType -> M.Just $ Port.Literal $ SQL2.time str
    IntervalFieldType -> M.Just $ Port.Literal $ SQL2.interval str
    ObjectIdFieldType -> M.Just $ Port.Literal $ SQL2.objectId str
    QueryFieldType -> M.Just $ Port.QueryExpr $ str
    _ | str == "" -> M.Nothing
    _ ->
      P.runParser str SQL2.parseLiteral
        # E.either (\_ -> M.Nothing) (Port.Literal >>> M.Just)
