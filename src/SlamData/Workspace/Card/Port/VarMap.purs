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

module SlamData.Workspace.Card.Port.VarMap
  ( VarMap
  , URLVarMap
  , VarMapValue(..)
  , renderVarMapValue
  , emptyVarMap
  ) where

import SlamData.Prelude

import Data.Foldable as F
import Data.HugeNum as HN
import Data.Json.Extended as EJSON
import Data.StrMap as SM

import Data.Argonaut ((.?))
import Data.Argonaut.Core (jsonSingletonObject)
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)

import Text.Markdown.SlamDown.Syntax.Value as SDV

import Test.StrongCheck.Gen as Gen
import Test.StrongCheck.Arbitrary as SC

data VarMapValue
  = Literal EJSON.EJson
  | SetLiteral (Array EJSON.EJson)
  | QueryExpr String -- TODO: syntax of SQL^2 queries

derive instance eqVarMapValue ∷ Eq VarMapValue

derive instance ordVarMapValue ∷ Ord VarMapValue

instance showVarMapValue ∷ Show VarMapValue where
  show = renderVarMapValue

instance encodeJsonVarMapValue ∷ EncodeJson VarMapValue where
  encodeJson v =
    case v of
      Literal ejson →
        jsonSingletonObject "literal" $
          encodeJson ejson
      SetLiteral as →
        jsonSingletonObject "set" $
          encodeJson as
      QueryExpr str →
        jsonSingletonObject "query" $
          encodeJson str

instance decodeJsonVarMapValue :: DecodeJson VarMapValue where
  decodeJson json = do
    obj <- decodeJson json
    decodeLiteral obj
      <|> decodeSetLiteral obj
      <|> decodeQueryExpr obj

    where
      decodeLiteral =
        (_ .? "literal")
          >=> decodeJson
          >>> map Literal

      decodeSetLiteral =
        (_ .? "set")
          >=> decodeJson
          >>> map SetLiteral

      decodeQueryExpr  =
        (_ .? "query")
          >>> map QueryExpr

renderVarMapValue
  ∷ VarMapValue
  → String
renderVarMapValue val =
  case val of
    Literal lit → EJSON.renderEJson lit
    SetLiteral as → "(" <> F.intercalate "," (EJSON.renderEJson <$> as) <> ")"
    QueryExpr str → str

displayVarMapValue
  ∷ VarMapValue
  → String
displayVarMapValue val =
  case val of
    Literal lit → displayEJson lit
    SetLiteral as → "(" <> F.intercalate ", " (displayEJson <$> as) <> ")"
    QueryExpr str → str

displayEJsonF
  ∷ ∀ a
  . (a → String)
  → EJSON.EJsonF a
  → String
displayEJsonF rec d =
  case d of
    EJSON.Null → "null"
    EJSON.Boolean b → if b then "true" else "false"
    EJSON.Integer i → show i
    EJSON.Decimal a → HN.toString a
    EJSON.String str → str
    EJSON.Timestamp str → str
    EJSON.Time str → str
    EJSON.Date str → str
    EJSON.Interval str → str
    EJSON.ObjectId str → str
    EJSON.Array ds → squares $ commaSep ds
    EJSON.Object ds → braces $ renderPairs ds
  where
    commaSep
      ∷ ∀ f
      . (Functor f, F.Foldable f)
      ⇒ f a
      → String
    commaSep =
      F.intercalate "," <<<
        map rec

    renderPairs
      ∷ Array (Tuple a a)
      → String
    renderPairs =
      F.intercalate ", " <<<
        map \(Tuple k v) →
          rec k <> ": " <> rec v

    parens
      ∷ String
      → String
    parens str =
      "(" <> str <> ")"

    squares
      ∷ String
      → String
    squares str =
      "[" <> str <> "]"

    braces
      ∷ String
      → String
    braces str =
      "{" <> str <> "}"

-- | A more readable, but forgetful renderer
displayEJson
  ∷ EJSON.EJson
  → String
displayEJson c =
  displayEJsonF displayEJson $
    EJSON.unroll c


instance valueVarMapValue ∷ SDV.Value VarMapValue where
  stringValue = Literal <<< EJSON.string
  renderValue = displayVarMapValue

instance arbitraryVarMapValue ∷ SC.Arbitrary VarMapValue where
  arbitrary =
    Literal <$> EJSON.arbitraryJsonEncodableEJsonOfSize 1
      <|> SetLiteral <$> Gen.arrayOf (EJSON.arbitraryJsonEncodableEJsonOfSize 1)
      <|> QueryExpr <$> SC.arbitrary

type VarMap = SM.StrMap VarMapValue

-- | A VarMap passed through the URL - the VarMapValues are left unparsed until
-- | they are unified with the Variables card for the deck so that values can
-- | be parsed according to their defined type.
type URLVarMap = SM.StrMap String

emptyVarMap ∷ VarMap
emptyVarMap = SM.empty
