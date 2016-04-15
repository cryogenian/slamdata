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

module SlamData.Notebook.Card.Port.VarMap
  ( VarMap
  , VarMapValue(..)
  , renderVarMapValue
  , parseVarMapValue
  ) where

import SlamData.Prelude

import Data.Array as A
import Data.SQL2.Literal as SQL2
import Data.String as S
import Data.StrMap as SM

import Data.Argonaut ((.?))
import Data.Argonaut.Core (jsonSingletonObject)
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)

import Text.Parsing.Parser as P
import Text.Parsing.Parser.Combinators as PC
import Text.Parsing.Parser.String as PS

import Text.Markdown.SlamDown.Syntax.Value as SDV

import Test.StrongCheck as SC

data VarMapValue
  = Literal SQL2.Literal
  | QueryExpr String -- TODO: syntax of SQL^2 queries

instance eqVarMapValue ∷ Eq VarMapValue where
  eq (Literal c) (Literal d) = c == d
  eq (QueryExpr q) (QueryExpr q') = q == q'
  eq _ _ = false

instance ordVarMapValue ∷ Ord VarMapValue where
  compare (Literal c) (Literal d) = compare c d
  compare _ (Literal _) = GT
  compare (Literal _) _ = LT
  compare (QueryExpr a) (QueryExpr b) = compare a b

instance showVarMapValue ∷ Show VarMapValue where
  show = renderVarMapValue

instance encodeJsonVarMapValue ∷ EncodeJson VarMapValue where
  encodeJson v =
    case v of
      Literal lit →
        jsonSingletonObject "literal" $
          SQL2.encodeJsonLiteral lit
      QueryExpr str →
        jsonSingletonObject "query" $
          encodeJson str

instance decodeJsonVarMapValue :: DecodeJson VarMapValue where
  decodeJson json = do
    obj <- decodeJson json
    decodeLiteral obj
      <|> decodeQueryExpr obj

    where
      decodeLiteral =
        (_ .? "literal")
          >=> SQL2.decodeJsonLiteral
          >>> map Literal

      decodeQueryExpr  =
        (_ .? "query")
          >>> map QueryExpr

renderVarMapValue
  ∷ VarMapValue
  → String
renderVarMapValue val =
  case val of
    Literal lit → SQL2.renderLiteral lit
    QueryExpr str → str

displayVarMapValue
  ∷ VarMapValue
  → String
displayVarMapValue val =
  case val of
    Literal lit → SQL2.displayLiteral lit
    QueryExpr str → str

instance valueVarMapValue ∷ SDV.Value VarMapValue where
  stringValue = Literal <<< SQL2.string
  renderValue = displayVarMapValue

instance arbitraryVarMapValue ∷ SC.Arbitrary VarMapValue where
  arbitrary = do
    Literal <$> SQL2.arbitraryLiteralOfSize 1
      <|> QueryExpr <$> SC.arbitrary

parseVarMapValue
  ∷ forall m
  . (Monad m)
  ⇒ P.ParserT String m VarMapValue
parseVarMapValue =
  Literal <$> PC.try SQL2.parseLiteral
    <|> QueryExpr <$> anyString
  where
    anyString =
      A.many PS.anyChar
        <#> S.fromCharArray

type VarMap = SM.StrMap VarMapValue
