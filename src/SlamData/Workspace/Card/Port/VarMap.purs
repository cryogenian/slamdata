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
  ( Var(..)
  , VarMap
  , URLVarMap
  , VarMapValue(..)
  , _VarMapValue
  , emptyVarMap
  , variables
  ) where

import SlamData.Prelude

import Data.Array as A
import Data.Json.Extended as EJSON
import Data.List as L
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Types (Iso')
import Data.String as S
import Data.StrMap as SM

import Data.Argonaut ((.?), Json)
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Encode (class EncodeJson)

import Matryoshka (embed, transAna)

import SqlSquared (Sql)
import SqlSquared as Sql
import SqlSquared.Parser as SqlP

import Text.Markdown.SlamDown.Syntax.Value as SDV

import Test.StrongCheck.Arbitrary as SC

newtype Var = Var String

derive newtype instance eqVar ∷ Eq Var
derive newtype instance ordVar ∷ Ord Var
derive newtype instance decodeVar ∷ DecodeJson Var
derive newtype instance encodeVar ∷ EncodeJson Var
derive newtype instance arbitraryVar ∷ SC.Arbitrary Var
derive instance newtypeVar :: Newtype Var _

newtype VarMapValue = VarMapValue Sql

_VarMapValue ∷ Iso' VarMapValue Sql
_VarMapValue = _Newtype


derive instance newtypeVarMapValue ∷ Newtype VarMapValue _

derive newtype instance eqVarMapValue ∷ Eq VarMapValue
derive newtype instance ordVarMapValue ∷ Ord VarMapValue
instance showVarMapValue ∷ Show VarMapValue where
  show (VarMapValue sql) = "(VarMapValue " ⊕ Sql.print sql ⊕ ")"

instance encodeJsonVarMapValue ∷ EncodeJson VarMapValue where
  encodeJson = Sql.encodeJson ∘ unwrap


instance decodeJsonVarMapValue :: DecodeJson VarMapValue where
  decodeJson json =
    (map VarMapValue $ Sql.decodeJson json)
    <|> (decodeLegacy json)
    where
    decodeLegacy = decodeJson >=> \obj →
      map VarMapValue
      $ decodeLiteral obj
      <|> decodeSetLiteral obj
      <|> decodeQueryExpr obj

    decodeLiteral obj = do
      (js ∷ Json) ← obj .? "literal"
      (ejs ∷ EJSON.EJson) ←
        EJSON.decodeEJson js
      pure $ transAna Sql.Literal ejs

    decodeSetLiteral obj = do
      (arr ∷ Array Json) ← obj .? "set"
      lst ←
        map (L.fromFoldable ∘ map unwrap)
        $ traverse decodeLegacy arr
      pure $ embed $ Sql.SetLiteral lst

    decodeQueryExpr obj  = do
      queryStr ← obj .? "query"
      lmap show $ SqlP.parse queryStr

instance valueVarMapValue ∷ SDV.Value VarMapValue where
  stringValue = VarMapValue ∘ Sql.string
  renderValue v =
    let p = Sql.print $ unwrap v
    in
     fromMaybe p
     $ S.stripSuffix (S.Pattern "\"") p
     >>= S.stripPrefix (S.Pattern "\"")

instance arbitraryVarMapValue ∷ SC.Arbitrary VarMapValue where
  arbitrary = map VarMapValue $ Sql.arbitrarySqlOfSize 2

type VarMap = SM.StrMap VarMapValue

-- | A VarMap passed through the URL - the VarMapValues are left unparsed until
-- | they are unified with the Variables card for the deck so that values can
-- | be parsed according to their defined type.
type URLVarMap = SM.StrMap String

emptyVarMap ∷ VarMap
emptyVarMap = SM.empty

variables ∷ VarMap → L.List Var
variables = L.fromFoldable ∘ map Var ∘ A.sort ∘ SM.keys
