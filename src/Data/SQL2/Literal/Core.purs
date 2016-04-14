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

module Data.SQL2.Literal.Core
  ( Literal
  , LiteralF(..)

  , renderLiteralF
  , renderLiteral

  , displayLiteralF
  , displayLiteral

  , arbitraryLiteralF
  , arbitraryLiteralOfSize
  , arbitraryLiteral

  , null
  , boolean
  , integer
  , decimal
  , string
  , dateTime
  , date
  , time
  , interval
  , objectId
  , orderedSet
  , object
  , array

  , decodeJsonLiteralF
  , decodeJsonLiteral

  , encodeJsonLiteralF
  , encodeJsonLiteral
  ) where

import Prelude

import Control.Alt ((<|>))
import Control.Bind ((>=>))

import Data.Eq1 (class Eq1)
import Data.Ord1 (class Ord1)
import Data.Foldable as F
import Data.Functor.Mu (Mu, unroll, roll)

import Data.Argonaut.Core as JSON
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Encode (encodeJson)
import Data.Argonaut.Combinators ((.?))
import Data.Either as E
import Data.HugeNum as HN
import Data.Int as Int
import Data.List as L
import Data.Maybe as M
import Data.String as S
import Data.StrMap as SM
import Data.String.Regex as Rx
import Data.Traversable as T
import Data.Tuple (Tuple(..))

import Test.StrongCheck as SC
import Test.StrongCheck.Gen as Gen

data LiteralF a
  = Null
  | Boolean Boolean
  | Integer Int
  | Decimal HN.HugeNum
  | String String
  | DateTime String
  | Date String
  | Time String
  | Interval String
  | ObjectId String
  | OrderedSet (Array a)
  | Array (Array a)
  | Object (SM.StrMap a)

instance functorLiteralF ∷ Functor LiteralF where
  map f c =
    case c of
      Null → Null
      Boolean b → Boolean b
      Integer i → Integer i
      Decimal a → Decimal a
      String s → String s
      DateTime s → DateTime s
      Date s → Date s
      Time s → Time s
      Interval s → Interval s
      ObjectId s → ObjectId s
      OrderedSet cs → OrderedSet $ f <$> cs
      Array cs → Array $ f <$> cs
      Object cs → Object $ f <$> cs

instance eq1LiteralF ∷ Eq1 LiteralF where
  eq1 Null Null = true
  eq1 (Boolean b1) (Boolean b2) = b1 == b2
  eq1 (Integer i) (Integer j) = i == j
  eq1 (Decimal a) (Decimal b) = a == b
  eq1 (Integer i) (Decimal b) = HN.fromNumber (Int.toNumber i) == b
  eq1 (Decimal a) (Integer j) = a == HN.fromNumber (Int.toNumber j)
  eq1 (String a) (String b) = a == b
  eq1 (DateTime a) (DateTime b) = a == b
  eq1 (Date a) (Date b) = a == b
  eq1 (Time a) (Time b) = a == b
  eq1 (Interval a) (Interval b) = a == b
  eq1 (ObjectId a) (ObjectId b) = a == b
  eq1 (OrderedSet xs) (OrderedSet ys) = xs == ys
  eq1 (Array xs) (Array ys) = xs == ys
  eq1 (Object xs) (Object ys) = xs == ys
  eq1 _ _ = false

instance ord1LiteralF ∷ Ord1 LiteralF where
  compare1 Null Null = EQ
  compare1 _ Null = GT
  compare1 Null _ = LT

  compare1 (Boolean b1) (Boolean b2) = compare b1 b2
  compare1 _ (Boolean _) = GT
  compare1 (Boolean _) _ = LT

  compare1 (Integer i) (Integer j) = compare i j
  compare1 (Integer i) (Decimal b) = compare (HN.fromNumber (Int.toNumber i)) b
  compare1 (Decimal a) (Integer j) = compare a (HN.fromNumber (Int.toNumber j))
  compare1 _ (Integer _) = GT
  compare1 (Integer _) _ = LT

  compare1 (Decimal a) (Decimal b) = compare a b
  compare1 _ (Decimal _) = GT
  compare1 (Decimal _) _ = LT

  compare1 (String a) (String b) = compare a b
  compare1 _ (String _) = GT
  compare1 (String _) _ = LT

  compare1 (DateTime a) (DateTime b) = compare a b
  compare1 _ (DateTime _) = GT
  compare1 (DateTime _) _ = LT

  compare1 (Date a) (Date b) = compare a b
  compare1 _ (Date _) = GT
  compare1 (Date _) _ = LT

  compare1 (Time a) (Time b) = compare a b
  compare1 _ (Time _) = GT
  compare1 (Time _) _ = LT

  compare1 (Interval a) (Interval b) = compare a b
  compare1 _ (Interval _) = GT
  compare1 (Interval _) _ = LT

  compare1 (ObjectId a) (ObjectId b) = compare a b
  compare1 _ (ObjectId _) = GT
  compare1 (ObjectId _) _ = LT

  compare1 (OrderedSet a) (OrderedSet b) = compare a b
  compare1 _ (OrderedSet _) = GT
  compare1 (OrderedSet _) _ = LT

  compare1 (Array a) (Array b) = compare a b
  compare1 _ (Array _) = GT
  compare1 (Array _) _ = LT

  compare1 (Object a) (Object b) = compare (SM.toList a) (SM.toList b)

arbitraryStrMap
  ∷ forall a
  . Gen.Gen a
  → Gen.Gen (SM.StrMap a)
arbitraryStrMap arb =
  map (SM.fromList <<< L.toList) <<< Gen.arrayOf $
    Tuple <$> SC.arbitrary <*> arb

arbitraryDecimal ∷ Gen.Gen HN.HugeNum
arbitraryDecimal = HN.fromNumber <<< Data.Int.toNumber <$> SC.arbitrary

arbitraryBaseLiteralF ∷ ∀ a. Gen.Gen (LiteralF a)
arbitraryBaseLiteralF =
  Gen.oneOf (pure Null)
    [ Boolean <$> SC.arbitrary
    , Integer <$> SC.arbitrary
    , Decimal <$> arbitraryDecimal
    , String <$> SC.arbitrary
    , DateTime <$> SC.arbitrary
    , Date <$> SC.arbitrary
    , Time <$> SC.arbitrary
    , Interval <$> SC.arbitrary
    , ObjectId <$> SC.arbitrary
    , pure Null
    ]

arbitraryLiteralF
  ∷ ∀ a
  . Gen.Gen a
  → Gen.Gen (LiteralF a)
arbitraryLiteralF rec =
  Gen.oneOf (pure Null)
    [ arbitraryBaseLiteralF
    , OrderedSet <$> Gen.arrayOf rec
    , Array <$> Gen.arrayOf rec
    , Object <$> arbitraryStrMap rec
    ]

arbitraryLiteralOfSize
  ∷ Gen.Size
  → Gen.Gen Literal
arbitraryLiteralOfSize size =
  roll <$>
    case size of
      0 → arbitraryBaseLiteralF
      n → arbitraryLiteralF (arbitraryLiteralOfSize (n - 1))

arbitraryLiteral ∷ Gen.Gen Literal
arbitraryLiteral = Gen.sized arbitraryLiteralOfSize

renderLiteralF
  ∷ ∀ a
  . (a → String)
  → LiteralF a
  → String
renderLiteralF rec d =
  case d of
    Null → "null"
    Boolean b → if b then "true" else "false"
    Integer i → show i
    Decimal a → HN.toString a
    String str → stringLiteral str
    DateTime str → tagged "TIMESTAMP" str
    Time str → tagged "TIME" str
    Date str → tagged "DATE" str
    Interval str → tagged "INTERVAL" str
    ObjectId str → tagged "OID" str
    OrderedSet ds → parens $ commaSep ds
    Array ds → squares $ commaSep ds
    Object ds → braces $ renderMap ds
  where
    tagged
      ∷ String
      → String
      → String
    tagged tag str =
      tag <> parens (stringLiteral str)

    replaceAll
      ∷ String
      → String
      → String
      → String
    replaceAll i =
      Rx.replace $
        Rx.regex i $
          Rx.noFlags { global = true }

    -- | Surround text in double quotes, escaping internal double quotes.
    stringLiteral
      ∷ String
      → String
    stringLiteral str =
      "\"" <> replaceAll "\"" "\\\"" str <> "\""

    commaSep
      ∷ ∀ f
      . (Functor f, F.Foldable f)
      ⇒ f a
      → String
    commaSep =
      F.intercalate "," <<<
        map rec

    renderMap
      ∷ SM.StrMap a
      → String
    renderMap =
      F.intercalate ", " <<<
        SM.foldMap \k v →
          [ stringLiteral k <> ": " <> rec v ]

displayLiteralF
  ∷ ∀ a
  . (a → String)
  → LiteralF a
  → String
displayLiteralF rec d =
  case d of
    Null → "null"
    Boolean b → if b then "true" else "false"
    Integer i → show i
    Decimal a → HN.toString a
    String str → str
    DateTime str → str
    Time str → str
    Date str → str
    Interval str → str
    ObjectId str → str
    OrderedSet ds → parens $ commaSep ds
    Array ds → squares $ commaSep ds
    Object ds → braces $ renderMap ds
  where
    commaSep
      ∷ ∀ f
      . (Functor f, F.Foldable f)
      ⇒ f a
      → String
    commaSep =
      F.intercalate "," <<<
        map rec

    renderMap
      ∷ SM.StrMap a
      → String
    renderMap =
      F.intercalate ", " <<<
        SM.foldMap \k v →
          [ k <> ": " <> rec v ]

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

instance showLiteralF ∷ (Show a) ⇒ Show (LiteralF a) where
  show = renderLiteralF show


type Literal = Mu LiteralF

null ∷ Literal
null = roll Null

boolean ∷ Boolean → Literal
boolean = roll <<< Boolean

integer ∷ Int → Literal
integer = roll <<< Integer

decimal ∷ HN.HugeNum → Literal
decimal = roll <<< Decimal

string ∷ String → Literal
string = roll <<< String

dateTime ∷ String → Literal
dateTime = roll <<< DateTime

date ∷ String → Literal
date = roll <<< Date

time ∷ String → Literal
time = roll <<< Time

interval ∷ String → Literal
interval = roll <<< Interval

objectId ∷ String → Literal
objectId = roll <<< ObjectId

orderedSet ∷ Array Literal → Literal
orderedSet = roll <<< OrderedSet

array ∷ Array Literal → Literal
array = roll <<< Array

object ∷ SM.StrMap Literal → Literal
object = roll <<< Object

-- | An information-preserving renderer
renderLiteral
  ∷ Literal
  → String
renderLiteral c =
  renderLiteralF renderLiteral $
    unroll c

-- | A more readable, but forgetful renderer
displayLiteral
  ∷ Literal
  → String
displayLiteral c =
  displayLiteralF displayLiteral $
    unroll c

encodeJsonLiteralF
  ∷ ∀ a
  . (a → JSON.Json)
  → LiteralF a
  → JSON.Json
encodeJsonLiteralF rec lit =
  case lit of
    Null → JSON.jsonNull
    Boolean b → encodeJson b
    Integer i → encodeJson i
    Decimal a → encodeJson $ HN.toNumber a
    String str → encodeJson str
    DateTime str → JSON.jsonSingletonObject "$timestamp" $ encodeJson str
    Time str → JSON.jsonSingletonObject "$time" $ encodeJson str
    Date str → JSON.jsonSingletonObject "$date" $ encodeJson str
    Interval str → JSON.jsonSingletonObject "$interval" $ encodeJson str
    ObjectId str → JSON.jsonSingletonObject "$oid" $ encodeJson str
    OrderedSet ds → JSON.jsonSingletonObject "$set" $ encodeJson $ rec <$> ds
    Array ds → encodeJson $ rec <$> ds
    Object ds →
      let obj = JSON.fromObject $ rec <$> ds in
      case F.find (\k → S.charAt 0 k == M.Just '$') (SM.keys ds) of
        M.Nothing → obj
        M.Just _ → JSON.jsonSingletonObject "$obj" obj

-- | Decode a SQL^2 Literal from "application/json;mode=precise" format
decodeJsonLiteralF
  ∷ ∀ a
  . (JSON.Json → E.Either String a)
  → JSON.Json
  → E.Either String (LiteralF a)
decodeJsonLiteralF rec =
  JSON.foldJson
    (\_ → pure Null)
    (pure <<< Boolean)
    (pure <<< decodeNumber)
    (pure <<< String)
    (map Array <<< T.traverse rec)
    decodeObject

  where
    decodeNumber
      ∷ Number
      → LiteralF a
    decodeNumber a =
      case Data.Int.fromNumber a of
        M.Just i → Integer i
        M.Nothing → Decimal $ HN.fromNumber a

    getOnlyKey
      ∷ String
      → JSON.JObject
      → E.Either String JSON.Json
    getOnlyKey key obj =
      case SM.keys obj of
        [_] → obj .? key
        keys → E.Left $ "Expected '" <> key <> "' to be the only key, but found: " <> show keys

    unwrapLeaf
      ∷ ∀ b
      . (DecodeJson b)
      ⇒ String
      → (b → LiteralF a)
      → JSON.JObject
      → E.Either String (LiteralF a)
    unwrapLeaf key con =
      getOnlyKey key
        >=> decodeJson
        >>> map con

    unwrapBranch
      ∷ ∀ t
      . (T.Traversable t, DecodeJson (t JSON.Json))
      ⇒ String
      → (t a → LiteralF a)
      → JSON.JObject
      → E.Either String (LiteralF a)
    unwrapBranch key con =
      getOnlyKey key
        >=> decodeJson
        >=> T.traverse rec
        >>> map con

    unwrapNull
      ∷ JSON.JObject
      → E.Either String (LiteralF a)
    unwrapNull =
      getOnlyKey "$na"
        >=> JSON.foldJsonNull (E.Left "Expected null") (\_ → pure Null)

    decodeObject
      ∷ JSON.JObject
      → E.Either String (LiteralF a)
    decodeObject obj =
      unwrapBranch "$obj" Object obj
        <|> unwrapBranch "$set" OrderedSet obj
        <|> unwrapLeaf "$timestamp" DateTime obj
        <|> unwrapLeaf "$date" Date obj
        <|> unwrapLeaf "$time" Time obj
        <|> unwrapLeaf "$interval" Interval obj
        <|> unwrapLeaf "$oid" ObjectId obj
        <|> unwrapNull obj
        <|> Object <$> T.traverse rec obj

decodeJsonLiteral
  ∷ JSON.Json
  → E.Either String Literal
decodeJsonLiteral json =
  roll <$> decodeJsonLiteralF decodeJsonLiteral json

encodeJsonLiteral
  ∷ Literal
  → JSON.Json
encodeJsonLiteral lit =
  encodeJsonLiteralF encodeJsonLiteral $
    unroll lit
