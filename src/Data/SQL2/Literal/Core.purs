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

module Data.SQL2.Literal.Core
  ( Literal()
  , LiteralF(..)
  , renderLiteralF
  , renderLiteral
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
  ) where

import Prelude

import Data.Eq1
import Data.Foldable as F
import Data.Functor.Mu
import Data.StrMap as SM
import Data.String as S

data LiteralF a
  = Null
  | Boolean Boolean
  | Integer Int
  | Decimal Number
  | String String
  | DateTime String -- ?
  | Date String -- ?
  | Time String -- ?
  | Interval String -- ?
  | ObjectId String
  | OrderedSet (Array a)
  | Array (Array a)
  | Object (SM.StrMap a)

instance functorLiteralF :: Functor LiteralF where
  map f c =
    case c of
      Null -> Null
      Boolean b -> Boolean b
      Integer i -> Integer i
      Decimal a -> Decimal a
      String s -> String s
      DateTime s -> DateTime s
      Date s -> Date s
      Time s -> Time s
      Interval s -> Interval s
      ObjectId s -> ObjectId s
      OrderedSet cs -> OrderedSet $ f <$> cs
      Array cs -> Array $ f <$> cs
      Object cs -> Object $ f <$> cs

instance eq1LiteralF :: Eq1 LiteralF where
  eq1 Null Null = true
  eq1 (Boolean b1) (Boolean b2) = b1 == b2
  eq1 (Integer i) (Integer j) = i == j
  eq1 (Decimal a) (Decimal b) = a == b
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

renderLiteralF
  :: forall a
   . (a -> String)
  -> LiteralF a
  -> String
renderLiteralF rec d =
  case d of
    Null -> "null"
    Boolean b -> if b then "true" else "false"
    Integer i -> show i
    Decimal a -> show a
    String str -> singleQuote str
    DateTime str -> tagged "TIMESTAMP" str
    Time str -> tagged "TIME" str
    Date str -> tagged "DATE" str
    Interval str -> tagged "INTERVAL" str
    ObjectId str -> tagged "OID" str
    OrderedSet ds -> parens $ commaSep ds
    Array ds -> squares $ commaSep ds
    Object ds -> braces $ renderMap ds
  where
    parens
      :: String
      -> String
    parens str =
      "(" <> str <> ")"

    tagged
      :: String
      -> String
      -> String
    tagged tag str =
      tag
        <> " "
        <> singleQuote str

    squares
      :: String
      -> String
    squares str =
      "[" <> str <> "]"

    braces
      :: String
      -> String
    braces str =
      "{" <> str <> "}"

    -- | Surround text in double quotes, escaping internal double quotes.
    doubleQuote
      :: String
      -> String
    doubleQuote str =
      "\"" <> S.replace "\"" "\"\"" str <> "\""

    -- | Surround text in single quotes, escaping internal quotes.
    singleQuote
      :: String
      -> String
    singleQuote str =
      "'" <> S.replace "'" "''" str <> "'"

    commaSep
      :: forall f
       . (Functor f, F.Foldable f)
      => f a
      -> String
    commaSep =
      F.intercalate "," <<<
        map rec

    renderMap
      :: SM.StrMap a
      -> String
    renderMap =
      F.intercalate ", " <<<
        SM.foldMap \k v ->
          [ doubleQuote k <> ": " <> rec v ]

instance showLiteralF :: (Show a) => Show (LiteralF a) where
  show = renderLiteralF show

type Literal = Mu LiteralF

null :: Literal
null = roll Null

boolean :: Boolean -> Literal
boolean = roll <<< Boolean

integer :: Int -> Literal
integer = roll <<< Integer

decimal :: Number -> Literal
decimal = roll <<< Decimal

string :: String -> Literal
string = roll <<< String

dateTime :: String -> Literal
dateTime = roll <<< DateTime

date :: String -> Literal
date = roll <<< Date

time :: String -> Literal
time = roll <<< Time

interval :: String -> Literal
interval = roll <<< Interval

objectId :: String -> Literal
objectId = roll <<< ObjectId

orderedSet :: Array Literal -> Literal
orderedSet = roll <<< OrderedSet

array :: Array Literal -> Literal
array = roll <<< Array

object :: SM.StrMap Literal -> Literal
object = roll <<< Object

renderLiteral
  :: Literal
  -> String
renderLiteral c =
  renderLiteralF renderLiteral $
    unroll c

