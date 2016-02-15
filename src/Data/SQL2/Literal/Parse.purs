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

module Data.SQL2.Literal.Parse
  ( parseLiteralF
  , parseLiteral
  ) where

import Prelude

import Control.Alt ((<|>))
import Control.Apply ((*>), (<*))
import Control.Lazy as Lazy
import Data.Functor ((<$), ($>))

import Data.Array as A
import Data.Foldable as F
import Data.Int as Int
import Data.List as L
import Data.String as S
import Data.StrMap as SM
import Data.Tuple as T

import Data.Functor.Mu
import Data.SQL2.Literal.Core

import Text.Parsing.Parser as P
import Text.Parsing.Parser.Combinators as PC
import Text.Parsing.Parser.String as PS

parens
  :: forall m a
   . (Monad m)
  => P.ParserT String m a
  -> P.ParserT String m a
parens =
  PC.between
    (PS.string "(")
    (PS.string ")")

squares
  :: forall m a
   . (Monad m)
  => P.ParserT String m a
  -> P.ParserT String m a
squares =
  PC.between
    (PS.string "[")
    (PS.string "]")

braces
  :: forall m a
   . (Monad m)
  => P.ParserT String m a
  -> P.ParserT String m a
braces =
  PC.between
    (PS.string "{")
    (PS.string "}")

commaSep
  :: forall m a
   . (Monad m)
  => P.ParserT String m a
  -> P.ParserT String m (L.List a)
commaSep =
  flip PC.sepBy $
    PS.skipSpaces
      *> PS.string ","
      <* PS.skipSpaces

stringLiteral
  :: forall m
   . (Monad m)
  => P.ParserT String m String
stringLiteral =
  PC.between quote quote (A.many stringChar)
    <#> S.fromCharArray

  where
    quote = PS.string "\""

    stringChar =
      PC.try stringEscape
        <|> stringLetter

    stringLetter =
      PS.satisfy \c ->
        c /= '"'

    stringEscape =
      PS.string "\\\"" $> '"'

taggedLiteral
  :: forall m
   . (Monad m)
  => String
  -> P.ParserT String m String
taggedLiteral tag =
  PC.try $
    PS.string tag
      *> parens stringLiteral

anyString
  :: forall m
   . (Monad m)
  => P.ParserT String m String
anyString =
  A.many PS.anyChar
    <#> S.fromCharArray

parseBoolean
  :: forall m
   . (Monad m)
  => P.ParserT String m Boolean
parseBoolean =
  PC.choice
    [ true <$ PS.string "true"
    , false <$ PS.string "false"
    ]


parseDigit
  :: forall m
   . (Monad m)
  => P.ParserT String m Int
parseDigit =
  PC.choice
    [ 0 <$ PS.string "0"
    , 1 <$ PS.string "1"
    , 2 <$ PS.string "2"
    , 3 <$ PS.string "3"
    , 4 <$ PS.string "4"
    , 5 <$ PS.string "5"
    , 6 <$ PS.string "6"
    , 7 <$ PS.string "7"
    , 8 <$ PS.string "8"
    , 9 <$ PS.string "9"
    ]


many1
  :: forall m s a
   . (Monad m)
  => P.ParserT s m a
  -> P.ParserT s m (L.List a)
many1 p =
  L.Cons
    <$> p
    <*> L.many p

parseNat
  :: forall m
   . (Monad m)
  => P.ParserT String m Int
parseNat =
  many1 parseDigit
    <#> F.foldl (\a i -> a * 10 + i) 0

parseNegative
  :: forall m a
   . (Monad m, Ring a)
  => P.ParserT String m a
  -> P.ParserT String m a
parseNegative p =
  PS.string "-"
    *> PS.skipSpaces
    *> p
    <#> negate

parsePositive
  :: forall m a
   . (Monad m, Ring a)
  => P.ParserT String m a
  -> P.ParserT String m a
parsePositive p =
  PC.optional (PS.string "+" *> PS.skipSpaces)
    *> p

parseSigned
  :: forall m a
   . (Monad m, Ring a)
  => P.ParserT String m a
  -> P.ParserT String m a
parseSigned p =
  parseNegative p
    <|> parsePositive p

parseInt
  :: forall m
   . (Monad m)
  => P.ParserT String m Int
parseInt =
  parseSigned parseNat

parseExponent
  :: forall m
   . (Monad m)
  => P.ParserT String m Number
parseExponent =
  (PS.string "e" <|> PS.string "E")
    *> parseInt
    <#> Int.toNumber

parsePositiveDecimal
  :: forall m
   . (Monad m)
  => P.ParserT String m Number
parsePositiveDecimal = do
  lhs <- PC.try $ Int.toNumber <$> parseNat <* PS.string "."
  rhs <- A.many parseDigit <#> F.foldr (\d f -> (f + Int.toNumber d) / 10.0) 0.0
  exp <- PC.option 0.0 parseExponent
  pure $ (lhs + rhs) * Math.pow 10.0 exp

parseDecimal
  :: forall m
   . (Monad m)
  => P.ParserT String m Number
parseDecimal =
  parseSigned parsePositiveDecimal

-- | Parse one layer of structure.
parseLiteralF
  :: forall m a
   . (Monad m)
  => P.ParserT String m a
  -> P.ParserT String m (LiteralF a)
parseLiteralF rec =
  PC.choice $
    [ Null <$ PS.string "null"
    , Boolean <$> parseBoolean
    , Decimal <$> parseDecimal
    , Integer <$> parseInt
    , String <$> stringLiteral
    , DateTime <$> taggedLiteral "TIMESTAMP"
    , Time <$> taggedLiteral "TIME"
    , Date <$> taggedLiteral "DATE"
    , Interval <$> taggedLiteral "INTERVAL"
    , ObjectId <$> taggedLiteral "OID"
    , OrderedSet <<< L.fromList <$> parens (commaSep rec)
    , Array <<< L.fromList <$> squares (commaSep rec)
    , Object <<< SM.fromList <$> braces (commaSep parseAssignment)
    ]

  where
    parseColon :: P.ParserT String m String
    parseColon =
      PS.skipSpaces
        *> PS.string ":"
        <* PS.skipSpaces

    parseAssignment :: P.ParserT String m (T.Tuple String a)
    parseAssignment =
      T.Tuple
        <$> stringLiteral <* parseColon
        <*> rec

-- | A closed parser of SQL^2 constant expressions
parseLiteral
  :: forall m
   . (Monad m)
  => P.ParserT String m Literal
parseLiteral =
  Lazy.fix \f ->
    roll <$>
      parseLiteralF f
