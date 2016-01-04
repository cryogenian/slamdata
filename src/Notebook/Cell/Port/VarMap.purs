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

module Notebook.Cell.Port.VarMap
  ( VarMap()
  , VarMapValue(..)
  , renderVarMapValue
  , parseVarMapValue
  ) where

import Prelude
import Control.Alt ((<|>))
import Data.Array as A
import Data.StrMap as SM
import Data.SQL2.Literal as SQL2
import Data.String as S
import Data.StrMap as SM

import Text.Parsing.Parser as P
import Text.Parsing.Parser.String as PS
import Text.Parsing.Parser.Combinators as PC

data VarMapValue
  = Literal SQL2.Literal
  | QueryExpr String -- TODO: syntax of SQL^2 queries

instance eqVarMapValue :: Eq VarMapValue where
  eq (Literal c) (Literal d) = c == d
  eq (QueryExpr q) (QueryExpr q') = q == q'
  eq _ _ = false

renderVarMapValue
  :: VarMapValue
  -> String
renderVarMapValue val =
  case val of
    Literal c -> SQL2.renderLiteral c
    QueryExpr str -> str

parseVarMapValue
  :: forall m
   . (Monad m)
  => P.ParserT String m VarMapValue
parseVarMapValue =
  Literal <$> PC.try SQL2.parseLiteral
    <|> QueryExpr <$> anyString
  where
    anyString =
      A.many PS.anyChar
        <#> S.fromCharArray

type VarMap = SM.StrMap VarMapValue
