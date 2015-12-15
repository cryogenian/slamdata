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

module Notebook.Cell.Query.Eval
  ( queryEval
  ) where

import Prelude
import Control.Alt ((<|>))
import Control.Apply ((*>))
import Control.Monad.Error.Class as EC
import Control.Monad.Trans as MT
import Control.Monad.Writer.Class as WC

import Data.Either as E
import Data.Foldable as F
import Data.Lens as L
import Data.Maybe as M
import Data.Set as Set
import Data.String.Regex as Rx
import Data.StrMap as SM

import Model.Resource as R
import Model.Port as Port
import Notebook.Cell.Common.EvalQuery as CEQ
import Notebook.Common (Slam())

import Text.Markdown.SlamDown as SD
import Text.Markdown.SlamDown.Html as SD
import Text.Parsing.StringParser as SP
import Text.Parsing.StringParser.Combinators as SP
import Text.Parsing.StringParser.String as SP

import Quasar.Aff as Quasar

queryEval :: CEQ.CellEvalInput -> String -> Slam CEQ.CellEvalResult
queryEval info sql =
  CEQ.runCellEvalT $ do
    let
      varMap = info.inputPort >>= L.preview Port._VarMap # M.fromMaybe SM.empty
      tempOutputResource = CEQ.temporaryOutputResource info
      inputResource = R.parent tempOutputResource -- TODO: make sure that this is actually still correct

    { plan: plan, outputResource: outputResource } <-
      Quasar.executeQuery sql (M.fromMaybe false info.cachingEnabled) (renderFormFieldValue <$> varMap) inputResource tempOutputResource
        # MT.lift
        >>= E.either EC.throwError pure

    F.for_ plan \p ->
      WC.tell ["Plan: " <> p]

    pure $ Port.Resource outputResource

renderFormFieldValue :: SD.FormFieldValue -> String
renderFormFieldValue formFieldValue =
  case formFieldValue of
    SD.SingleValue ty s ->
      case ty of
        SD.PlainText -> quoteString s
        SD.Numeric
          | isSQLNum s -> s
          | otherwise -> quoteString s
        SD.Date -> "DATE '" ++ s ++ "'"
        SD.Time -> "TIME '" ++ s ++ "'"
        SD.DateTime -> "TIMESTAMP '" ++ s ++ ":00Z'"
    SD.MultipleValues s ->
      "(" <> F.intercalate ", " (quoteString <$> Set.toList s) <> ")"
      -- TODO: is this anything like we want for multi-values?

  where
    quoteString :: String -> String
    quoteString s = "'" ++ Rx.replace rxQuot "''" s ++ "'"
      where
        rxQuot :: Rx.Regex
        rxQuot = Rx.regex "'" Rx.noFlags { global = true }

    isSQLNum :: String -> Boolean
    isSQLNum s =
      E.isRight $ flip SP.runParser s do
        SP.many1 SP.anyDigit
        SP.optional $ SP.string "." *> SP.many SP.anyDigit
        SP.optional $
          (SP.string "e" <|> SP.string "E")
            *> (SP.string "-" <|> SP.string "+")
            *> SP.many SP.anyDigit
        SP.eof
