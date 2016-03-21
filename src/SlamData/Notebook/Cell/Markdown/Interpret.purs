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

module SlamData.Notebook.Cell.Markdown.Interpret
  ( formFieldValueToLiteral
  ) where

import SlamData.Prelude

import Data.List as L
import Data.Set as Set
import Data.SQL2.Literal as SQL2

import Text.Markdown.SlamDown as MD
import Text.Markdown.SlamDown.Html as MD
import Text.Parsing.Parser as P

formFieldValueToLiteral
  :: MD.FormFieldValue
  -> SQL2.Literal
formFieldValueToLiteral v =
  case v of
    MD.SingleValue ty str ->
      case ty of
        MD.PlainText -> SQL2.string str
        MD.Numeric ->
          P.runParser str SQL2.parseLiteral
            # either
                (\_ -> SQL2.string str) -- input validation precludes this case
                id
        MD.Date -> SQL2.date str
        MD.Time -> SQL2.time str
        MD.DateTime -> SQL2.dateTime str
    MD.MultipleValues vs ->
      SQL2.orderedSet $
        SQL2.string <$>
          setToArray vs
  where
    setToArray
      :: forall a
       . Set.Set a
      -> Array a
    setToArray =
      Set.toList
        >>> L.fromList
