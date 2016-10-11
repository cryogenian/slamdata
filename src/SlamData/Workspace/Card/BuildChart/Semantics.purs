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

module SlamData.Workspace.Card.BuildChart.Semantics where

import SlamData.Prelude

import Data.Argonaut (runJsonPrim, toPrims, JsonPrim, Json, JArray, JCursor, foldJson, cursorGet)
import Data.Array as A
import Data.Enum (fromEnum, toEnum)
import Data.Int as Int
import Data.List (List(..), catMaybes)
import Data.List as L
import Data.Map (Map, keys, update, lookup, fromFoldable)
import Data.String (take)
import Data.String.Regex (Regex, noFlags, regex, match)
import Data.Time as Dt
import Data.Date as Dd
import Data.DateTime as Ddt

import Utils (stringToNumber)

data Semantics
  = Value Number
  | Percent Number
  | Money Number String
  | Bool Boolean
  | Category String
  | Time Dt.Time
  | Date Dd.Date
  | DateTime Ddt.DateTime

printTime ∷ Dt.Time → String
printTime time =
  let
    hour = fromEnum $ Dt.hour time
    minute = fromEnum $ Dt.minute time
    second = fromEnum $ Dt.second time
    millisecond = fromEnum $ Dt.millisecond time
  in
    show hour ⊕ ":" ⊕ show minute ⊕ ":" ⊕ show second ⊕ "." ⊕ show millisecond
-- We _can_ user purescript-formatters here, but printing semantics is used
-- heavily in axis analysis, so, simple printing used here instead of Μ stuff
printDate ∷ Dd.Date → String
printDate date =
  let
    year = fromEnum $ Dd.year date
    month = fromEnum $ Dd.month date
    day = fromEnum $ Dd.day date
  in
    show year ⊕ "-" ⊕ show month ⊕ "-" ⊕ show day

printSemantics ∷ Semantics → String
printSemantics (Value v) = show v
printSemantics (Percent v) = show v <> "%"
printSemantics (Money v m) = show v <> m
printSemantics (Category s) = s
printSemantics (Bool b) = show b
printSemantics (Time t) = printTime t
printSemantics (Date d) = printDate d
printSemantics (DateTime (Ddt.DateTime d t)) = printDate d ⊕ "T" ⊕ printTime t

semanticsToNumber ∷ Semantics → Maybe Number
semanticsToNumber (Value v) = pure v
semanticsToNumber (Money v _) = pure v
semanticsToNumber (Percent v) = pure v
semanticsToNumber (Bool b) = pure if b then one else zero
semanticsToNumber _ = Nothing

semanticsToTime ∷ Semantics → Maybe Dt.Time
semanticsToTime (Time t) = pure t
semanticsToTime _ = Nothing

semanticsToDate ∷ Semantics → Maybe Dd.Date
semanticsToDate (Date d) = pure d
semanticsToDate _ = Nothing

semanticsToDateTime ∷ Semantics → Maybe Ddt.DateTime
semanticsToDateTime (DateTime dt) = pure dt
semanticsToDateTime _ = Nothing

isUsedAsNothing ∷ Semantics → Boolean
isUsedAsNothing (Category "undefined") = true
isUsedAsNothing (Category "null") = true
isUsedAsNothing (Category "NA") = true
isUsedAsNothing (Category "N/A") = true
isUsedAsNothing (Category "") = true
isUsedAsNothing _ = false

derive instance eqSemantics ∷ Eq Semantics
derive instance ordSemantics ∷ Ord Semantics

analyze ∷ JsonPrim → Maybe Semantics
analyze p =
  runJsonPrim p
    (const Nothing)
    (Just ∘ Bool)
    (Just ∘ Value)
    analyzeString

analyzeJson ∷ Json → Maybe Semantics
analyzeJson =
  foldJson
    (const Nothing)
    (pure ∘ Bool)
    (pure ∘ Value)
    analyzeString
    (const Nothing)
    (const Nothing)

analyzeString ∷ String → Maybe Semantics
analyzeString str =
  analyzeTime str
  <|> analyzeDatetime str
  <|> analyzeDate str
  <|> analyzeNumber str
  <|> analyzeMoney str
  <|> analyzePercent str
  <|> (Just $ Category str)

analyzeNumber ∷ String → Maybe Semantics
analyzeNumber s = do
  num ← stringToNumber s
  int ← Int.fromString s
  guard $ show num == s || show int == s
  pure $ Value num

percentRegex ∷ Regex
percentRegex = unsafePartial fromRight $ regex """^(-?\d+(\.\d+)?)\%$""" noFlags

analyzePercent ∷ String → Maybe Semantics
analyzePercent input = do
  matches ← match percentRegex input
  maybeMatch ← matches A.!! 1
  num ← maybeMatch >>= stringToNumber
  pure $ Percent num

moneyRegex ∷ Regex
moneyRegex = unsafePartial fromRight $ regex rgxStr noFlags
  where
  rgxStr = "^" <> curSymbols <> """?(([0-9]{1,3},([0-9]{3},)*[0-9]{3}|[0-9]+)(.[0-9][0-9])?)$"""
  curSymbols ∷ String
  curSymbols = """[\$\u20A0-\u20CF\u00A2\u00A3\u00A4\u00A5\u058F\u060B\u09F2\u09F3\u09FB\u0AF1\u0BF9\u0E3F\u17DB\uA838\uFDFC\uFE69\uFF04\uFFE0\uFFE1\uFFE5\uFFE6]"""

analyzeMoney ∷ String → Maybe Semantics
analyzeMoney str = do
  matches ← match moneyRegex str
  maybeMatch ← matches A.!! 1
  num ← maybeMatch >>= stringToNumber
  currencySymbol ←
    let fstSymbol = take 1 str
    in if fstSymbol == ""
       then Nothing
       else pure fstSymbol
  pure $ Money num currencySymbol

datetimeRegex ∷ Regex
datetimeRegex = unsafePartial fromRight $ regex rgxStr noFlags
  where
  rgxStr = "^(-?(?:[1-9][0-9]*)?[0-9]{4})-(1[0-2]|0[1-9])-(3[0-1]|0[1-9]|[1-2][0-9])(T| )(2[0-3]|[0-1][0-9]):([0-5][0-9]):([0-5][0-9])(\\.[0-9]+)?(Z|[+-](?:2[0-3]|[0-1][0-9]):[0-5][0-9])?$"

timeRegex ∷ Regex
timeRegex = unsafePartial fromRight $ regex rgxStr noFlags
  where
  rgxStr = "^(2[0-3]|[0-1][0-9]):([0-5][0-9]):([0-5][0-9])?(\\.[0-9]+)?(Z|[+-](?:2[0-3]|[0-1][0-9]):[0-5][0-9])?$"

dateRegex ∷ Regex
dateRegex = unsafePartial fromRight $ regex rgxStr noFlags
  where
  rgxStr = "^(-?(?:[1-9][0-9]*)?[0-9]{4})-(1[0-2]|0[1-9])-(3[0-1]|0[1-9]|[1-2][0-9])$"

analyzeDate ∷ String → Maybe Semantics
analyzeDate str = do
  matches ← match dateRegex str
  year ← (join $ matches A.!! 1) >>= Int.fromString >>= toEnum
  month ← (join $ matches A.!! 2) >>= Int.fromString >>= toEnum
  day ← (join $ matches A.!! 3) >>= Int.fromString >>= toEnum
  date ← Dd.exactDate year month day
  pure $ Date date

analyzeTime ∷ String → Maybe Semantics
analyzeTime str = do
  matches ← match timeRegex str
  hour ← (join $ matches A.!! 1) >>= Int.fromString >>= toEnum
  minute ← (join $ matches A.!! 2) >>= Int.fromString >>= toEnum
  let
    second = fromMaybe bottom $ (join $ matches A.!! 3) >>= Int.fromString >>= toEnum
    millisecond = fromMaybe bottom $ (join $ matches A.!! 4) >>= Int.fromString >>= toEnum
  pure $ Time $ Dt.Time hour minute second millisecond

analyzeDatetime ∷ String → Maybe Semantics
analyzeDatetime str = do
  matches ← match datetimeRegex str
  year ← (join $ matches A.!! 1) >>= Int.fromString >>= toEnum
  month ← (join $ matches A.!! 2) >>= Int.fromString >>= toEnum
  day ← (join $ matches A.!! 3) >>= Int.fromString >>= toEnum
  hour ← (join $ matches A.!! 5) >>= Int.fromString >>= toEnum
  minute ← (join $ matches A.!! 6) >>= Int.fromString >>= toEnum
  let
    second = fromMaybe bottom $ (join $ matches A.!! 7) >>= Int.fromString >>= toEnum
    millisecond = fromMaybe bottom $  (join $ matches A.!! 8) >>= Int.fromString >>= toEnum
    time = Dt.Time hour minute second millisecond

  date ← Dd.exactDate year month day
  pure
    $ DateTime
    $ Ddt.DateTime date time

jsonToSemantics ∷ Json → Map JCursor Semantics
jsonToSemantics j =
  fromFoldable $ catMaybes $ map (traverse analyze) $ toPrims j

jarrayToSemantics ∷ JArray → Map JCursor (List (Maybe Semantics))
jarrayToSemantics arr = foldl foldFn initial mapArr
  where
  mapArr ∷ Array (Map JCursor Semantics)
  mapArr = map jsonToSemantics arr

  initial ∷ Map JCursor (List (Maybe Semantics))
  initial = fromFoldable $ map (flip Tuple Nil) ks

  ks ∷ List JCursor
  ks = L.nub $ foldMap keys mapArr

  foldFn
    ∷ Map JCursor (List (Maybe Semantics))
    → Map JCursor Semantics
    → Map JCursor (List (Maybe Semantics))
  foldFn acc m = foldl (insertOne m) acc ks

  insertOne
    ∷ Map JCursor Semantics
    → Map JCursor (List (Maybe Semantics))
    → JCursor
    → Map JCursor (List (Maybe Semantics))
  insertOne m acc k = update (pure ∘ Cons (lookup k m)) k acc

getMaybeString ∷ Json → JCursor → Maybe String
getMaybeString js cursor =
  map printSemantics
    $ analyzeJson
    =<< cursorGet cursor js

getValues ∷ Json → Maybe JCursor → Array Number
getValues js cursor =
  foldMap A.singleton
    $ semanticsToNumber
    =<< analyzeJson
    =<< flip cursorGet js
    =<< cursor
