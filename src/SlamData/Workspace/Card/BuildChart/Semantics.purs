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

import Control.Monad.Eff (Eff)
import Control.Monad.ST (STRef, ST, newSTRef, modifySTRef, readSTRef, pureST)

import Data.Argonaut (runJsonPrim, toPrims, JsonPrim, Json, JArray, JCursor, class DecodeJson, class EncodeJson, decodeJson, jsonEmptyObject, (:=), (.?), (~>), foldJson)
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

isValue ∷ Semantics → Boolean
isValue (Value _) = true
isValue _ = false

isPercent ∷ Semantics → Boolean
isPercent (Percent _) = true
isPercent _ = false

isMoney ∷ Semantics → Boolean
isMoney (Money _ _) = true
isMoney _ = false

isBool ∷ Semantics → Boolean
isBool (Bool _) = true
isBool _ = false

isCategory ∷ Semantics → Boolean
isCategory (Category _) = true
isCategory _ = false

isTime ∷ Semantics → Boolean
isTime (Time _) = true
isTime _ = false

isDate ∷ Semantics → Boolean
isDate (Date _) = true
isDate _ = false

isDateTime ∷ Semantics → Boolean
isDateTime (DateTime _) = true
isDateTime _ = false

printTime ∷ Dt.Time → String
printTime time =
  let
    hour = fromEnum $ Dt.hour time
    minute = fromEnum $ Dt.minute time
    second = fromEnum $ Dt.second time
    millisecond = fromEnum $ Dt.millisecond time
  in
    show hour ⊕ ":" ⊕ show minute ⊕ ":" ⊕ show second ⊕ "." ⊕ show millisecond

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
printSemantics (DateTime (Ddt.DateTime d t)) =
  printDate d ⊕ "T" ⊕ printTime t


semanticsToNumber ∷ Semantics → Maybe Number
semanticsToNumber (Value v) = pure v
semanticsToNumber (Money v _) = pure v
semanticsToNumber (Percent v) = pure v
semanticsToNumber _ = Nothing



-- | Used as accumulator in `checkPredicate` only
type CheckAccum =
  { correct ∷ Int
  , incorrect ∷ Int
  , filtered ∷ List (Maybe Semantics)
  }

emptyAccum ∷ CheckAccum
emptyAccum =
  { correct: zero
  , incorrect: zero
  , filtered: Nil
  }

-- | This function checks values in list of `Maybe Semantics` and
-- | returns a list with values that satisfies predicate wrapped in `Just`
-- | or (if there more incorrect values than correct) it returns `Nothing`
checkPredicate
  ∷ (Semantics → Boolean) → List (Maybe Semantics)
  → Maybe (List (Maybe Semantics))
checkPredicate p lst = pureST do
  corrects ← newSTRef 0
  incorrects ← newSTRef 0
  filtered ← newSTRef Nil
  -- `traverse_` uses `foldr`. `List`s `foldr` isn't tail recursive.
  -- To make it tail recursive we probably should reimplement `foldr` like
  -- `foldr f init lst = reverse $ foldl (flip f) init $ reverse lst`
  -- and this make our optimization absolutely useless because of two
  -- `reverse`s. And this is worse then converting list to array before `traverse_`
  -- ```
  --  let arr ∷ Array _
  --      arr = L.fromFoldable lst
  --  in traverse_ (checkPredicateTraverseFn p corrects incorrects filtered) arr
  -- ```
  foldl
    (\b a → checkPredicateTraverseFn p corrects incorrects filtered a *> b)
    (pure unit) lst
  c ← readSTRef corrects
  ic ← readSTRef incorrects
  if c > ic
    then do
    f ← readSTRef filtered
    pure $ Just f
    else
    pure Nothing

checkPredicateTraverseFn
  ∷ forall h
   . (Semantics → Boolean)
  → STRef h Int → STRef h Int → STRef h (List (Maybe Semantics))
  → Maybe Semantics → Eff (st ∷ ST h) Unit
checkPredicateTraverseFn _ corrects incorrects filtered Nothing = do
  modifySTRef filtered (Cons Nothing)
  pure unit
checkPredicateTraverseFn p corrects incorrects filtered (Just c)
  | isUsedAsNothing c = do
    modifySTRef filtered $ Cons Nothing
    pure unit

  | p c = do
    modifySTRef filtered $ Cons $ Just c
    modifySTRef corrects (_ + 1)
    pure unit

  | otherwise = do
    modifySTRef filtered $ Cons Nothing
    modifySTRef incorrects (_ + 1)
    pure unit

isUsedAsNothing ∷ Semantics → Boolean
isUsedAsNothing (Category "undefined") = true
isUsedAsNothing (Category "null") = true
isUsedAsNothing (Category "NA") = true
isUsedAsNothing (Category "N/A") = true
isUsedAsNothing (Category "") = true
isUsedAsNothing _ = false

instance eqSemantics ∷ Eq Semantics where
  eq (Value v) (Value v') = v == v'
  eq (Value v) _ = false
  eq (Percent p) (Percent p') = p == p'
  eq (Percent _) _ = false
  eq (Money m c) (Money m' c') = m == m' && c == c'
  eq (Money _ _) _ = false
  eq (Time t) (Time t') = t == t'
  eq (Time _) _ = false
  eq (Date d) (Date d') = d == d'
  eq (Date _) _ = false
  eq (DateTime dt) (DateTime dt') = dt == dt'
  eq (DateTime dt) _ = false
  eq (Category c) (Category c') = c == c'
  eq (Category _) _ = false
  eq (Bool b) (Bool b') = b == b'
  eq (Bool _) _ = false

instance ordSemantics ∷ Ord Semantics where
  compare (Time t) (Time t') = compare t t'
  compare (Time _) _ = LT
  compare (Date d) (Date d') = compare d d'
  compare (Date d) _ = LT
  compare (DateTime dt) (DateTime dt') = compare dt dt'
  compare (DateTime dt) _ = LT
  compare (Money v a) (Money v' a') =
    case compare a a' of
      EQ → compare v v'
      c → c
  compare (Money _ _) _ = LT
  compare (Percent v) (Percent v') = compare v v'
  compare (Percent _) _ = LT
  compare (Value v) (Value v') = compare v v'
  compare (Value _) _ = LT
  compare (Category c) (Category c') = compare c c'
  compare (Category _) _ = LT
  compare (Bool b) (Bool b') = compare b b'
  compare (Bool _) _ = LT


analyze ∷ JsonPrim → Maybe Semantics
analyze p = runJsonPrim p
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
  rgxStr = "^(2[0-3]|[0-1][0-9]):([0-5][0-9])(\\.[0-9]+)?(Z|[+-](?:2[0-3]|[0-1][0-9]):[0-5][0-9])?$"

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
jsonToSemantics j = fromFoldable $ catMaybes $ map (traverse analyze) $ toPrims j

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

checkValues ∷ List (Maybe Semantics) → Maybe (List (Maybe Semantics))
checkValues = checkPredicate isValue

checkMoney ∷ List (Maybe Semantics) → Maybe (List (Maybe Semantics))
checkMoney = checkPredicate isMoney

checkPercent ∷ List (Maybe Semantics) → Maybe (List (Maybe Semantics))
checkPercent = checkPredicate isPercent

checkBool ∷ List (Maybe Semantics) → Maybe (List (Maybe Semantics))
checkBool = checkPredicate isBool

checkTime ∷ List (Maybe Semantics) → Maybe (List (Maybe Semantics))
checkTime = checkPredicate isTime

checkCategory ∷ List (Maybe Semantics) → Maybe (List (Maybe Semantics))
checkCategory = checkPredicate isCategory

instance encodeJsonSemantics ∷ EncodeJson Semantics where
  encodeJson (Value n)
     = "type" := "value"
    ~> "value" := n
    ~> jsonEmptyObject
  encodeJson (Percent p)
     = "type" := "percent"
     ~> "value" := p
     ~> jsonEmptyObject
  encodeJson (Money n v)
     = "type" := "money"
     ~> "currency" := v
     ~> "value" := n ~> jsonEmptyObject
  encodeJson (Bool b)
     = "type" := "bool"
     ~> "value" := b
     ~> jsonEmptyObject
  encodeJson (Category c)
     = "type" := "category"
     ~> "value" := c
     ~> jsonEmptyObject
  encodeJson s@(Time t)
     = "type" := "time"
     ~> "value" := printSemantics s
     ~> jsonEmptyObject
  encodeJson s@(Date d)
     = "type" := "date"
     ~> "value" := printSemantics s
     ~> jsonEmptyObject
  encodeJson s@(DateTime dt)
     = "type" := "datetime"
     ~> "value" := printSemantics s
     ~> jsonEmptyObject

instance decodeJsonSemantics ∷ DecodeJson Semantics where
  decodeJson = decodeJson >=> \obj → do
    ty ← obj .? "type"
    case ty of
      "money" → Money <$> obj .? "value" <*> obj .? "currency"
      "bool" → Bool <$> obj .? "value"
      "category" → Category <$> obj .? "value"
      "value" → Value <$> obj .? "value"
      "percent" → Percent <$> obj .? "value"
      "time" → do
        strVal ← obj .? "value"
        maybe (Left "incorrect time") pure $ analyzeTime strVal
      "date" → do
        strVal ← obj .? "value"
        maybe (Left "incorrect date") pure $ analyzeDate strVal
      "datetime" → do
        strVal ← obj .? "value"
        maybe (Left "incorrect datetime") pure $ analyzeDatetime strVal
      _ → Left "incorrect type value for Semantics"
