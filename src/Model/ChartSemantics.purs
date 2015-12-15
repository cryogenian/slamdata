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

module Model.ChartSemantics where

import Prelude

import Control.Alt ((<|>))
import Control.Apply ((*>))
import Control.Monad.Eff (Eff())
import Control.Monad.ST (STRef(), ST(), newSTRef, modifySTRef, readSTRef, pureST)
import Control.MonadPlus (guard)
import Data.Argonaut
  ( runJsonPrim, toPrims, JsonPrim(), Json(), JArray(), JCursor()
  , DecodeJson, EncodeJson, decodeJson, jsonEmptyObject, (:=), (.?), (~>))
import Data.Array as A
import Data.Array.ST
import Data.Either (Either(..))
import Data.Foldable (foldl, foldMap)
import Data.List (List(..), reverse, catMaybes, toList)
import Data.List as L
import Data.Map (Map(), keys, update, lookup, fromList)
import Data.Maybe (Maybe(..), isJust)
import Data.String (take)
import Data.String.Regex (Regex(), noFlags, regex, match, test)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Utils (stringToNumber, stringToInt)

data Semantics
  = Value Number
  | Percent Number
  | Money Number String
  | Bool Boolean
  | Category String
  | Time String

isValue :: Semantics -> Boolean
isValue (Value _) = true
isValue _ = false

isPercent :: Semantics -> Boolean
isPercent (Percent _) = true
isPercent _ = false

isMoney :: Semantics -> Boolean
isMoney (Money _ _) = true
isMoney _ = false

isBool :: Semantics -> Boolean
isBool (Bool _) = true
isBool _ = false

isCategory :: Semantics -> Boolean
isCategory (Category _) = true
isCategory _ = false

isTime :: Semantics -> Boolean
isTime (Time _) = true
isTime _ = false

printSemantics :: Semantics -> String
printSemantics (Value v) = show v
printSemantics (Percent v) = show v <> "%"
printSemantics (Money v m) = show v <> m
printSemantics (Category s) = s
printSemantics (Time t) = t
printSemantics (Bool b) = show b

semanticsToNumber :: Semantics -> Maybe Number
semanticsToNumber (Value v) = pure v
semanticsToNumber (Money v _) = pure v
semanticsToNumber (Percent v) = pure v
semanticsToNumber _ = Nothing



-- | Used as accumulator in `checkPredicate` only
type CheckAccum =
  { correct :: Int
  , incorrect :: Int
  , filtered :: List (Maybe Semantics)
  }

emptyAccum :: CheckAccum
emptyAccum =
  { correct: zero
  , incorrect: zero
  , filtered: Nil
  }

-- | This function checks values in list of `Maybe Semantics` and
-- | returns a list with values that satisfies predicate wrapped in `Just`
-- | or (if there more incorrect values than correct) it returns `Nothing`
checkPredicate
  :: (Semantics -> Boolean) -> List (Maybe Semantics)
  -> Maybe (List (Maybe Semantics))
checkPredicate p lst = pureST do
  corrects <- newSTRef 0
  incorrects <- newSTRef 0
  filtered <- newSTRef Nil
  foldl
    (\_ a -> checkPredicateTraverseFn p corrects incorrects filtered a)
    (pure unit) lst
  c <- readSTRef corrects
  ic <- readSTRef incorrects
  if c > ic
    then do
    f <- readSTRef filtered
    pure $ Just f
    else
    pure Nothing

checkPredicateTraverseFn
  :: forall h
   . (Semantics -> Boolean)
  -> STRef h Int -> STRef h Int -> STRef h (List (Maybe Semantics))
  -> Maybe Semantics -> Eff (st :: ST h) Unit
checkPredicateTraverseFn _ corrects incorrects filtered Nothing = do
  modifySTRef filtered (Cons Nothing)
  pure unit
checkPredicateTraverseFn p corrects incorrects filtered (Just c)
  | isUsedAsNothing c = do
    modifySTRef filtered $ Cons Nothing
    pure unit

  | p c = do
    modifySTRef filtered $ Cons $ Just c
    modifySTRef corrects (+ 1)
    pure unit

  | otherwise = do
    modifySTRef filtered $ Cons Nothing
    modifySTRef incorrects (+ 1)
    pure unit

isUsedAsNothing :: Semantics -> Boolean
isUsedAsNothing (Category "undefined") = true
isUsedAsNothing (Category "null") = true
isUsedAsNothing (Category "NA") = true
isUsedAsNothing (Category "N/A") = true
isUsedAsNothing (Category "") = true
isUsedAsNothing _ = false

instance semanthicShow :: Show Semantics where
  show (Value v) = "(Value " <> show v <> ")"
  show (Percent p) = "(Percent " <> show p <> ")"
  show (Money n s) = "(Money " <> show n <> s <> ")"
  show (Bool b) = "(Bool " <> show b <> ")"
  show (Category s) = "(Category " <> s <> ")"
  show (Time t) = "(Time " <> t <> ")"

instance semanthicEq :: Eq Semantics where
  eq (Value v) (Value v') = v == v'
  eq (Value v) _ = false
  eq (Percent p) (Percent p') = p == p'
  eq (Percent _) _ = false
  eq (Money m c) (Money m' c') = m == m' && c == c'
  eq (Money _ _) _ = false
  eq (Time t) (Time t') = t == t'
  eq (Time _) _ = false
  eq (Category c) (Category c') = c == c'
  eq (Category _) _ = false
  eq (Bool b) (Bool b') = b == b'
  eq (Bool _) _ = false

instance semanthicOrd :: Ord Semantics where
  compare (Time t) (Time t') = compare t t'
  compare (Time _) _ = LT
  compare (Money v a) (Money v' a') =
    if curComp == EQ
    then compare v v'
    else curComp
    where
    curComp = compare a a'
  compare (Money _ _) _ = LT
  compare (Percent v) (Percent v') = compare v v'
  compare (Percent _) _ = LT
  compare (Value v) (Value v') = compare v v'
  compare (Value _) _ = LT
  compare (Category c) (Category c') = compare c c'
  compare (Category _) _ = LT
  compare (Bool b) (Bool b') = compare b b'
  compare (Bool _) _ = LT


analyze :: JsonPrim -> Maybe Semantics
analyze p = runJsonPrim p
            (const Nothing)
            (Just <<< Bool)
            (Just <<< Value)
            analyzeString

analyzeString :: String -> Maybe Semantics
analyzeString str =
      analyzeDate str
  <|> analyzeNumber str
  <|> analyzeMoney str
  <|> analyzePercent str
  <|> (Just $ Category str)

analyzeNumber :: String -> Maybe Semantics
analyzeNumber s = do
  num <- stringToNumber s
  int <- stringToInt s
  guard $ show num == s || show int == s
  pure $ Value num

percentRegex :: Regex
percentRegex = regex """^(-?\d+(\.\d+)?)\%$""" noFlags

analyzePercent :: String -> Maybe Semantics
analyzePercent input = do
  matches <- match percentRegex input
  maybeMatch <- matches A.!! 1
  num <- maybeMatch >>= stringToNumber
  pure $ Percent num

moneyRegex :: Regex
moneyRegex = regex rgxStr noFlags
  where
  rgxStr = "^" <> curSymbols <> """?(([0-9]{1,3},([0-9]{3},)*[0-9]{3}|[0-9]+)(.[0-9][0-9])?)$"""
  curSymbols :: String
  curSymbols = """[\$\u20A0-\u20CF\u00A2\u00A3\u00A4\u00A5\u058F\u060B\u09F2\u09F3\u09FB\u0AF1\u0BF9\u0E3F\u17DB\uA838\uFDFC\uFE69\uFF04\uFFE0\uFFE1\uFFE5\uFFE6]"""

analyzeMoney :: String -> Maybe Semantics
analyzeMoney str = do
  matches <- match moneyRegex str
  maybeMatch <- matches A.!! 1
  num <- maybeMatch >>= stringToNumber
  currencySymbol <- let fstSymbol = take 1 str
                    in if fstSymbol == ""
                       then Nothing
                       else pure fstSymbol
  pure $ Money num currencySymbol

dateRegex :: Regex
dateRegex = regex rgxStr noFlags
  where
  rgxStr = "^(-?(?:[1-9][0-9]*)?[0-9]{4})-(1[0-2]|0[1-9])-(3[0-1]|0[1-9]|[1-2][0-9]) (2[0-3]|[0-1][0-9]):([0-5][0-9]):([0-5][0-9])(\\.[0-9]+)?(Z|[+-](?:2[0-3]|[0-1][0-9]):[0-5][0-9])?$"

analyzeDate :: String -> Maybe Semantics
analyzeDate str = do
  guard $ test dateRegex str
  pure $ Time str

jsonToSemantics :: Json -> Map JCursor Semantics
jsonToSemantics j = fromList $ catMaybes $ map (traverse analyze) $ toPrims j

jarrayToSemantics :: JArray -> Map JCursor (List (Maybe Semantics))
jarrayToSemantics arr = foldl foldFn initial mapArr
  where
  mapArr :: Array (Map JCursor Semantics)
  mapArr = map jsonToSemantics arr

  initial :: Map JCursor (List (Maybe Semantics))
  initial = fromList $ map (flip Tuple Nil) ks

  ks :: List JCursor
  ks = L.nub $ foldMap keys mapArr

  foldFn
    :: Map JCursor (List (Maybe Semantics))
    -> Map JCursor Semantics
    -> Map JCursor (List (Maybe Semantics))
  foldFn acc m = foldl (insertOne m) acc ks

  insertOne
    :: Map JCursor Semantics
    -> Map JCursor (List (Maybe Semantics))
    -> JCursor
    -> Map JCursor (List (Maybe Semantics))
  insertOne m acc k = update (pure <<< Cons (lookup k m)) k acc

checkValues :: List (Maybe Semantics) -> Maybe (List (Maybe Semantics))
checkValues = checkPredicate isValue

checkMoney :: List (Maybe Semantics) -> Maybe (List (Maybe Semantics))
checkMoney = checkPredicate isMoney

checkPercent :: List (Maybe Semantics) -> Maybe (List (Maybe Semantics))
checkPercent = checkPredicate isPercent

checkBool :: List (Maybe Semantics) -> Maybe (List (Maybe Semantics))
checkBool = checkPredicate isBool

checkTime :: List (Maybe Semantics) -> Maybe (List (Maybe Semantics))
checkTime = checkPredicate isTime

checkCategory :: List (Maybe Semantics) -> Maybe (List (Maybe Semantics))
checkCategory = checkPredicate isCategory


instance encodeJsonSemantics :: EncodeJson Semantics where
  encodeJson (Value n) =
    "type" := "value" ~> "value" := n ~> jsonEmptyObject
  encodeJson (Percent p) =
    "type" := "percent" ~> "value" := p ~> jsonEmptyObject
  encodeJson (Money n v) =
    "type" := "money" ~> "currency" := v ~> "value" := n ~> jsonEmptyObject
  encodeJson (Bool b) =
    "type" := "bool" ~> "value" := b ~> jsonEmptyObject
  encodeJson (Category c) =
    "type" := "category" ~> "value" := c ~> jsonEmptyObject
  encodeJson (Time t) =
    "type" := "time" ~> "value" := t ~> jsonEmptyObject

instance decodeJsonSemantics :: DecodeJson Semantics where
  decodeJson json = do
    obj <- decodeJson json
    ty <- obj .? "type"
    decode' ty obj
    where
    decode' "money" obj = do
      curr <- obj .? "currency"
      m <- obj .? "value"
      pure $ Money m curr
    decode' "time" obj = do
      t <- obj .? "value"
      pure $ Time t
    decode' "bool" obj = do
      b <- obj .? "value"
      pure $ Bool b
    decode' "category" obj = do
      c <- obj .? "value"
      pure $ Category c
    decode' "value" obj = do
      v <- obj .? "value"
      pure $ Value v
    decode' "percent" obj = do
      p <- obj .? "value"
      pure $ Percent p
    decode' _ _ = Left "incorrect type of semantics"
