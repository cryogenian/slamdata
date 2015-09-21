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

module Model.Notebook.ECharts
  ( analyzeJArray
  , getPossibleDependencies
  , dependsOn
  , isComplement
  , Axis(..)
  , Semantics(..)
  , isValue
  , isPercent
  , isMoney
  , isBool
  , isTime
  , isCategory
  , isValAxis
  , isCatAxis
  , isTimeAxis
  , runAxis
  , catFromSemantics
  , valFromSemantics
  ) where

import Prelude
import Control.Alt ((<|>))
import Control.MonadPlus (guard)
import Data.Argonaut.Core (Json(), JArray(), toNumber, toString)
import Data.Argonaut.JCursor (JCursor(..), JsonPrim(), toPrims, runJsonPrim, insideOut)
import Data.Array (filter, concat, nubBy, singleton, sortBy, head, reverse, length, tail, (!!), (:), nub, elemIndex)
import Data.Bifunctor (rmap)
import Data.Foldable (for_, traverse_, foldl, fold, Foldable)
import Data.Traversable (traverse, sequence)
import Data.Function (on)
import Data.Map (fromList, Map(), toList, empty, alter, insert, keys, lookup, delete, update)
import Data.Maybe (Maybe(..), maybe, isJust, isNothing, fromMaybe)
import Data.Maybe.Unsafe (fromJust)
import Data.Monoid.Conj (Conj(..), runConj)
import Data.String (take)
import Data.String.Regex (noFlags, regex, match, test)
import Data.Tuple (Tuple(..), fst, snd)
import Global (readFloat, isNaN)
import Data.Argonaut.Decode (DecodeJson, decodeJson)
import Data.Argonaut.Encode (EncodeJson, encodeJson)
import Data.Argonaut.Combinators ((~>), (:=), (.?))
import Data.Argonaut.Core (fromString, jsonEmptyObject, JArray(), Json())
import Data.Either
import Data.Unfoldable (Unfoldable)
import Utils (s2n, s2i)
import qualified Data.List as L
import qualified Data.StrMap as Sm

dependsOn :: JCursor -> JCursor -> Boolean
dependsOn a b = a /= b &&
  dependsOn' (insideOut a) (insideOut b)
  where
  dependsOn' JCursorTop JCursorTop = true
  dependsOn' JCursorTop _ = true
  dependsOn' _ JCursorTop = true
  dependsOn' (JField _ c) (JField _ c') = dependsOn' c c'
  dependsOn' (JIndex _ c) (JIndex _ c') = dependsOn' c c'
  dependsOn' _ _ = false


data Semantics
  = Value Number
  | Percent Number
  | Money Number String
  | Bool Boolean
  | Category String
  | Time String

instance encodeJsonSemantics :: EncodeJson Semantics where
  encodeJson (Value n) = "type" := "value"
                         ~> "value" := n
                         ~> jsonEmptyObject

  encodeJson (Percent n) = "type" := "percent"
                           ~> "value" := n
                           ~> jsonEmptyObject

  encodeJson (Money n v) = "type" := "money"
                           ~> "value" := n
                           ~> "currency" := v
                           ~> jsonEmptyObject

  encodeJson (Bool b) = "type" := "bool"
                        ~> "value" := b
                        ~> jsonEmptyObject

  encodeJson (Category c) = "type" := "category"
                            ~> "value" := c
                            ~> jsonEmptyObject

  encodeJson (Time t) = "type" := "time"
                        ~> "value" := t
                        ~> jsonEmptyObject

instance decodeJsonSemantics :: DecodeJson Semantics where
  decodeJson json = do
    obj <- decodeJson json
    ty <- obj .? "type"
    case ty of
      "money" -> do
        curr <- obj .? "currency"
        m <- obj .? "value"
        pure (Money m curr)
      "time" -> do
        t <- obj .? "value"
        pure $ Time t
      "bool" -> do
        b <- obj .? "value"
        pure $ Bool b
      "category" -> do
        c <- obj .? "value"
        pure $ Category c
      "value" -> do
        v <- obj .? "value"
        pure $ Value v
      "percent" -> do
        p <- obj .? "value"
        pure $ Percent p
      _ -> Left "incorrect type of semanthic"



data Axis
  = ValAxis (L.List (Maybe Semantics))
  | CatAxis (L.List (Maybe Semantics))
  | TimeAxis (L.List (Maybe Semantics))


instance encodeJsonAxis :: EncodeJson Axis where
  encodeJson (ValAxis mbs) = "type" := "val-axis"
                             ~> "els" := mbs
                             ~> jsonEmptyObject

  encodeJson (CatAxis mbs) = "type" := "cat-axis"
                             ~> "els" := mbs
                             ~> jsonEmptyObject

  encodeJson (TimeAxis mbs) = "type" := "time-axis"
                              ~> "els" := mbs
                              ~> jsonEmptyObject

instance decodeJsonAxis :: DecodeJson Axis where
  decodeJson json = do
    obj <- decodeJson json
    ty <- obj .? "type"
    mbs <- obj .? "els"
    case ty of
      "val-axis" -> pure $ ValAxis mbs
      "cat-axis" -> pure $ CatAxis mbs
      "time-axis" -> pure $ TimeAxis mbs
      _ -> Left "incorrect axis type"



isValAxis :: Axis -> Boolean
isValAxis (ValAxis _) = true
isValAxis _ = false

isCatAxis :: Axis -> Boolean
isCatAxis (CatAxis _) = true
isCatAxis _ = false

isTimeAxis :: Axis -> Boolean
isTimeAxis (TimeAxis _) = true
isTimeAxis _ = false

runAxis :: Axis -> L.List (Maybe Semantics)
runAxis (ValAxis a) = a
runAxis (CatAxis a) = a
runAxis (TimeAxis a) = a



instance axisShow :: Show Axis where
  show (ValAxis vs) = "(ValAxis " <> show vs <> ")"
  show (CatAxis vs) = "(CatAxis " <> show vs <> ")"
  show (TimeAxis vs) = "(TimeAxis " <> show vs <> ")"

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

isComplement :: Axis -> Axis -> Boolean
isComplement (CatAxis _) (CatAxis _) = false
isComplement (TimeAxis _) (TimeAxis _) = false
isComplement _ _ = true

catFromSemantics :: Semantics -> Maybe String
catFromSemantics v =
  case v of
    Value v -> pure $ show v
    Percent v -> pure $ show v <> "%"
    Money v m -> pure $ show v <> m
    Category s -> pure s
    Time t -> pure t
    Bool b -> pure $ show b

valFromSemantics :: Semantics -> Maybe Number
valFromSemantics v =
  case v of
    Value v -> pure v
    Money v _ -> pure v
    Percent v -> pure v
    _ -> Nothing


checkSemantics :: L.List (Maybe Semantics) -> Maybe Axis
checkSemantics lst =
  (ValAxis  <$> checkValues lst)   <|>
  (ValAxis  <$> checkMoney lst)    <|>
  (ValAxis  <$> checkPercent lst)  <|>
  (ValAxis  <$> checkBool lst)     <|>
  (TimeAxis <$> checkTime lst)     <|>
  (CatAxis  <$> checkCategory lst)

checkPredicate :: (Semantics -> Boolean) -> L.List (Maybe Semantics) ->
                  Maybe (L.List (Maybe Semantics))
checkPredicate p lst =
  go 0 0 lst L.Nil
  where
  -- traversed. If there is more correct value then incorrect return
  -- list with filtered values. else nothing.
  go correct incorrect L.Nil acc
    | correct > incorrect = Just $ L.reverse acc
    | otherwise = Nothing
  -- There is no value. Just put it to acc don't touch counters
  go correct incorrect (L.Cons Nothing lst) acc =
    go correct incorrect lst (L.Cons Nothing acc)
  -- There is a value
  go correct incorrect (L.Cons (Just c) lst) acc
    -- It means nothing. Put Nothing to accum and don't touch counters
    | isJust $ elemIndex c nothings = go correct incorrect lst (L.Cons Nothing acc)
    -- It's correct. Increase correct counter, put value to accum
    | p c = go (correct + one) incorrect lst (L.Cons (Just c) acc)
    -- It's incorrect. Increase incorrect counter, put nothing to accum
    | otherwise = go correct (incorrect + one) lst (L.Cons Nothing acc)

  nothings :: Array Semantics
  nothings = map Category [ "undefined"
                          , "null"
                          , "NA"
                          , "N/A"
                          ]

checkValues :: L.List (Maybe Semantics) -> Maybe (L.List (Maybe Semantics))
checkValues = checkPredicate isValue

checkMoney :: L.List (Maybe Semantics) -> Maybe (L.List (Maybe Semantics))
checkMoney = checkPredicate isMoney

checkPercent :: L.List (Maybe Semantics) -> Maybe (L.List (Maybe Semantics))
checkPercent = checkPredicate isPercent

checkBool :: L.List (Maybe Semantics) -> Maybe (L.List (Maybe Semantics))
checkBool = checkPredicate isBool

checkTime :: L.List (Maybe Semantics) -> Maybe (L.List (Maybe Semantics))
checkTime = checkPredicate isTime

checkCategory :: L.List (Maybe Semantics) -> Maybe (L.List (Maybe Semantics))
checkCategory = checkPredicate isCategory

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
  (analyzeDate str) <|>
  (analyzeNumber str) <|>
  (analyzeMoney str) <|>
  (analyzePercent str) <|>
  (Just $ Category str)

analyzeNumber :: String -> Maybe Semantics
analyzeNumber s = do
  num <- s2n s
  i <- s2i s
  guard $ show num == s || show i == s
  pure $ Value num


analyzePercent :: String -> Maybe Semantics
analyzePercent input = do
  ms <- match rgx input
  s <- (ms !! 1)
  n <- (s >>= s2n)
  pure $ Percent n
  where rgx = regex """^(-?\d+(\.\d+)?)\%$""" noFlags

curSymbols :: String
curSymbols = """[\$\u20A0-\u20CF\u00A2\u00A3\u00A4\u00A5\u058F\u060B\u09F2\u09F3\u09FB\u0AF1\u0BF9\u0E3F\u17DB\uA838\uFDFC\uFE69\uFF04\uFFE0\uFFE1\uFFE5\uFFE6]"""

analyzeMoney :: String -> Maybe Semantics
analyzeMoney str = do
  ms <- match rgx str
  s <- (ms !! 1)
  n <- (s >>= s2n)
  curSymbol <- let fstSymbol = take 1 str
               in if fstSymbol == ""
                  then Nothing
                  else pure fstSymbol

  pure $ Money n curSymbol
  where
  rgx = regex rgxStr noFlags
  rgxStr = "^" <> curSymbols <> """?(([0-9]{1,3},([0-9]{3},)*[0-9]{3}|[0-9]+)(.[0-9][0-9])?)$"""


analyzeDate :: String -> Maybe Semantics
analyzeDate str =
  if test rgx str
  then Just $ Time str
  else Nothing
  where
  rgxStr = "^(-?(?:[1-9][0-9]*)?[0-9]{4})-(1[0-2]|0[1-9])-(3[0-1]|0[1-9]|[1-2][0-9]) (2[0-3]|[0-1][0-9]):([0-5][0-9]):([0-5][0-9])(\\.[0-9]+)?(Z|[+-](?:2[0-3]|[0-1][0-9]):[0-5][0-9])?$"
  rgx = regex rgxStr noFlags

toSemantics :: Json -> Map JCursor Semantics
toSemantics j = fromList $ L.catMaybes ((traverse analyze) <$> toPrims j)


toSemantics' :: JArray -> Map JCursor (L.List (Maybe Semantics))
toSemantics' arr =
  step initial mapLst
  where
  mapArr = map toSemantics $ reverse arr
  mapLst = L.toList mapArr
  ks = L.toList $ nub $ concat $ map (L.fromList <<< keys) mapArr
  initial = fromList $ map (flip Tuple L.Nil) ks

  step acc L.Nil = acc
  step acc (L.Cons m lst) =
    step (insertOne acc m ks) lst

  insertOne acc m L.Nil = acc
  insertOne acc m (L.Cons k ks') =
    insertOne (update (pure <<< L.Cons (lookup k m)) k acc) m ks'

analyzeJArray :: JArray -> Map JCursor Axis
analyzeJArray arr =
  -- If array has exactly one element return it
  do guard $ length arr == 1
     arr !! 0
  -- If element returned transpose it else take initial array
  # maybe arr transpose
  -- Produce map from JCursor to List of values (Maybe Semantics) for every Json
  # toSemantics'
  -- Check if values of that map can be converted to axes (if can it will be Just)
  # map checkSemantics
  -- Make list of Tuple JCursor (Maybe Axis)
  # toList
  -- lift Maybe to Tuple from Axis
  # map sequence
  -- Drop Nothings
  # L.catMaybes
  -- Create new Map
  # fromList
  -- Drop records those keys have no relations to other keys
  # checkPairs


  where
  transpose :: Json -> JArray
  transpose =
    -- decoding Json it should be a JObject to success
    decodeJson
    -- mapping Either
    -- convert StrMap to list of Tuples
    >>> map (Sm.toList
             -- mapping list
             -- make pair of Tuples from this one. One for key. One for value
             >>> map (tpl2sm
                      -- make StrMap from that pair {key: a, value: b}
                      >>> Sm.fromList
                      -- encode it to json
                      >>> encodeJson))
    -- if decoding to StrMap has no success return empty JArray it will
    -- not produce any Axises. Else convert List of JObjects to Array
    >>> either (const []) L.fromList

  tpl2sm :: Tuple String Json -> L.List (Tuple String Json)
  tpl2sm (Tuple key val) = L.Cons (Tuple "key" $ encodeJson key)
                           $ L.Cons (Tuple "value" val)
                           $ L.Nil

getPossibleDependencies :: JCursor -> Map JCursor Axis -> L.List JCursor
getPossibleDependencies cursor m =
  L.filter (dependsOn cursor) $ keys m


checkPairs :: Map JCursor Axis -> Map JCursor Axis
checkPairs m =
  foldl check m $ keys m
  where
  check :: Map JCursor Axis -> JCursor -> Map JCursor Axis
  check m cursor =
    case getPossibleDependencies cursor m of
      L.Nil -> delete cursor m
      _ -> m
