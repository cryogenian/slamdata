module Model.Notebook.ECharts (
  analyzeJArray,
  getPossibleDependencies,
  dependsOn,
  isComplement,
  Axis(..),
  Semanthic(..),
  isValue,
  isPercent,
  isMoney,
  isBool,
  isTime,
  isCategory,
  isValAxis,
  isCatAxis,
  isTimeAxis,
  runAxis,
  catFromSemanthic,
  valFromSemanthic
  ) where

import Control.Alt ((<|>))
import Data.Argonaut.Core (Json(), JArray(), toNumber, toString)
import Data.Argonaut.JCursor (JCursor(..), JsonPrim(), toPrims, runJsonPrim, insideOut)
import Data.Array (filter, concat, nubBy, singleton, sortBy, head, reverse, length)
import Data.Bifunctor (rmap)
import Data.Foldable (for_, traverse_, foldl, fold)
import Data.Function (on)
import Data.Map (fromList, Map(), toList, empty, alter, insert, keys, lookup, delete)
import Data.Maybe (Maybe(..), maybe, isJust, isNothing)
import Data.Maybe.Unsafe (fromJust)
import Data.Monoid.All (All(..), runAll)
import Data.String (take)
import Data.String.Regex (noFlags, regex, match, test)
import Data.Tuple (Tuple(..), fst, snd)
import Global (readFloat, isNaN)
import Data.Argonaut.Decode (DecodeJson, decodeJson)
import Data.Argonaut.Encode (EncodeJson)
import Data.Argonaut.Combinators ((~>), (:=), (.?))
import Data.Argonaut.Core (fromString, jsonEmptyObject, JArray(), Json())
import Data.Either


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


data Semanthic
  = Value Number
  | Percent Number
  | Money Number String
  | Bool Boolean
  | Category String 
  | Time String

instance encodeJsonSemanthic :: EncodeJson Semanthic where
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

instance decodeJsonSemanthic :: DecodeJson Semanthic where
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
  = ValAxis [Maybe Semanthic]
  | CatAxis [Maybe Semanthic]
  | TimeAxis [Maybe Semanthic]


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

runAxis :: Axis -> [Maybe Semanthic]
runAxis (ValAxis a) = a
runAxis (CatAxis a) = a
runAxis (TimeAxis a) = a



instance axisShow :: Show Axis where
  show (ValAxis vs) = "(ValAxis " <> show vs <> ")"
  show (CatAxis vs) = "(CatAxis " <> show vs <> ")"
  show (TimeAxis vs) = "(TimeAxis " <> show vs <> ")"

isValue :: Semanthic -> Boolean
isValue (Value _) = true
isValue _ = false

isPercent :: Semanthic -> Boolean
isPercent (Percent _) = true
isPercent _ = false

isMoney :: Semanthic -> Boolean
isMoney (Money _ _) = true
isMoney _ = false

isBool :: Semanthic -> Boolean
isBool (Bool _) = true
isBool _ = false

isCategory :: Semanthic -> Boolean
isCategory (Category _) = true
isCategory _ = false

isTime :: Semanthic -> Boolean
isTime (Time _) = true
isTime _ = false

isComplement :: Axis -> Axis -> Boolean
isComplement (CatAxis _) (CatAxis _) = false
isComplement (TimeAxis _) (TimeAxis _) = false
isComplement _ _ = true

catFromSemanthic :: Semanthic -> Maybe String 
catFromSemanthic v = 
  case v of
    Value v -> pure $ show v
    Percent v -> pure $ show v <> "%"
    Money v m -> pure $ show v <> m
    Category s -> pure s
    Time t -> pure t

valFromSemanthic :: Semanthic -> Maybe Number
valFromSemanthic v =
  case v of
    Value v -> pure v
    Money v _ -> pure v
    Percent v -> pure v
    _ -> Nothing


check :: [Maybe Semanthic] -> Maybe Axis
check lst =
  (ValAxis <$> checkValues lst) <|>
  (ValAxis <$> checkMoney lst) <|>
  (ValAxis <$> checkPercent lst) <|>
  (ValAxis <$> checkBool lst) <|>
  (TimeAxis <$> checkTime lst) <|>
  (CatAxis <$> checkCategory lst)

checkPredicate :: (Semanthic -> Boolean) -> [Maybe Semanthic] ->
                  Maybe [Maybe Semanthic]
checkPredicate p lst =
  if runAll (fold (isPredicate <$> lst))
  then Just lst
  else Nothing
  where isPredicate = All <<< (maybe true p)
        
checkValues :: [Maybe Semanthic] -> Maybe [Maybe Semanthic]
checkValues = checkPredicate isValue

checkMoney :: [Maybe Semanthic] -> Maybe [Maybe Semanthic]
checkMoney = checkPredicate isMoney 

checkPercent :: [Maybe Semanthic] -> Maybe [Maybe Semanthic]
checkPercent = checkPredicate isPercent 

checkBool :: [Maybe Semanthic] -> Maybe [Maybe Semanthic]
checkBool = checkPredicate isBool

checkTime :: [Maybe Semanthic] -> Maybe [Maybe Semanthic]
checkTime = checkPredicate isTime

checkCategory :: [Maybe Semanthic] -> Maybe [Maybe Semanthic]
checkCategory = checkPredicate isCategory

instance semanthicShow :: Show Semanthic where
  show (Value v) = "(Value " <> show v <> ")"
  show (Percent p) = "(Percent " <> show p <> ")"
  show (Money n s) = "(Money " <> show n <> s <> ")"
  show (Bool b) = "(Bool " <> show b <> ")"
  show (Category s) = "(Category " <> s <> ")"
  show (Time t) = "(Time " <> t <> ")"

instance semanthicEq :: Eq Semanthic where
  (==) (Value v) (Value v') = v == v'
  (==) (Percent p) (Percent p') = p == p'
  (==) (Money m c) (Money m' c') = m == m' && c == c'
  (==) (Time t) (Time t') = t == t'
  (==) (Category c) (Category c') = c == c'
  (==) _ _ = false
  (/=) a b = not $ a == b

instance semanthicOrd :: Ord Semanthic where
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


toPrims' :: JArray -> [Tuple JCursor JsonPrim]
toPrims' arr = (nubBy (on (==) fst)) <<< concat $ (toPrims <$> arr)

analyze :: JsonPrim -> Maybe Semanthic
analyze p = runJsonPrim p
            (const Nothing)
            (Just <<< Bool)
            (Just <<< Value)
            analyzeString

analyzeString :: String -> Maybe Semanthic
analyzeString str =
  (analyzeDate str) <|>
  (analyzeMoney str) <|>
  (analyzePercent str) <|>
  (Just $ Category str)

s2n :: String -> Maybe Number
s2n s =
  if isNaN num
  then Nothing
  else Just num
  where num = readFloat s 

analyzePercent :: String -> Maybe Semanthic
analyzePercent input = 
  case match rgx input of
    Just (_:s:_) -> Percent <$> s2n s
    _ -> Nothing
  where rgx = regex """^(-?\d+(\.\d+)?)\%$""" noFlags 

curSymbols :: String
curSymbols = """[\$\u20A0-\u20CF\u00A2\u00A3\u00A4\u00A5\u058F\u060B\u09F2\u09F3\u09FB\u0AF1\u0BF9\u0E3F\u17DB\uA838\uFDFC\uFE69\uFF04\uFFE0\uFFE1\uFFE5\uFFE6]"""

analyzeMoney :: String -> Maybe Semanthic
analyzeMoney str =
  case match rgx str of
    Just (_:s:_) -> Money <$> s2n s <*> curSymbol
    _ -> Nothing
  where
  rgx = regex rgxStr noFlags
  rgxStr = "^" <> curSymbols <> """?(([0-9]{1,3},([0-9]{3},)*[0-9]{3}|[0-9]+)(.[0-9][0-9])?)$"""
  fstSymbol = take 1 str
  curSymbol :: Maybe String
  curSymbol =
    if fstSymbol /= "" 
    then Just fstSymbol
    else Nothing

analyzeDate :: String -> Maybe Semanthic
analyzeDate str =
  if test rgx str
  then Just $ Time str
  else Nothing 
  where
  rgxStr = "^(-?(?:[1-9][0-9]*)?[0-9]{4})-(1[0-2]|0[1-9])-(3[0-1]|0[1-9]|[1-2][0-9]) (2[0-3]|[0-1][0-9]):([0-5][0-9]):([0-5][0-9])(\\.[0-9]+)?(Z|[+-](?:2[0-3]|[0-1][0-9]):[0-5][0-9])?$"
  rgx = regex rgxStr noFlags 
       
toSemanthic :: Json -> Map JCursor Semanthic
toSemanthic j = fromList' empty (rmap analyze <$> toPrims j)
  where
  fromList' acc [] = acc
  fromList' acc ((Tuple _ Nothing):lst) = fromList' acc lst
  fromList' acc ((Tuple k (Just el)):lst) = fromList' (insert k el acc) lst

toSemanthic' :: JArray -> Map JCursor [Maybe Semanthic] 
toSemanthic' arr =
  reverse <$> (mergeMaps empty (toSemanthic <$> arr))
  where
  mergeMaps :: Map JCursor [Maybe Semanthic] -> [Map JCursor Semanthic] -> Map JCursor [Maybe Semanthic]
  mergeMaps acc [] = acc
  mergeMaps acc (m:lst) =
    let alter' a k = alter (mergeFn k) k a
        mergeFn :: JCursor -> Maybe [Maybe Semanthic] -> Maybe [Maybe Semanthic]
        mergeFn k Nothing = Just [lookup k m]
        mergeFn k (Just l) = Just $ (lookup k m):l
        newMap = foldl alter' acc $ keys m
    in mergeMaps newMap lst 


analyzeJArray :: JArray -> Map JCursor Axis
analyzeJArray arr =
  checkPairs $ 
  fromList $ 
  (rmap fromJust) <$> 
  (filter (isJust <<< snd) $ 
    (toList (check <$> (toSemanthic' arr))))


getPossibleDependencies :: JCursor -> Map JCursor Axis -> [JCursor]
getPossibleDependencies cursor m =
  filter (dependsOn cursor) $ keys m


checkPairs :: Map JCursor Axis -> Map JCursor Axis
checkPairs m =
  foldl check m ks 
  where
  ks = keys m
  check :: Map JCursor Axis -> JCursor -> Map JCursor Axis
  check m cursor = 
    if length (getPossibleDependencies cursor m) > 0
    then m
    else delete cursor m
