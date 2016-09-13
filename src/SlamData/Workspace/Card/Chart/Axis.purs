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

module SlamData.Workspace.Card.Chart.Axis where

import SlamData.Prelude

import Data.Argonaut (JCursor(..), JObject, JArray, Json, insideOut, decodeJson, encodeJson, (:=), (.?), (~>), jsonEmptyObject)
import Data.Array ((!!), length, fromFoldable)
import Data.Foldable as F
import Data.List (List(..), filter, catMaybes)
import Data.Map as M
import Data.StrMap as Sm

import SlamData.Workspace.Card.Chart.Semantics (Semantics, jarrayToSemantics, checkCategory, checkTime, checkBool, checkPercent, checkMoney, checkValues)

import Test.StrongCheck.Arbitrary (class Arbitrary, arbitrary)
import Test.StrongCheck.Gen as Gen
import Test.Property.ArbJson (runArbJCursor)

data Axis
  = ValAxis (List (Maybe Semantics))
  | CatAxis (List (Maybe Semantics))
  | TimeAxis (List (Maybe Semantics))

type Axes =
  { value ∷ Array JCursor
  , time ∷ Array JCursor
  , category ∷ Array JCursor
  }

genAxes ∷ Gen.Gen Axes
genAxes = do
  value ← map (map runArbJCursor) arbitrary
  time ← map (map runArbJCursor) arbitrary
  category ← map (map runArbJCursor) arbitrary
  pure {value, time, category}

encodeAxes ∷ Axes → Json
encodeAxes axes =
  "value" := axes.value
  ~> "time" := axes.time
  ~> "category" := axes.category
  ~> jsonEmptyObject

decodeAxes ∷ JObject → String ⊹ Axes
decodeAxes js = do
  value ← js .? "value"
  category ← js .? "category"
  time ← js .? "time"
  pure {value, category, time}


initialAxes ∷ Axes
initialAxes =
  { value: []
  , time: []
  , category: []
  }

eqAxes ∷ Axes → Axes → Boolean
eqAxes r1 r2 =
  F.and
    [ r1.category ≡ r2.category
    , r1.time ≡ r2.time
    , r1.value ≡ r2.value
    ]

isValAxis ∷ Axis → Boolean
isValAxis (ValAxis _) = true
isValAxis _ = false

isCatAxis ∷ Axis → Boolean
isCatAxis (CatAxis _) = true
isCatAxis _ = false

isTimeAxis ∷ Axis → Boolean
isTimeAxis (TimeAxis _) = true
isTimeAxis _ = false

runAxis ∷ Axis → List (Maybe Semantics)
runAxis (ValAxis a) = a
runAxis (CatAxis a) = a
runAxis (TimeAxis a) = a

checkSemantics ∷ List (Maybe Semantics) → Maybe Axis
checkSemantics lst =
      (ValAxis  <$> checkValues lst)
  <|> (ValAxis  <$> checkMoney lst)
  <|> (ValAxis  <$> checkPercent lst)
  <|> (ValAxis  <$> checkBool lst)
  <|> (TimeAxis <$> checkTime lst)
  <|> (CatAxis  <$> checkCategory lst)

analyzeJArray ∷ JArray → M.Map JCursor Axis
analyzeJArray arr =
  -- If array has exactly one element return it
  do guard $ length arr == 1
     arr !! 0
  -- If element returned transpose it else take initial array
  # maybe arr toKeyValJArray
  -- Produce map from JCursor to List of values (Maybe Semantics) for every Json
  # jarrayToSemantics
  -- Check if values of that map can be converted to axes (if can it will be Just)
  # map checkSemantics
  -- Make list of Tuple JCursor (Maybe Axis)
  # M.toList
  -- lift Maybe to Tuple from Axis
  # map sequence
  -- Drop Nothings
  # catMaybes
  -- Create new Map
  # M.fromFoldable
  -- Drop records those keys have no relations to other keys
  # checkPairs

  where
  -- | Translate encoded {foo: 1, bar: 2}
  -- | to [{key: "foo", value: 1}, {key: "bar", value: 2}]
  toKeyValJArray ∷ Json → JArray
  toKeyValJArray =
    decodeJson >>> (map toKeyValJsons) >>> either (const []) fromFoldable

  -- | same as `toKeyValJArray` but argument isn't encoded and it returns `List`
  toKeyValJsons ∷ JObject → List Json
  toKeyValJsons =
    Sm.toList >>> map (toKeyVals >>> Sm.fromFoldable >>> encodeJson)

  toKeyVals ∷ Tuple String Json → List (Tuple String Json)
  toKeyVals (Tuple key val) =
    Cons (Tuple "key" $ encodeJson key)
    $ Cons (Tuple "value" val) Nil

getPossibleDependencies ∷ JCursor → M.Map JCursor Axis → List JCursor
getPossibleDependencies cursor m =
  filter (dependsOn cursor) $ M.keys m

checkPairs ∷ M.Map JCursor Axis → M.Map JCursor Axis
checkPairs m =
  foldl check m $ M.keys m
  where
  check ∷ M.Map JCursor Axis → JCursor → M.Map JCursor Axis
  check m cursor =
    case getPossibleDependencies cursor m of
      Nil → M.delete cursor m
      _ → m

dependsOn ∷ JCursor → JCursor → Boolean
dependsOn a b = a /= b &&
  dependsOn' (insideOut a) (insideOut b)
  where
  dependsOn' JCursorTop JCursorTop = true
  dependsOn' JCursorTop _ = true
  dependsOn' _ JCursorTop = true
  dependsOn' (JField _ c) (JField _ c') = dependsOn' c c'
  dependsOn' (JIndex _ c) (JIndex _ c') = dependsOn' c c'
  dependsOn' _ _ = false

newtype ParentJCursor = ParentJCursor JCursor
runParentJCursor ∷ ParentJCursor → JCursor
runParentJCursor (ParentJCursor x) = x

isDrilledByIndex ∷ JCursor → ParentJCursor → Boolean
isDrilledByIndex child (ParentJCursor parent) = isDrilledByIndex' child parent
  where
  isDrilledByIndex' JCursorTop (JIndex _ JCursorTop) = true
  isDrilledByIndex' (JField a1 j1) (JField a2 j2) | a1 ≡ a2 = isDrilledByIndex' j1 j2
  isDrilledByIndex' (JIndex i1 j1) (JIndex i2 j2) | i1 ≡ i2 = isDrilledByIndex' j1 j2
  isDrilledByIndex' _ _ = false

instance axisShow ∷ Show Axis where
  show (ValAxis vs) = "(ValAxis " <> show vs <> ")"
  show (CatAxis vs) = "(CatAxis " <> show vs <> ")"
  show (TimeAxis vs) = "(TimeAxis " <> show vs <> ")"
