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

module SlamData.Workspace.Card.Ace.Model
  ( Model
  , emptyModel
  , eqModel
  , encode
  , decode
  , genModel
  ) where

import SlamData.Prelude

import Data.Array as A
import Data.Foldable as F
import Data.Argonaut (Json, (:=), (~>), (.?), decodeJson, encodeJson, jsonEmptyObject, isNull)

import Test.StrongCheck.Arbitrary as SC
import Test.StrongCheck.Gen as Gen

import Utils.Ace (RangeRec, encodeRangeRec, decodeRangeRec, eqRangeRec, genRangeRec)

type Model =
  { text ∷ String
  , ranges ∷ Array RangeRec
  }

eqModel ∷ Model → Model → Boolean
eqModel m1 m2 =
  m1.text ≡ m2.text
    && F.all id (A.zipWith eqRangeRec m1.ranges m2.ranges)

genModel ∷ Gen.Gen Model
genModel = do
  text ← SC.arbitrary
  ranges ← Gen.arrayOf genRangeRec
  pure { text, ranges }

emptyModel ∷ Model
emptyModel =
  { text: ""
  , ranges: []
  }

encode ∷ Model → Json
encode m =
  ("text" := m.text)
  ~> ("ranges" := (encodeJson $ map encodeRangeRec m.ranges))
  ~> jsonEmptyObject

decode ∷ Json → Either String Model
decode js
  | isNull js = pure emptyModel
  | otherwise = do
      obj ← decodeJson js
      m ←
        { text: _, ranges: _ }
        <$> (obj .? "text")
        <*> (obj .? "ranges" >>= traverse decodeRangeRec)
      pure m
