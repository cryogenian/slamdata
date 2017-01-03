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

module SlamData.Workspace.Card.Chart.Model where

import SlamData.Prelude
import Data.Argonaut (Json, class EncodeJson, class DecodeJson, encodeJson, decodeJson, (~>), (:=), isNull, jsonNull, (.?), jsonEmptyObject)
import Test.StrongCheck.Arbitrary (class Arbitrary, arbitrary)
import Test.StrongCheck.Gen as Gen
import SlamData.Workspace.Card.Chart.PivotTableRenderer.Model as PTRM

data ChartModel
  = PivotTableRenderer PTRM.Model

type Model = Maybe ChartModel

eqModel ∷ Model → Model → Boolean
eqModel = eq

genModel ∷ Gen.Gen Model
genModel =
  arbitrary >>= if _
    then pure Nothing
    else Just ∘ PivotTableRenderer <$> PTRM.genModel

emptyModel ∷ Model
emptyModel = Nothing

encode ∷ Model → Json
encode Nothing = jsonNull
encode (Just r) = encodeJson r

decode ∷ Json → Either String Model
decode js
  | isNull js = pure Nothing
  -- Backwards compatability has the model as an empty object rather than null,
  -- so we have to fallback to a successful Nothing if there's a parse error.
  | otherwise = (Just <$> decodeJson js) <|> pure Nothing

derive instance eqChartModel ∷ Eq ChartModel

instance arbitraryChartModel ∷ Arbitrary ChartModel where
  arbitrary = PivotTableRenderer <$> PTRM.genModel

instance encodeJsonChartModel ∷ EncodeJson ChartModel where
  encodeJson (PivotTableRenderer m1) =
    "renderer" := "pivot"
    ~> "state" := PTRM.encode m1
    ~> jsonEmptyObject

instance decodeJsonChartModel ∷ DecodeJson ChartModel where
  decodeJson json = do
    obj ← decodeJson json
    renderer ← obj .? "renderer"
    state ← obj .? "state"
    case renderer of
      "pivot" → PivotTableRenderer <$> PTRM.decode state
      _       → Left "Not a valid chart renderer"
