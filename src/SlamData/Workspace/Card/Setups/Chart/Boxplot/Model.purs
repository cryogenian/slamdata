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

module SlamData.Workspace.Card.Setups.Chart.Boxplot.Model where

import SlamData.Prelude

import Data.Argonaut (JObject, Json, decodeJson, (~>), (:=), isNull, jsonNull, (.?), jsonEmptyObject)
import Data.Lens ((^.))
import Data.Newtype (un)

import Test.StrongCheck.Arbitrary (arbitrary)
import Test.StrongCheck.Gen as Gen
import Test.StrongCheck.Data.Argonaut (ArbJCursor(..))

import SlamData.Workspace.Card.Setups.Dimension as D
import SlamData.Workspace.Card.Setups.Behaviour as SB
import SlamData.Workspace.Card.Setups.Axis as Ax
import SlamData.Form.Select as S

type ModelR =
  { dimension ∷ D.LabeledJCursor
  , value ∷ D.LabeledJCursor
  , series ∷ Maybe D.LabeledJCursor
  , parallel ∷ Maybe D.LabeledJCursor
  }

type Model = Maybe ModelR

initialModel ∷ Model
initialModel = Nothing

eqR ∷ ModelR → ModelR → Boolean
eqR r1 r2 =
  r1.dimension ≡ r2.dimension
  ∧ r1.value ≡ r2.value
  ∧ r1.series ≡ r2.series
  ∧ r1.parallel ≡ r2.parallel

eqModel ∷ Model → Model → Boolean
eqModel Nothing Nothing = true
eqModel (Just r1) (Just r2) = eqR r1 r2
eqModel _ _ = false

genModel ∷ Gen.Gen Model
genModel = do
  isNothing ← arbitrary
  if isNothing
    then pure Nothing
    else map Just do
    dimension ← map (map (un ArbJCursor) ∘ un D.DimensionWithStaticCategory) arbitrary
    value ← map (map (un ArbJCursor) ∘ un D.DimensionWithStaticCategory) arbitrary
    series ← map (map (un ArbJCursor) ∘ un D.DimensionWithStaticCategory) <$> arbitrary
    parallel ← map (map (un ArbJCursor) ∘ un D.DimensionWithStaticCategory) <$> arbitrary
    pure { dimension, value, series, parallel }

encode ∷ Model → Json
encode Nothing = jsonNull
encode (Just r) =
  "configType" := "boxplot"
  ~> "dimension" := r.dimension
  ~> "value" := r.value
  ~> "series" := r.series
  ~> "parallel" := r.parallel
  ~> jsonEmptyObject

decode ∷ Json → String ⊹ Model
decode js
  | isNull js = pure Nothing
  | otherwise = map Just do
    obj ← decodeJson js
    configType ← obj .? "configType"
    unless (configType ≡ "boxplot")
      $ throwError "This is not boxplot"
    decodeR obj <|> decodeLegacyR obj
  where
  decodeR ∷ JObject → String ⊹ ModelR
  decodeR obj = do
    dimension ← obj .? "dimension"
    value ← obj .? "value"
    series ← obj .? "series"
    parallel ← obj .? "parallel"
    pure { dimension, value, series, parallel }

  decodeLegacyR ∷ JObject → String ⊹ ModelR
  decodeLegacyR obj = do
    dimension ← map D.defaultJCursorDimension $ obj .? "dimension"
    value ← map D.defaultJCursorDimension $ obj .? "value"
    series ← map (map D.defaultJCursorDimension) $ obj .? "series"
    parallel ← map (map D.defaultJCursorDimension) $ obj .? "parallel"
    pure { dimension, value, series, parallel }

type ReducedState r =
  { axes ∷ Ax.Axes
  , dimension ∷ S.Select D.LabeledJCursor
  , value ∷ S.Select D.LabeledJCursor
  , series ∷ S.Select D.LabeledJCursor
  , parallel ∷ S.Select D.LabeledJCursor
  | r}

initialState ∷ ReducedState ()
initialState =
  { axes: Ax.initialAxes
  , dimension: S.emptySelect
  , value: S.emptySelect
  , series: S.emptySelect
  , parallel: S.emptySelect
  }


behaviour ∷ ∀ r. SB.Behaviour (ReducedState r) Model
behaviour =
  { synchronize
  , load
  , save
  }
  where
  synchronize st = st
{-    let
      setPreviousValueFrom =
        S.setPreviousValueOn $ view $ D._value ∘ D._projection

      except =
        S.exceptOn $ view $ D._value ∘ D._projection

      newDimension =
        setPreviousValueFrom st.dimension
          $ S.autoSelect
          $ S.newSelect
          $ map D.defaultJCursorDimension
          $ st.axes.category
          ⊕ st.axes.time
          ⊕ st.axes.date
          ⊕ st.axes.datetime

      newValue =
        setPreviousValueFrom st.value
          $ S.autoSelect
          $ S.newSelect
          $ map D.defaultJCursorDimension
          $ st.axes.value

      newSeries =
        setPreviousValueFrom st.series
          $ S.newSelect
          $ except newDimension
          $ map D.defaultJCursorDimension
          $ S.ifSelected [newDimension]
          $ st.axes.category
          ⊕ st.axes.time

      newParallel =
        setPreviousValueFrom st.parallel
          $ S.newSelect
          $ except newDimension
          $ except newSeries
          $ map D.defaultJCursorDimension
          $ S.ifSelected [newDimension]
          $ st.axes.category
          ⊕ st.axes.time

    in
     st { value = newValue
        , dimension = newDimension
        , series = newSeries
        , parallel = newParallel
        }
-}
  load Nothing st = st
  load (Just m) st =
    st { value = S.fromSelected $ Just m.value
       , dimension = S.fromSelected $ Just m.dimension
       , series = S.fromSelected m.series
       , parallel = S.fromSelected m.parallel
       }

  save st =
    { dimension: _
    , value: _
    , series: st.series ^. S._value
    , parallel: st.parallel ^. S._value
    }
    <$> (st.dimension ^. S._value)
    <*> (st.value ^. S._value)
