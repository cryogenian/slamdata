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

module SlamData.Workspace.Card.Setups.Chart.Metric.Model where

import SlamData.Prelude

import Data.Argonaut (JCursor, Json, (.?), (:=), (~>), jsonEmptyObject, jsonNull, isNull, decodeJson)
import Data.Lens ((^.))

import SlamData.Workspace.Card.Setups.Transform.Aggregation as Ag

import Test.StrongCheck.Arbitrary (arbitrary)
import Test.StrongCheck.Gen as Gen
import Test.StrongCheck.Data.Argonaut (runArbJCursor)

import SlamData.Workspace.Card.Setups.Transform.Aggregation (Aggregation, nonMaybeAggregationSelect)
import SlamData.Workspace.Card.Setups.Behaviour as SB
import SlamData.Workspace.Card.Setups.Axis as Ax
import SlamData.Form.Select as S

type MetricR =
  { value ∷ JCursor
  , valueAggregation ∷ Ag.Aggregation
  , label ∷ Maybe String
  , formatter ∷ Maybe String
  }

type Model = Maybe MetricR

initialModel ∷ Model
initialModel = Nothing

eqMetricR ∷ MetricR → MetricR → Boolean
eqMetricR r1 r2 =
  r1.value ≡ r2.value
  ∧ r1.valueAggregation ≡ r2.valueAggregation
  ∧ r1.label ≡ r2.label
  ∧ r1.formatter ≡ r2.formatter

eqModel ∷ Model → Model → Boolean
eqModel Nothing Nothing = true
eqModel (Just r1) (Just r2) = eqMetricR r1 r2
eqModel _ _ = false

genModel ∷ Gen.Gen Model
genModel = do
  isNothing ← arbitrary
  if isNothing
    then pure Nothing
    else do
    value ← map runArbJCursor arbitrary
    valueAggregation ← arbitrary
    label ← arbitrary
    formatter ← arbitrary
    pure $ Just { value, valueAggregation, label, formatter }

encode ∷ Model → Json
encode Nothing = jsonNull
encode (Just r) =
  "configType" := "metric"
  ~> "value" := r.value
  ~> "valueAggregation" := r.valueAggregation
  ~> "label" := r.label
  ~> "formatter" := r.formatter
  ~> jsonEmptyObject

decode ∷ Json → String ⊹ Model
decode js
  | isNull js = pure Nothing
  | otherwise = do
    obj ← decodeJson js
    configType ← obj .? "configType"
    unless (configType ≡ "metric")
      $ throwError "Incorrect build metric model"
    value ← obj .? "value"
    valueAggregation ← obj .? "valueAggregation"
    label ← obj .? "label"
    formatter ← obj .? "formatter"
    pure $ Just {value, valueAggregation, label, formatter }


type ReducedState r =
  { value ∷ S.Select JCursor
  , valueAgg ∷ S.Select Aggregation
  , label ∷ Maybe String
  , formatter ∷ Maybe String
  , axes ∷ Ax.Axes
  | r}

behaviour ∷ ∀ r. SB.Behaviour (ReducedState r) Model
behaviour =
  { synchronize
  , load
  , save
  }
  where
  synchronize st =
    let
      newValue =
        S.setPreviousValueFrom (Just st.value)
          $ S.autoSelect
          $ S.newSelect
          $ st.axes.value

      newValueAggregation =
        S.setPreviousValueFrom (Just st.valueAgg)
          $ nonMaybeAggregationSelect
    in
     st{ value = newValue
       , valueAgg = newValueAggregation
       }
  load Nothing st = st
  load (Just m) st =
    st{ formatter = m.formatter
      , label = m.label
      , value = S.fromSelected $ Just m.value
      , valueAgg = S.fromSelected $ Just m.valueAggregation
      }
  save st =
    { value: _
    , valueAggregation: _
    , label: st.label
    , formatter: st.formatter
    }
    <$> (st.value ^. S._value)
    <*> (st.valueAgg ^. S._value)

initialState ∷ ReducedState ()
initialState =
    { value: S.emptySelect
    , valueAgg: S.emptySelect
    , label: Nothing
    , formatter: Nothing
    , axes: Ax.initialAxes
    }
