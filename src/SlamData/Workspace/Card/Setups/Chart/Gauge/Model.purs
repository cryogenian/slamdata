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

module SlamData.Workspace.Card.Setups.Chart.Gauge.Model where

import SlamData.Prelude

import Data.Argonaut (JCursor, Json, decodeJson, (~>), (:=), isNull, jsonNull, (.?), jsonEmptyObject)
import Data.Lens ((^.))

import SlamData.Workspace.Card.Setups.Transform.Aggregation as Ag

import Test.StrongCheck.Arbitrary (arbitrary)
import Test.StrongCheck.Gen as Gen
import Test.StrongCheck.Data.Argonaut (runArbJCursor)

import SlamData.Workspace.Card.Setups.Transform.Aggregation (Aggregation, nonMaybeAggregationSelect)
import SlamData.Workspace.Card.Setups.Behaviour as SB
import SlamData.Workspace.Card.Setups.Axis as Ax
import SlamData.Form.Select ((⊝))
import SlamData.Form.Select as S

type GaugeR =
  { value ∷ JCursor
  , valueAggregation ∷ Ag.Aggregation
  , parallel ∷ Maybe JCursor
  , multiple ∷ Maybe JCursor
  }

type Model = Maybe GaugeR

initialModel ∷ Model
initialModel = Nothing

eqGaugeR ∷ GaugeR → GaugeR → Boolean
eqGaugeR r1 r2 =
  r1.value ≡ r2.value
  ∧ r1.valueAggregation ≡ r2.valueAggregation
  ∧ r1.parallel ≡ r2.parallel
  ∧ r1.multiple ≡ r2.multiple

eqModel ∷ Model → Model → Boolean
eqModel Nothing Nothing = true
eqModel (Just r1) (Just r2) = eqGaugeR r1 r2
eqModel _ _ = false

genModel ∷ Gen.Gen Model
genModel = do
  isNothing ← arbitrary
  if isNothing
    then pure Nothing
    else do
    value ← map runArbJCursor arbitrary
    valueAggregation ← arbitrary
    parallel ← map (map runArbJCursor) arbitrary
    multiple ← map (map runArbJCursor) arbitrary
    pure
      $ Just { value
             , valueAggregation
             , parallel
             , multiple
             }

encode ∷ Model → Json
encode Nothing = jsonNull
encode (Just r) =
  "configType" := "gauge"
  ~> "value" := r.value
  ~> "valueAggregation" := r.valueAggregation
  ~> "parallel" := r.parallel
  ~> "multiple" := r.multiple
  ~> jsonEmptyObject

decode ∷ Json → String ⊹ Model
decode js
  | isNull js = pure Nothing
  | otherwise = do
    obj ← decodeJson js
    configType ← obj .? "configType"
    unless (configType ≡ "gauge")
      $ throwError "This config is not gauge"
    value ← obj .? "value"
    valueAggregation ← obj .? "valueAggregation"
    parallel ← obj .? "parallel"
    multiple ← obj .? "multiple"
    pure $ Just { value
                , valueAggregation
                , parallel
                , multiple
                }

type ReducedState r =
  { axes ∷ Ax.Axes
  , value ∷ S.Select JCursor
  , valueAgg ∷ S.Select Aggregation
  , multiple ∷ S.Select JCursor
  , parallel ∷ S.Select JCursor
  | r}

initialState ∷ ReducedState ()
initialState =
  { axes: Ax.initialAxes
  , value: S.emptySelect
  , valueAgg: S.emptySelect
  , multiple: S.emptySelect
  , parallel: S.emptySelect
  }

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

      newParallel =
        S.setPreviousValueFrom (Just st.parallel)
          $ S.newSelect
          $ S.ifSelected [newValue]
          $ st.axes.category
          ⊕ st.axes.time

      newMultiple =
        S.setPreviousValueFrom (Just st.multiple)
          $ S.newSelect
          $ S.ifSelected [newValue]
          $ st.axes.category
          ⊕ st.axes.time
          ⊝ newParallel

    in
      st{ value = newValue
        , valueAgg = newValueAggregation
        , parallel = newParallel
        , multiple = newMultiple
        }

  load Nothing st = st
  load (Just m) st =
    st{ value = S.fromSelected $ Just m.value
      , valueAgg = S.fromSelected $ Just m.valueAggregation
      , multiple = S.fromSelected m.multiple
      , parallel = S.fromSelected m.parallel
      }

  save st =
    { value: _
    , valueAggregation: _
    , parallel: st.parallel ^. S._value
    , multiple: st.multiple ^. S._value
    }
    <$> (st.value ^. S._value)
    <*> (st.valueAgg ^. S._value)
