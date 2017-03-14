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

module SlamData.Workspace.Card.Setups.Chart.Area.Model where

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
import SlamData.Form.Select as S
import SlamData.Form.Select ((⊝))

type AreaR =
  { dimension ∷ JCursor
  , value ∷ JCursor
  , valueAggregation ∷ Ag.Aggregation
  , series ∷ Maybe JCursor
  , isStacked ∷ Boolean
  , isSmooth ∷ Boolean
  , axisLabelAngle ∷ Number
  }

type Model = Maybe AreaR

initialModel ∷ Model
initialModel = Nothing

eqAreaR ∷ AreaR → AreaR → Boolean
eqAreaR r1 r2 =
  r1.dimension ≡ r2.dimension
  ∧ r1.value ≡ r2.value
  ∧ r1.valueAggregation ≡ r2.valueAggregation
  ∧ r1.isStacked ≡ r2.isStacked
  ∧ r1.isSmooth ≡ r2.isSmooth
  ∧ r1.series ≡ r2.series
  ∧ r1.axisLabelAngle ≡ r2.axisLabelAngle

eqModel ∷ Model → Model → Boolean
eqModel Nothing Nothing = true
eqModel (Just r1) (Just r2) = eqAreaR r1 r2
eqModel _ _ = false

genModel ∷ Gen.Gen Model
genModel = do
  isNothing ← arbitrary
  if isNothing
    then pure Nothing
    else map Just do
    dimension ← map runArbJCursor arbitrary
    value ← map runArbJCursor arbitrary
    valueAggregation ← arbitrary
    isStacked ← arbitrary
    isSmooth ← arbitrary
    series ← map (map runArbJCursor) arbitrary
    axisLabelAngle ← arbitrary
    pure { dimension
         , value
         , valueAggregation
         , isStacked
         , isSmooth
         , series
         , axisLabelAngle
         }


encode ∷ Model → Json
encode Nothing = jsonNull
encode (Just r) =
  "configType" := "area"
  ~> "dimension" := r.dimension
  ~> "value" := r.value
  ~> "valueAggregation" := r.valueAggregation
  ~> "isStacked" := r.isStacked
  ~> "isSmooth" := r.isSmooth
  ~> "series" := r.series
  ~> "axisLabelAngle" := r.axisLabelAngle
  ~> jsonEmptyObject

decode ∷ Json → String ⊹ Model
decode js
  | isNull js = pure Nothing
  | otherwise = map Just do
    obj ← decodeJson js
    configType ← obj .? "configType"
    unless (configType ≡ "area")
      $ throwError "This config is not area"
    dimension ← obj .? "dimension"
    value ← obj .? "value"
    valueAggregation ← obj .? "valueAggregation"
    isStacked ← obj .? "isStacked"
    isSmooth ← obj .? "isSmooth"
    series ← obj .? "series"
    axisLabelAngle ← obj .? "axisLabelAngle"
    pure { dimension
         , value
         , valueAggregation
         , isStacked
         , isSmooth
         , series
         , axisLabelAngle
         }


type ReducedState r =
  { axes ∷ Ax.Axes
  , axisLabelAngle ∷ Number
  , isStacked ∷ Boolean
  , isSmooth ∷ Boolean
  , dimension ∷ S.Select JCursor
  , value ∷ S.Select JCursor
  , valueAgg ∷ S.Select Aggregation
  , series ∷ S.Select JCursor
  | r}

initialState ∷ ReducedState ()
initialState =
  { axes: Ax.initialAxes
  , axisLabelAngle: zero
  , isStacked: false
  , isSmooth: false
  , dimension: S.emptySelect
  , value: S.emptySelect
  , valueAgg: S.emptySelect
  , series: S.emptySelect
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
      newDimension =
        S.setPreviousValueFrom (Just st.dimension)
          $ S.autoSelect
          $ S.newSelect
          $ st.axes.category
          ⊕ st.axes.time
          ⊕ st.axes.value
          ⊕ st.axes.date
          ⊕ st.axes.datetime

      newValue =
        S.setPreviousValueFrom (Just st.value)
          $ S.autoSelect
          $ S.newSelect
          $ st.axes.value

      newValueAggregation =
        S.setPreviousValueFrom (Just st.valueAgg)
          $ nonMaybeAggregationSelect

      newSeries =
        S.setPreviousValueFrom (Just st.series)
          $ S.newSelect
          $ S.ifSelected [ newDimension ]
          $ st.axes.category
          ⊕ st.axes.time
          ⊕ st.axes.date
          ⊕ st.axes.datetime
          ⊝ newDimension
    in
     st{ value = newValue
       , valueAgg = newValueAggregation
       , dimension = newDimension
       , series = newSeries
       }
  load Nothing st = st
  load (Just m) st =
    st{ isStacked = m.isStacked
      , isSmooth = m.isSmooth
      , axisLabelAngle = m.axisLabelAngle
      , value = S.fromSelected $ Just m.value
      , valueAgg = S.fromSelected $ Just m.valueAggregation
      , dimension = S.fromSelected $ Just m.dimension
      , series = S.fromSelected m.series
      }
  save st =
    { dimension: _
    , value: _
    , valueAggregation: _
    , series: st.series ^. S._value
    , isStacked: st.isStacked
    , isSmooth: st.isSmooth
    , axisLabelAngle: st.axisLabelAngle
    }
    <$> (st.dimension ^. S._value)
    <*> (st.value ^. S._value)
    <*> (st.valueAgg ^. S._value)
