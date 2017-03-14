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

module SlamData.Workspace.Card.Setups.Chart.PunchCard.Model where

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

type PunchCardR =
  { abscissa ∷ JCursor
  , ordinate ∷ JCursor
  , value ∷ JCursor
  , valueAggregation ∷ Ag.Aggregation
  , circular ∷ Boolean
  , minSize ∷ Number
  , maxSize ∷ Number
  }

type Model = Maybe PunchCardR

initialModel ∷ Model
initialModel = Nothing

eqPunchCardR ∷ PunchCardR → PunchCardR → Boolean
eqPunchCardR r1 r2 =
  r1.abscissa ≡ r2.abscissa
  ∧ r1.ordinate ≡ r2.ordinate
  ∧ r1.valueAggregation ≡ r2.valueAggregation
  ∧ r1.value ≡ r2.value
  ∧ r1.circular ≡ r2.circular
  ∧ r1.minSize ≡ r2.minSize
  ∧ r1.maxSize ≡ r2.maxSize

eqModel ∷ Model → Model → Boolean
eqModel Nothing Nothing = true
eqModel (Just r1) (Just r2) = eqPunchCardR r1 r2
eqModel _ _ = false

genModel ∷ Gen.Gen Model
genModel = do
  isNothing ← arbitrary
  if isNothing
    then pure Nothing
    else map Just do
    abscissa ← map runArbJCursor arbitrary
    ordinate ← map runArbJCursor arbitrary
    value ← map runArbJCursor arbitrary
    valueAggregation ← arbitrary
    circular ← arbitrary
    minSize ← arbitrary
    maxSize ← arbitrary
    pure { abscissa
         , ordinate
         , value
         , valueAggregation
         , circular
         , minSize
         , maxSize
         }

encode ∷ Model → Json
encode Nothing = jsonNull
encode (Just r) =
  "configType" := "punch-card"
  ~> "abscissa" := r.abscissa
  ~> "ordinate" := r.ordinate
  ~> "value" := r.value
  ~> "valueAggregation" := r.valueAggregation
  ~> "circular" := r.circular
  ~> "minSize" := r.minSize
  ~> "maxSize" := r.maxSize
  ~> jsonEmptyObject

decode ∷ Json → String ⊹ Model
decode js
  | isNull js = pure Nothing
  | otherwise = map Just do
    obj ← decodeJson js
    configType ← obj .? "configType"
    unless (configType ≡ "punch-card")
      $ throwError "This config is not punch card"
    abscissa ← obj .? "abscissa"
    ordinate ← obj .? "ordinate"
    value ← obj .? "value"
    valueAggregation ← obj .? "valueAggregation"
    circular ← obj .? "circular"
    minSize ← obj .? "minSize"
    maxSize ← obj .? "maxSize"
    pure { abscissa, ordinate, value, valueAggregation, circular, minSize, maxSize }


type ReducedState r =
  { axes ∷ Ax.Axes
  , circular ∷ Boolean
  , abscissa ∷ S.Select JCursor
  , ordinate ∷ S.Select JCursor
  , value ∷ S.Select JCursor
  , valueAgg ∷ S.Select Aggregation
  , minSize ∷ Number
  , maxSize ∷ Number
  | r}

initialState ∷ ReducedState ()
initialState =
  { axes: Ax.initialAxes
  , circular: false
  , abscissa: S.emptySelect
  , ordinate: S.emptySelect
  , value: S.emptySelect
  , valueAgg: S.emptySelect
  , minSize: 10.0
  , maxSize: 50.0
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
      newAbscissa =
        S.setPreviousValueFrom (Just st.abscissa)
          $ S.autoSelect
          $ S.newSelect
          -- date and time may not be continuous, but datetime is definitely continuous axis
          -- that's why it's not here
          $ st.axes.category
          ⊕ st.axes.time
          ⊕ st.axes.date

      newOrdinate =
        S.setPreviousValueFrom (Just st.ordinate)
          $ S.autoSelect
          $ S.newSelect
          $ S.ifSelected [ newAbscissa ]
          $ st.axes.category
          ⊕ st.axes.time
          ⊕ st.axes.date

      newValue =
        S.setPreviousValueFrom (Just st.value)
          $ S.autoSelect
          $ S.newSelect
          $ st.axes.value

      newValueAgg =
        S.setPreviousValueFrom (Just st.valueAgg)
          $ nonMaybeAggregationSelect


    in
      st{ abscissa = newAbscissa
        , ordinate = newOrdinate
        , value = newValue
        , valueAgg = newValueAgg
        }

  load Nothing st = st
  load (Just m) st =
    st{ abscissa = S.fromSelected $ Just m.abscissa
      , ordinate = S.fromSelected $ Just m.ordinate
      , value = S.fromSelected $ Just m.value
      , valueAgg = S.fromSelected $ Just m.valueAggregation
      , circular = m.circular
      , minSize = m.minSize
      , maxSize = m.maxSize
      }

  save st =
    { abscissa: _
    , ordinate: _
    , value: _
    , valueAggregation: _
    , circular: st.circular
    , minSize: st.minSize
    , maxSize: st.maxSize
    }
    <$> (st.abscissa ^. S._value)
    <*> (st.ordinate ^. S._value)
    <*> (st.value ^. S._value)
    <*> (st.valueAgg ^. S._value)
