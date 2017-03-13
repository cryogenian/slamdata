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

module SlamData.Workspace.Card.Setups.Chart.Funnel.Model where

import SlamData.Prelude

import Data.Argonaut (JCursor, Json, decodeJson, (~>), (:=), isNull, jsonNull, (.?), jsonEmptyObject)
import Data.Lens ((^.))

import Test.StrongCheck.Arbitrary (arbitrary)
import Test.StrongCheck.Gen as Gen
import Test.StrongCheck.Data.Argonaut (runArbJCursor)

import SlamData.Common.Align (Align, alignSelect)
import SlamData.Common.Sort (Sort, sortSelect)
import SlamData.Form.Select ((⊝))
import SlamData.Form.Select as S
import SlamData.Workspace.Card.Setups.Axis as Ax
import SlamData.Workspace.Card.Setups.Behaviour as SB
import SlamData.Workspace.Card.Setups.Transform.Aggregation (Aggregation, nonMaybeAggregationSelect)

type FunnelR =
  { category ∷ JCursor
  , value ∷ JCursor
  , valueAggregation ∷ Aggregation
  , series ∷ Maybe JCursor
  , order ∷ Sort
  , align ∷ Align
  }

type Model = Maybe FunnelR

initialModel ∷ Model
initialModel = Nothing


eqFunnelR ∷ FunnelR → FunnelR → Boolean
eqFunnelR r1 r2 =
  r1.category ≡ r2.category
  ∧ r1.value ≡ r2.value
  ∧ r1.valueAggregation ≡ r2.valueAggregation
  ∧ r1.series ≡ r2.series
  ∧ r1.order ≡ r2.order
  ∧ r1.align ≡ r2.align

eqModel ∷ Model → Model → Boolean
eqModel Nothing Nothing = true
eqModel (Just r1) (Just r2) = eqFunnelR r1 r2
eqModel _ _ = false

genModel ∷ Gen.Gen Model
genModel = do
  isNothing ← arbitrary
  if isNothing
    then pure Nothing
    else map Just do
    category ← map runArbJCursor arbitrary
    value ← map runArbJCursor arbitrary
    valueAggregation ← arbitrary
    series ← map (map runArbJCursor) arbitrary
    order ← arbitrary
    align ← arbitrary
    pure { category, value, valueAggregation, series, order, align }

encode ∷ Model → Json
encode Nothing = jsonNull
encode (Just r) =
  "configType" := "funnel"
  ~> "category" := r.category
  ~> "value" := r.value
  ~> "valueAggregation" := r.valueAggregation
  ~> "series" := r.series
  ~> "order" := r.order
  ~> "align" := r.align
  ~> jsonEmptyObject

decode ∷ Json → String ⊹ Model
decode js
  | isNull js = pure Nothing
  | otherwise = map Just do
    obj ← decodeJson js
    configType ← obj .? "configType"
    unless (configType ≡ "funnel")
      $ throwError "This config is not funnel"
    category ← obj .? "category"
    value ← obj .? "value"
    valueAggregation ← obj .? "valueAggregation"
    series ← obj .? "series"
    order ← obj .? "order"
    align ← obj .? "align"
    pure { category, value, valueAggregation, series, order, align }

type ReducedState r =
  { axes ∷ Ax.Axes
  , category ∷ S.Select JCursor
  , value ∷ S.Select JCursor
  , valueAgg ∷ S.Select Aggregation
  , series ∷ S.Select JCursor
  , align ∷ S.Select Align
  , order ∷ S.Select Sort
  | r}

initialState ∷ ReducedState ()
initialState =
  { axes:Ax.initialAxes
  , category: S.emptySelect
  , value: S.emptySelect
  , valueAgg: S.emptySelect
  , series: S.emptySelect
  , align: S.emptySelect
  , order: S.emptySelect
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
      newCategory =
        S.setPreviousValueFrom (Just st.category)
          $ S.autoSelect
          $ S.newSelect
          $ st.axes.category
          ⊕ st.axes.time
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
          $ S.ifSelected [ newCategory ]
          $ st.axes.category
          ⊕ st.axes.time
          ⊝ newCategory

      newOrder =
        S.setPreviousValueFrom (Just st.order)
          $ sortSelect

      newAlign =
        S.setPreviousValueFrom (Just st.align)
          $ alignSelect
    in
     st{ value = newValue
       , valueAgg = newValueAggregation
       , category = newCategory
       , series = newSeries
       , align = newAlign
       , order = newOrder
       }

  load Nothing st = st
  load (Just m) st =
    st{ value = S.fromSelected $ Just m.value
      , valueAgg = S.fromSelected $ Just m.valueAggregation
      , category = S.fromSelected $ Just m.category
      , series = S.fromSelected m.series
      , align = S.fromSelected $ Just m.align
      , order = S.fromSelected $ Just m.order
      }

  save st =
    { category: _
    , value: _
    , valueAggregation: _
    , series: st.series ^. S._value
    , order: _
    , align: _
    }
    <$> (st.category ^. S._value)
    <*> (st.value ^. S._value)
    <*> (st.valueAgg ^. S._value)
    <*> (st.order ^. S._value)
    <*> (st.align ^. S._value)
