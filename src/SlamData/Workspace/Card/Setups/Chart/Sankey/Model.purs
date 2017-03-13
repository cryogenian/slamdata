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

module SlamData.Workspace.Card.Setups.Chart.Sankey.Model where

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

type SankeyR =
  { source ∷ JCursor
  , target ∷ JCursor
  , value ∷ JCursor
  , valueAggregation ∷ Ag.Aggregation
  }

type Model = Maybe SankeyR

initialModel ∷ Model
initialModel = Nothing

eqSankeyR ∷ SankeyR → SankeyR → Boolean
eqSankeyR r1 r2 =
  r1.source ≡ r2.source
  ∧ r1.target ≡ r2.target
  ∧ r1.value ≡ r2.value
  ∧ r1.valueAggregation ≡ r2.valueAggregation

eqModel ∷ Model → Model → Boolean
eqModel Nothing Nothing = true
eqModel (Just r1) (Just r2) = eqSankeyR r1 r2
eqModel _ _ = false

genModel ∷ Gen.Gen Model
genModel = do
  isNothing ← arbitrary
  if isNothing
    then pure Nothing
    else do
    source ← map runArbJCursor arbitrary
    target ← map runArbJCursor arbitrary
    value ← map runArbJCursor arbitrary
    valueAggregation ← arbitrary
    pure $ Just { source, target, value, valueAggregation }


encode ∷ Model → Json
encode Nothing = jsonNull
encode (Just r) =
  "configType" := "sankey"
  ~> "source" := r.source
  ~> "target" := r.target
  ~> "value" := r.value
  ~> "valueAggregation" := r.valueAggregation
  ~> jsonEmptyObject

decode ∷ Json → String ⊹ Model
decode js
  | isNull js = pure Nothing
  | otherwise = do
    obj ← decodeJson js
    configType ← obj .? "configType"
    unless (configType ≡ "sankey")
      $ throwError "This config is not sankey"
    source ← obj .? "source"
    target ← obj .? "target"
    value ← obj .? "value"
    valueAggregation ← obj .? "valueAggregation"
    pure $ Just { source, target, value, valueAggregation }

type ReducedState r =
  { axes ∷ Ax.Axes
  , source ∷ S.Select JCursor
  , target ∷ S.Select JCursor
  , value ∷ S.Select JCursor
  , valueAgg ∷ S.Select Aggregation
  | r }

initialState ∷ ReducedState ()
initialState =
  { axes: Ax.initialAxes
  , source: S.emptySelect
  , target: S.emptySelect
  , value: S.emptySelect
  , valueAgg: S.emptySelect
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
      newSource =
        S.setPreviousValueFrom (Just st.source)
          $ S.autoSelect
          $ S.newSelect
          $ st.axes.category

      newTarget =
        S.setPreviousValueFrom (Just st.target)
          $ S.autoSelect
          $ S.newSelect
          $ S.ifSelected [ newSource ]
          $ st.axes.category
          ⊝ newSource

      newValue =
        S.setPreviousValueFrom (Just st.value)
          $ S.autoSelect
          $ S.newSelect
          $ S.ifSelected [newTarget]
          $ st.axes.value

      newValueAggregation =
        S.setPreviousValueFrom (Just st.valueAgg)
          $ nonMaybeAggregationSelect
    in
      st{ source = newSource
        , target = newTarget
        , value = newValue
        , valueAgg = newValueAggregation
        }

  load Nothing st = st
  load (Just m) st =
    st{ source = S.fromSelected $ Just m.source
      , target = S.fromSelected $ Just m.target
      , value = S.fromSelected $ Just m.value
      , valueAgg = S.fromSelected $ Just m.valueAggregation
      }

  save st =
    { source: _
    , target: _
    , value: _
    , valueAggregation: _
    }
    <$> (st.source ^. S._value)
    <*> (st.target ^. S._value)
    <*> (st.value ^.  S._value)
    <*> (st.valueAgg ^. S._value)
