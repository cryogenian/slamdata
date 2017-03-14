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

module SlamData.Workspace.Card.Setups.Chart.Graph.Model where

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

type GraphR =
  { source ∷ JCursor
  , target ∷ JCursor
  , size ∷ Maybe JCursor
  , color ∷ Maybe JCursor
  , minSize ∷ Number
  , maxSize ∷ Number
  , circular ∷ Boolean
  , sizeAggregation ∷ Maybe Ag.Aggregation
  }

type Model = Maybe GraphR

initialModel ∷ Model
initialModel = Nothing

eqGraphR ∷ GraphR → GraphR → Boolean
eqGraphR r1 r2 =
  r1.source ≡ r2.source
  ∧ r1.target ≡ r2.target
  ∧ r1.size ≡ r2.size
  ∧ r1.color ≡ r2.color
  ∧ r1.minSize ≡ r2.minSize
  ∧ r1.maxSize ≡ r2.maxSize
  ∧ r1.circular ≡ r2.circular
  ∧ r1.sizeAggregation ≡ r2.sizeAggregation

eqModel ∷ Model → Model → Boolean
eqModel Nothing Nothing = true
eqModel (Just r1) (Just r2) = eqGraphR r1 r2
eqModel _ _ = false

genModel ∷ Gen.Gen Model
genModel = do
  isNothing ← arbitrary
  if isNothing
    then pure Nothing
    else map Just do
    source ← map runArbJCursor arbitrary
    target ← map runArbJCursor arbitrary
    size ← map (map runArbJCursor) arbitrary
    color ← map (map runArbJCursor) arbitrary
    minSize ← arbitrary
    maxSize ← arbitrary
    circular ← arbitrary
    sizeAggregation ← arbitrary
    pure { source
         , target
         , size
         , color
         , minSize
         , maxSize
         , circular
         , sizeAggregation
         }

encode ∷ Model → Json
encode Nothing = jsonNull
encode (Just r) =
  "configType" := "graph"
  ~> "source" := r.source
  ~> "target" := r.target
  ~> "size" := r.size
  ~> "color" := r.color
  ~> "minSize" := r.minSize
  ~> "maxSize" := r.maxSize
  ~> "circular" := r.circular
  ~> "sizeAggregation" := r.sizeAggregation
  ~> jsonEmptyObject

decode ∷ Json → String ⊹ Model
decode js
  | isNull js = pure Nothing
  | otherwise = map Just do
    obj ← decodeJson js
    configType ← obj .? "configType"
    unless (configType ≡ "graph")
      $ throwError "This config is not graph"
    source ← obj .? "source"
    target ← obj .? "target"
    size ← obj .? "size"
    color ← obj .? "color"
    minSize ← obj .? "minSize"
    maxSize ← obj .? "maxSize"
    circular ← obj .? "circular"
    sizeAggregation ← obj .? "sizeAggregation"
    pure  { source
          , target
          , size
          , color
          , minSize
          , maxSize
          , circular
          , sizeAggregation
          }

type ReducedState r =
  { axes ∷ Ax.Axes
  , circular ∷ Boolean
  , maxSize ∷ Number
  , minSize ∷ Number
  , source ∷ S.Select JCursor
  , target ∷ S.Select JCursor
  , size ∷ S.Select JCursor
  , sizeAgg ∷ S.Select Aggregation
  , color ∷ S.Select JCursor
  | r}

initialState ∷ ReducedState ()
initialState =
  { axes: Ax.initialAxes
  , maxSize: 50.0
  , minSize: 1.0
  , circular: false
  , source: S.emptySelect
  , target: S.emptySelect
  , size: S.emptySelect
  , sizeAgg: S.emptySelect
  , color: S.emptySelect
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
          $ S.ifSelected [newSource]
          $ st.axes.category
          ⊝ newSource

      newSize =
        S.setPreviousValueFrom (Just st.size)
          $ S.newSelect
          $ S.ifSelected [newTarget]
          $ st.axes.value

      newColor =
        S.setPreviousValueFrom (Just st.color)
          $ S.newSelect
          $ S.ifSelected [newTarget]
          $ st.axes.category
          ⊕ st.axes.time
          ⊝ newSource
          ⊝ newTarget

      newSizeAggregation =
        S.setPreviousValueFrom (Just st.sizeAgg)
          $ nonMaybeAggregationSelect
    in
      st{ source = newSource
        , target = newTarget
        , size = newSize
        , sizeAgg = newSizeAggregation
        , color = newColor
        }

  load Nothing st = st
  load (Just m) st =
    st{ source = S.fromSelected $ Just m.source
      , target = S.fromSelected $ Just m.target
      , size = S.fromSelected m.size
      , sizeAgg = S.fromSelected m.sizeAggregation
      , color = S.fromSelected m.color
      }

  save st =
    { source: _
    , target: _
    , size: st.size ^. S._value
    , color: st.color ^. S._value
    , sizeAggregation: st.sizeAgg ^. S._value
    , minSize: st.minSize
    , maxSize: st.maxSize
    , circular: st.circular
    }
    <$> (st.source ^. S._value)
    <*> (st.target ^. S._value)
