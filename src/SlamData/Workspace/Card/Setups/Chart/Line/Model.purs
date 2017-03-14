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

module SlamData.Workspace.Card.Setups.Chart.Line.Model where

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


type LineR =
  { dimension ∷ JCursor
  , value ∷ JCursor
  , valueAggregation ∷ Ag.Aggregation
  , secondValue ∷ Maybe JCursor
  , secondValueAggregation ∷ Maybe Ag.Aggregation
  , series ∷ Maybe JCursor
  , size ∷ Maybe JCursor
  , sizeAggregation ∷ Maybe Ag.Aggregation
  , maxSize ∷ Number
  , minSize ∷ Number
  , axisLabelAngle ∷ Number
  , optionalMarkers ∷ Boolean
  }

type Model = Maybe LineR

initialModel ∷ Model
initialModel = Nothing


eqLineR ∷ LineR → LineR → Boolean
eqLineR r1 r2 =
  r1.dimension ≡ r2.dimension
  ∧ r1.value ≡ r2.value
  ∧ r1.valueAggregation ≡ r2.valueAggregation
  ∧ r1.secondValue ≡ r2.secondValue
  ∧ r1.secondValueAggregation ≡ r2.secondValueAggregation
  ∧ r1.series ≡ r2.series
  ∧ r1.size ≡ r2.size
  ∧ r1.sizeAggregation ≡ r2.sizeAggregation
  ∧ r1.maxSize ≡ r2.maxSize
  ∧ r1.minSize ≡ r2.minSize
  ∧ r1.axisLabelAngle ≡ r2.axisLabelAngle
  ∧ r1.optionalMarkers ≡ r2.optionalMarkers

eqModel ∷ Model → Model → Boolean
eqModel Nothing Nothing = true
eqModel (Just r1) (Just r2) = eqLineR r1 r2
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
    secondValue ← map (map runArbJCursor) arbitrary
    secondValueAggregation ← arbitrary
    series ← map (map runArbJCursor) arbitrary
    size ← map (map runArbJCursor) arbitrary
    sizeAggregation ← arbitrary
    maxSize ← arbitrary
    minSize ← arbitrary
    axisLabelAngle ← arbitrary
    optionalMarkers ← arbitrary
    pure { dimension
         , value
         , valueAggregation
         , secondValue
         , secondValueAggregation
         , series
         , size
         , sizeAggregation
         , maxSize
         , minSize
         , axisLabelAngle
         , optionalMarkers
         }

encode ∷ Model → Json
encode Nothing = jsonNull
encode (Just r) =
  "configType" := "line"
  ~> "dimension" := r.dimension
  ~> "value" := r.value
  ~> "valueAggregation" := r.valueAggregation
  ~> "secondValue" := r.secondValue
  ~> "secondValueAggregation" := r.secondValueAggregation
  ~> "series" := r.series
  ~> "size" := r.size
  ~> "sizeAggregation" := r.sizeAggregation
  ~> "maxSize" := r.maxSize
  ~> "minSize" := r.minSize
  ~> "axisLabelAngle" := r.axisLabelAngle
  ~> "optionalMarkers" := r.optionalMarkers
  ~> jsonEmptyObject

decode ∷ Json → String ⊹ Model
decode js
  | isNull js = pure Nothing
  | otherwise = map Just do
    obj ← decodeJson js
    configType ← obj .? "configType"
    unless (configType ≡ "line")
      $ throwError "This config is not line"
    dimension ← obj .? "dimension"
    value ← obj .? "value"
    valueAggregation ← obj .? "valueAggregation"
    secondValue ← obj .? "secondValue"
    secondValueAggregation ← obj .? "secondValueAggregation"
    series ← obj .? "series"
    size ← obj .? "size"
    sizeAggregation ← obj .? "sizeAggregation"
    maxSize ← obj .? "maxSize"
    minSize ← obj .? "minSize"
    axisLabelAngle ← obj .? "axisLabelAngle"
    optionalMarkers ←
      (obj .? "optionalMarkers") <|> (pure false)
    pure { dimension
         , value
         , valueAggregation
         , secondValue
         , secondValueAggregation
         , series
         , size
         , sizeAggregation
         , maxSize
         , minSize
         , axisLabelAngle
         , optionalMarkers
         }


type ReducedState r =
  { axes ∷ Ax.Axes
  , axisLabelAngle ∷ Number
  , minSize ∷ Number
  , maxSize ∷ Number
  , dimension ∷ S.Select JCursor
  , value ∷ S.Select JCursor
  , valueAgg ∷ S.Select Aggregation
  , secondValue ∷ S.Select JCursor
  , secondValueAgg ∷ S.Select Aggregation
  , size ∷ S.Select JCursor
  , sizeAgg ∷ S.Select Aggregation
  , series ∷ S.Select JCursor
  , optionalMarkers ∷ Boolean
  | r }

initialState ∷ ReducedState ()
initialState =
  { axes: Ax.initialAxes
  , axisLabelAngle: zero
  , minSize: 10.0
  , maxSize: 20.0
  , dimension: S.emptySelect
  , value: S.emptySelect
  , valueAgg: S.emptySelect
  , secondValue: S.emptySelect
  , secondValueAgg: S.emptySelect
  , size: S.emptySelect
  , sizeAgg: S.emptySelect
  , series: S.emptySelect
  , optionalMarkers: false
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
          ⊕ st.axes.date
          ⊕ st.axes.datetime
          ⊕ st.axes.value

      newValue =
        S.setPreviousValueFrom (Just st.value)
          $ S.autoSelect
          $ S.newSelect
          $ st.axes.value

      newValueAggregation =
        S.setPreviousValueFrom (Just st.valueAgg)
          $ nonMaybeAggregationSelect

      newSecondValue =
        S.setPreviousValueFrom (Just st.secondValue)
          $ S.newSelect
          $ S.ifSelected [ newValue ]
          $ st.axes.value
          ⊝ newValue

      newSecondValueAggregation =
        S.setPreviousValueFrom (Just st.secondValueAgg)
          $ nonMaybeAggregationSelect

      newSize =
        S.setPreviousValueFrom (Just st.size)
          $ S.newSelect
          $ S.ifSelected [ newValue ]
          $ st.axes.value
          ⊝ newValue
          ⊝ newSecondValue

      newSizeAggregation =
        S.setPreviousValueFrom (Just st.sizeAgg)
          $ nonMaybeAggregationSelect

      newSeries =
        S.setPreviousValueFrom (Just st.series)
          $ S.newSelect
          $ S.ifSelected [ newDimension ]
          $ st.axes.category
          ⊝ newDimension


    in
      st{ dimension = newDimension
        , value = newValue
        , valueAgg = newValueAggregation
        , secondValue = newSecondValue
        , secondValueAgg = newSecondValueAggregation
        , size = newSize
        , sizeAgg = newSizeAggregation
        , series = newSeries
        }

  load Nothing st = st
  load (Just m) st =
    st{ maxSize = m.maxSize
      , minSize = m.minSize
      , optionalMarkers = m.optionalMarkers
      , axisLabelAngle = m.axisLabelAngle
      , value = S.fromSelected $ Just m.value
      , valueAgg = S.fromSelected $ Just m.valueAggregation
      , dimension = S.fromSelected $ Just m.dimension
      , secondValue = S.fromSelected m.secondValue
      , secondValueAgg = S.fromSelected m.secondValueAggregation
      , series = S.fromSelected m.series
      , size = S.fromSelected m.size
      , sizeAgg = S.fromSelected m.sizeAggregation
      }

  save st =
    { dimension: _
    , value: _
    , valueAggregation: _
    , secondValue: st.secondValue ^. S._value
    , secondValueAggregation: st.secondValueAgg ^. S._value
    , size: st.size ^. S._value
    , sizeAggregation: st.sizeAgg ^. S._value
    , series: st.series ^. S._value
    , maxSize: st.maxSize
    , minSize: st.minSize
    , axisLabelAngle: st.axisLabelAngle
    , optionalMarkers: st.optionalMarkers
    }
    <$> (st.dimension ^. S._value)
    <*> (st.value ^. S._value)
    <*> (st.valueAgg ^. S._value)
