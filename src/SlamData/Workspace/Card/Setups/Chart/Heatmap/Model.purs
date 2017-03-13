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

module SlamData.Workspace.Card.Setups.Chart.Heatmap.Model where

import SlamData.Prelude

import Data.Argonaut (JCursor, Json, decodeJson, (~>), (:=), isNull, jsonNull, (.?), jsonEmptyObject)
import Data.Lens ((^.))

import Test.StrongCheck.Arbitrary (arbitrary)
import Test.StrongCheck.Gen as Gen
import Test.StrongCheck.Data.Argonaut (runArbJCursor)

import SlamData.Workspace.Card.Setups.Transform.Aggregation (Aggregation, nonMaybeAggregationSelect)
import SlamData.Workspace.Card.Setups.Behaviour as SB
import SlamData.Workspace.Card.Setups.Axis as Ax
import SlamData.Form.Select ((⊝))
import SlamData.Form.Select as S
import SlamData.Workspace.Card.Setups.Chart.ColorScheme (ColorScheme, colorSchemeSelect)

type HeatmapR =
  { abscissa ∷ JCursor
  , ordinate ∷ JCursor
  , value ∷ JCursor
  , valueAggregation ∷ Aggregation
  , series ∷ Maybe JCursor
  , colorScheme ∷ ColorScheme
  , isColorSchemeReversed ∷ Boolean
  , minValue ∷ Number
  , maxValue ∷ Number
  }

type Model = Maybe HeatmapR

initialModel ∷ Model
initialModel = Nothing


eqHeatmapR ∷ HeatmapR → HeatmapR → Boolean
eqHeatmapR r1 r2 =
  r1.abscissa ≡ r2.abscissa
  ∧ r1.ordinate ≡ r2.ordinate
  ∧ r1.value ≡ r2.value
  ∧ r1.valueAggregation ≡ r2.valueAggregation
  ∧ r1.series ≡ r2.series
  ∧ r1.colorScheme ≡ r2.colorScheme
  ∧ r1.isColorSchemeReversed ≡ r2.isColorSchemeReversed
  ∧ r1.minValue ≡ r2.minValue
  ∧ r1.maxValue ≡ r2.maxValue

eqModel ∷ Model → Model → Boolean
eqModel Nothing Nothing = true
eqModel (Just r1) (Just r2) = eqHeatmapR r1 r2
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
    series ← map (map runArbJCursor) arbitrary
    colorScheme ← arbitrary
    isColorSchemeReversed ← arbitrary
    minValue ← arbitrary
    maxValue ← arbitrary
    pure { abscissa
         , ordinate
         , value
         , valueAggregation
         , series
         , colorScheme
         , isColorSchemeReversed
         , minValue
         , maxValue
         }

encode ∷ Model → Json
encode Nothing = jsonNull
encode (Just r) =
  "configType" := "heatmap"
  ~> "abscissa" := r.abscissa
  ~> "ordinate" := r.ordinate
  ~> "value" := r.value
  ~> "valueAggregation" := r.valueAggregation
  ~> "series" := r.series
  ~> "colorScheme" := r.colorScheme
  ~> "isColorSchemeReversed" := r.isColorSchemeReversed
  ~> "minValue" := r.minValue
  ~> "maxValue" := r.maxValue
  ~> jsonEmptyObject

decode ∷ Json → String ⊹ Model
decode js
  | isNull js = pure Nothing
  | otherwise = map Just do
    obj ← decodeJson js
    configType ← obj .? "configType"
    unless (configType ≡ "heatmap")
      $ throwError "This config is not heatmap"
    abscissa ← obj .? "abscissa"
    ordinate ← obj .? "ordinate"
    value ← obj .? "value"
    valueAggregation ← obj .? "valueAggregation"
    series ← obj .? "series"
    colorScheme ← obj .? "colorScheme"
    isColorSchemeReversed ← obj .? "isColorSchemeReversed"
    minValue ← obj .? "minValue"
    maxValue ← obj .? "maxValue"
    pure { abscissa
         , ordinate
         , value
         , valueAggregation
         , series
         , colorScheme
         , isColorSchemeReversed
         , minValue
         , maxValue
         }


type ReducedState r =
  { axes ∷ Ax.Axes
  , minValue ∷ Number
  , maxValue ∷ Number
  , isSchemeReversed ∷ Boolean
  , abscissa ∷ S.Select JCursor
  , ordinate ∷ S.Select JCursor
  , value ∷ S.Select JCursor
  , valueAgg ∷ S.Select Aggregation
  , series ∷ S.Select JCursor
  , colorScheme ∷ S.Select ColorScheme
  | r}

initialState ∷ ReducedState ()
initialState =
  { axes: Ax.initialAxes
  , minValue: 1.0
  , maxValue: 50.0
  , isSchemeReversed: false
  , abscissa: S.emptySelect
  , ordinate: S.emptySelect
  , value: S.emptySelect
  , valueAgg: S.emptySelect
  , series: S.emptySelect
  , colorScheme: S.emptySelect
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
          $ st.axes.category
          ⊕ st.axes.value
          ⊕ st.axes.time
          ⊕ st.axes.date
          ⊕ st.axes.datetime

      newOrdinate =
        S.setPreviousValueFrom (Just st.ordinate)
          $ S.autoSelect
          $ S.newSelect
          $ st.axes.category
          ⊕ st.axes.value
          ⊕ st.axes.time
          ⊕ st.axes.date
          ⊕ st.axes.datetime
          ⊝ newAbscissa

      newValue =
        S.setPreviousValueFrom (Just st.value)
          $ S.autoSelect
          $ S.newSelect
          $ st.axes.value
          ⊝ newAbscissa
          ⊝ newOrdinate

      newValueAggregation =
        S.setPreviousValueFrom (Just st.valueAgg)
          $ nonMaybeAggregationSelect

      newSeries =
        S.setPreviousValueFrom (Just st.series)
          $ S.newSelect
          $ S.ifSelected [newAbscissa, newOrdinate, newValue]
          $ st.axes.category
          ⊕ st.axes.time
          ⊝ newAbscissa
          ⊝ newOrdinate

      newColorScheme =
        S.setPreviousValueFrom (Just st.colorScheme)
          $ colorSchemeSelect

    in
      st{ abscissa = newAbscissa
        , ordinate = newOrdinate
        , value = newValue
        , valueAgg = newValueAggregation
        , series = newSeries
        , colorScheme = newColorScheme
        }

  load Nothing st = st
  load (Just m) st =
    st{ minValue = m.minValue
      , maxValue = m.maxValue
      , isSchemeReversed = m.isColorSchemeReversed
      , abscissa = S.fromSelected $ Just m.abscissa
      , ordinate = S.fromSelected $ Just m.ordinate
      , value = S.fromSelected $ Just m.value
      , valueAgg = S.fromSelected $ Just m.valueAggregation
      , colorScheme = S.fromSelected $ Just m.colorScheme
      , series = S.fromSelected m.series
      }

  save st =
    { abscissa: _
    , ordinate: _
    , value: _
    , valueAggregation: _
    , series: (st.series ^. S._value)
    , colorScheme: _
    , isColorSchemeReversed: st.isSchemeReversed
    , minValue: st.minValue
    , maxValue: st.maxValue
    }
    <$> (st.abscissa ^. S._value)
    <*> (st.ordinate ^. S._value)
    <*> (st.value ^. S._value)
    <*> (st.valueAgg ^. S._value)
    <*> (st.colorScheme ^. S._value)
