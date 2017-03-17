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

import Data.Argonaut (JObject, Json, decodeJson, (~>), (:=), isNull, jsonNull, (.?), jsonEmptyObject)
import Data.Lens (view, (^.))
import Data.Newtype (un)

import Test.StrongCheck.Arbitrary (arbitrary)
import Test.StrongCheck.Gen as Gen
import Test.StrongCheck.Data.Argonaut (ArbJCursor(..))

import SlamData.Workspace.Card.Setups.Behaviour as SB
import SlamData.Workspace.Card.Setups.Axis as Ax
import SlamData.Form.Select as S
import SlamData.Workspace.Card.Setups.Dimension as D

type ModelR =
  { dimension ∷ D.LabeledJCursor
  , value ∷ D.LabeledJCursor
  , series ∷ Maybe D.LabeledJCursor
  , isStacked ∷ Boolean
  , isSmooth ∷ Boolean
  , axisLabelAngle ∷ Number
  }

type Model = Maybe ModelR

initialModel ∷ Model
initialModel = Nothing

eqR ∷ ModelR → ModelR → Boolean
eqR r1 r2 =
  r1.dimension ≡ r2.dimension
  ∧ r1.value ≡ r2.value
  ∧ r1.isStacked ≡ r2.isStacked
  ∧ r1.isSmooth ≡ r2.isSmooth
  ∧ r1.series ≡ r2.series
  ∧ r1.axisLabelAngle ≡ r2.axisLabelAngle

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
    isStacked ← arbitrary
    isSmooth ← arbitrary

    axisLabelAngle ← arbitrary
    pure { dimension
         , value
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
  ~> "isStacked" := r.isStacked
  ~> "isSmooth" := r.isSmooth
  ~> "series" := r.series
  ~> "axisLabelAngle" := r.axisLabelAngle
  ~> jsonEmptyObject

decode ∷ Json → String ⊹ Model
decode js
  | isNull js = pure Nothing
  | otherwise = map Just $ decode' js
  where
  decode' ∷ Json → String ⊹ ModelR
  decode' js' = do
    obj ← decodeJson js'
    configType ← obj .? "configType"
    unless (configType ≡ "area")
      $ throwError "This config is not area"
    decodeR obj <|> decodeLegacyR obj

  decodeR ∷ JObject → String ⊹ ModelR
  decodeR obj = do
    dimension ← obj .? "dimension"
    value ← obj .? "value"
    series ← obj .? "series"
    isStacked ← obj .? "isStacked"
    isSmooth ← obj .? "isSmooth"
    axisLabelAngle ← obj .? "axisLabelAngle"

    pure { dimension
         , value
         , isStacked
         , isSmooth
         , series
         , axisLabelAngle
         }
  decodeLegacyR ∷ JObject → String ⊹ ModelR
  decodeLegacyR obj = do
    dimension ← map D.defaultJCursorDimension $ obj .? "dimension"
    value ←
      D.pairToDimension
      <$> (obj .? "value")
      <*> (obj .? "valueAggregation")
    series ← map (map D.defaultJCursorDimension) $ obj .? "series"
    isStacked ← obj .? "isStacked"
    isSmooth ← obj .? "isSmooth"
    axisLabelAngle ← obj .? "axisLabelAngle"

    pure { dimension
         , value
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
  , dimension ∷ S.Select D.LabeledJCursor
  , value ∷ S.Select D.LabeledJCursor
  , series ∷ S.Select D.LabeledJCursor
  | r}

initialState ∷ ReducedState ()
initialState =
  { axes: Ax.initialAxes
  , axisLabelAngle: zero
  , isStacked: false
  , isSmooth: false
  , dimension: S.emptySelect
  , value: S.emptySelect
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
          ⊕ st.axes.value
          ⊕ st.axes.date
          ⊕ st.axes.datetime

      newValue =
        setPreviousValueFrom st.value
          $ S.autoSelect
          $ S.newSelect
          $ except newDimension
          $ map D.defaultJCursorDimension
          $ st.axes.value

      newSeries =
        setPreviousValueFrom st.series
          $ S.newSelect
          $ except newDimension
          $ except newValue
          $ map D.defaultJCursorDimension
          $ S.ifSelected [ newDimension ]
          $ st.axes.category
          ⊕ st.axes.time
          ⊕ st.axes.date
          ⊕ st.axes.datetime
    in
     st{ value = newValue
       , dimension = newDimension
       , series = newSeries
       }
  load Nothing st = st
  load (Just m) st =
    st{ isStacked = m.isStacked
      , isSmooth = m.isSmooth
      , axisLabelAngle = m.axisLabelAngle
      , value = S.fromSelected $ Just m.value
      , dimension = S.fromSelected $ Just m.dimension
      , series = S.fromSelected m.series
      }
  save st =
    { dimension: _
    , value: _
    , series: st.series ^. S._value
    , isStacked: st.isStacked
    , isSmooth: st.isSmooth
    , axisLabelAngle: st.axisLabelAngle
    }
    <$> (st.dimension ^. S._value)
    <*> (st.value ^. S._value)
