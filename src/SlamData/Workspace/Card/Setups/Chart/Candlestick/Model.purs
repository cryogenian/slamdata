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

module SlamData.Workspace.Card.Setups.Chart.Candlestick.Model where

import SlamData.Prelude

import Data.Argonaut (JObject, Json, decodeJson, (~>), (:=), isNull, jsonNull, (.?), jsonEmptyObject)
import Data.Lens ((^.), view)
import Data.Newtype (un)

import Test.StrongCheck.Arbitrary (arbitrary)
import Test.StrongCheck.Gen as Gen
import Test.StrongCheck.Data.Argonaut (ArbJCursor(..))

import SlamData.Workspace.Card.Setups.Dimension as D
import SlamData.Workspace.Card.Setups.Behaviour as SB
import SlamData.Workspace.Card.Setups.Axis as Ax
import SlamData.Form.Select as S

type ModelR =
  { dimension ∷ D.LabeledJCursor
  , high ∷ D.LabeledJCursor
  , low ∷ D.LabeledJCursor
  , open ∷ D.LabeledJCursor
  , close ∷ D.LabeledJCursor
  , parallel ∷ Maybe D.LabeledJCursor
  }

type Model = Maybe ModelR

initialModel ∷ Maybe ModelR
initialModel = Nothing

eqR ∷ ModelR → ModelR → Boolean
eqR r1 r2 =
  r1.dimension ≡ r2.dimension
  ∧ r1.high ≡ r2.high
  ∧ r1.low ≡ r2.low
  ∧ r1.open ≡ r2.open
  ∧ r1.close ≡ r2.close
  ∧ r1.parallel ≡ r2.parallel

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
    high ← map (map (un ArbJCursor) ∘ un D.DimensionWithStaticCategory) arbitrary
    low ← map (map (un ArbJCursor) ∘ un D.DimensionWithStaticCategory) arbitrary
    open ← map (map (un ArbJCursor) ∘ un D.DimensionWithStaticCategory) arbitrary
    close ← map (map (un ArbJCursor) ∘ un D.DimensionWithStaticCategory) arbitrary
    parallel ← map (map (un ArbJCursor) ∘ un D.DimensionWithStaticCategory) <$> arbitrary

    pure { dimension
         , high
         , low
         , open
         , close
         , parallel
         }

encode ∷ Model → Json
encode Nothing = jsonNull
encode (Just r) =
  "configType" := "candlestick"
  ~> "dimension" := r.dimension
  ~> "high" := r.high
  ~> "low" := r.low
  ~> "open" := r.open
  ~> "close" := r.close
  ~> "parallel" := r.parallel
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
    unless (configType ≡ "candlestick")
      $ throwError "This config is not a candlestick"
    decodeR obj <|> decodeLegacyR obj

  decodeR ∷ JObject → String ⊹ ModelR
  decodeR obj = do
    dimension ← obj .? "dimension"
    high ← obj .? "high"
    low ← obj .? "low"
    open ← obj .? "open"
    close ← obj .? "close"
    parallel ← obj .? "parallel"
    pure { dimension
         , high
         , low
         , close
         , open
         , parallel
         }
  decodeLegacyR ∷ JObject → String ⊹ ModelR
  decodeLegacyR obj = do
    dimension ← map D.defaultJCursorDimension $ obj .? "dimension"
    high ←
      D.pairToDimension
      <$> (obj .? "high")
      <*> (obj .? "highAggregation")
    low ←
      D.pairToDimension
      <$> (obj .? "low")
      <*> (obj .? "lowAggregation")
    open ←
      D.pairToDimension
      <$> (obj .? "open")
      <*> (obj .? "openAggregation")
    close ←
      D.pairToDimension
      <$> (obj .? "close")
      <*> (obj .? "closeAggregation")
    parallel ← map (map D.defaultJCursorDimension) $ obj .? "parallel"
    pure { dimension
         , high
         , low
         , open
         , close
         , parallel
         }

type ReducedState r =
  { axes ∷ Ax.Axes
  , dimension ∷ S.Select D.LabeledJCursor
  , high ∷ S.Select D.LabeledJCursor
  , low ∷ S.Select D.LabeledJCursor
  , open ∷ S.Select D.LabeledJCursor
  , close ∷ S.Select D.LabeledJCursor
  , parallel ∷ S.Select D.LabeledJCursor
  | r }

initialState ∷ ReducedState ()
initialState =
  { axes: Ax.initialAxes
  , dimension: S.emptySelect
  , high: S.emptySelect
  , low: S.emptySelect
  , open: S.emptySelect
  , close: S.emptySelect
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
      setPreviousValueFrom =
        S.setPreviousValueOn $ view $ D._value ∘ D._projection
      except =
        S.exceptOn $ view $ D._value ∘ D._projection

      newOpen =
        setPreviousValueFrom st.open
          $ S.newSelect
          $ map D.defaultJCursorDimension
          $ st.axes.value

      newClose =
        setPreviousValueFrom st.close
          $ S.newSelect
          $ except newOpen
          $ map D.defaultJCursorDimension
          $ st.axes.value

      newHigh =
        setPreviousValueFrom st.high
          $ S.newSelect
          $ except newOpen
          $ except newClose
          $ map D.defaultJCursorDimension
          $ st.axes.value

      newLow =
        setPreviousValueFrom st.low
          $ S.newSelect
          $ except newOpen
          $ except newClose
          $ except newHigh
          $ map D.defaultJCursorDimension
          $ st.axes.value

      newDimension =
        setPreviousValueFrom st.dimension
          $ S.autoSelect
          $ S.newSelect
          $ map D.defaultJCursorDimension
          $ st.axes.category
          ⊕ st.axes.time
          ⊕ st.axes.date
          ⊕ st.axes.datetime

      newParallel =
        setPreviousValueFrom st.parallel
          $ S.newSelect
          $ except newDimension
          $ map D.defaultJCursorDimension
          $ st.axes.category
    in
      st{ open = newOpen
        , close = newClose
        , high = newHigh
        , low = newLow
        , dimension = newDimension
        , parallel = newParallel
        }
  load Nothing st = st
  load (Just m) st =
    st{ dimension = S.fromSelected $ Just m.dimension
      , open = S.fromSelected $ Just m.open
      , close = S.fromSelected $ Just m.close
      , high = S.fromSelected $ Just m.high
      , low = S.fromSelected $ Just m.low
      , parallel = S.fromSelected m.parallel
      }

  save st =
    { dimension: _
    , open: _
    , close: _
    , high: _
    , low: _
    , parallel: st.parallel ^. S._value
    }
    <$> (st.dimension ^. S._value)
    <*> (st.open ^. S._value)
    <*> (st.close ^. S._value)
    <*> (st.high ^. S._value)
    <*> (st.low ^. S._value)
