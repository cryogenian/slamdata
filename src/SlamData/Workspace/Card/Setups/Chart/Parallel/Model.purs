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

module SlamData.Workspace.Card.Setups.Chart.Parallel.Model where

import SlamData.Prelude

import Data.Argonaut as J
import Data.Argonaut ((~>), (:=), (.?))
import Data.Newtype (un)

import SlamData.Workspace.Card.Setups.Dimension as D

import Test.StrongCheck.Arbitrary (arbitrary)
import Test.StrongCheck.Gen as Gen
import Test.StrongCheck.Data.Argonaut (ArbJCursor(..))

type ModelR =
  { dims ∷ Array D.LabeledJCursor
  , series ∷ D.LabeledJCursor
  }

type Model = Maybe ModelR

initialModel ∷ Model
initialModel = Nothing

eqR ∷ ModelR → ModelR → Boolean
eqR r1 r2 =
  r1.dims ≡ r2.dims
  ∧ r1.series ≡ r2.series

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
    dims ← map (map (un ArbJCursor) ∘ un D.DimensionWithStaticCategory) <$> arbitrary
    series ← map (map (un ArbJCursor) ∘ un D.DimensionWithStaticCategory) arbitrary
    pure { dims, series }


encode ∷ Model → J.Json
encode Nothing = J.jsonNull
encode (Just r) =
  "configType" := "parallel"
  ~> "dims" := r.dims
  ~> "series" := r.series
  ~> J.jsonEmptyObject

decode ∷ J.Json → String ⊹ Model
decode js
  | J.isNull js = pure Nothing
  | otherwise = map Just do
    obj ← J.decodeJson js
    configType ← obj .? "configType"
    unless (configType ≡ "parallel")
      $ throwError "This config is not parallel"
    decodeR obj <|> decodeLegacyR obj
  where
  decodeR ∷ J.JObject → String ⊹ ModelR
  decodeR obj = do
    dims ← obj .? "dims"
    series ← obj .? "series"
    pure { dims, series }

  decodeLegacyR ∷ J.JObject → String ⊹ ModelR
  decodeLegacyR obj = do
    series ← map D.defaultJCursorDimension $ obj .? "series"
    dprs ← obj .? "dims"
    aggs ← obj .? "aggs"
    let dims = D.pairToDimension <$> dprs <*> aggs
    pure { dims, series }
{-
type ReducedState r =
  { axes ∷ Ax.Axes
  , dims ∷ Array (S.Select JCursor)
  , aggs ∷ Array (S.Select Aggregation)
  , series ∷ S.Select JCursor
  | r}

initialState ∷ ReducedState ()
initialState =
  { axes: Ax.initialAxes
  , dims: [ S.emptySelect ]
  , aggs: [ S.emptySelect ]
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
      newSeries =
        S.setPreviousValueFrom (Just st.series)
          $ S.newSelect
          $ st.axes.category
          ⊕ st.axes.value
          ⊕ st.axes.date
          ⊕ st.axes.time
          ⊕ st.axes.datetime

      newDimensions = foldl dimsFoldFn [ ] st.dims

      dimsFoldFn acc dim =
        let
          newDim =
            S.setPreviousValueFrom (Just dim)
              $ S.newSelect
              $ S.ifSelected acc
              $ (\res → foldl (\b a → b ⊝ a) res acc)
              $ st.axes.value
        in A.snoc acc newDim

      newAggregations = st.aggs <#> \agg →
        S.setPreviousValueFrom (Just agg)
          nonMaybeAggregationSelect
    in
      st{ dims = newDimensions
        , aggs = newAggregations
        , series = newSeries
        }

  load Nothing st = st
  load (Just m) st =
    st{ dims = (map (S.fromSelected ∘ Just) m.dims) ⊕ [ S.emptySelect ]
      , aggs = (map (S.fromSelected ∘ Just) m.aggs) ⊕ [ S.emptySelect ]
      , series = S.fromSelected $ Just m.series
      }

  save st = do
    st.dims A.!! 1
    st.aggs A.!! 1
    series ← st.series ^. S._value
    pure { dims: A.catMaybes $ map (_ ^. S._value) st.dims
         , aggs: A.catMaybes $ map (_ ^. S._value) st.aggs
         , series
         }
-}
