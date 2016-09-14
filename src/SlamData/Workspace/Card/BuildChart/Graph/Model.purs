module SlamData.Workspace.Card.BuildChart.Graph.Model where

import SlamData.Prelude

import Data.Argonaut (JArray, JCursor, Json, decodeJson, cursorGet, toNumber, toString, (~>), (:=), isNull, jsonNull, (.?), jsonEmptyObject)
import Data.Array as A
import Data.Foldable as F
import Data.Map as M

import SlamData.Workspace.Card.CardType.ChartType (ChartType(..))
import SlamData.Workspace.Card.Chart.Axis as Ax
import SlamData.Workspace.Card.Chart.Aggregation as Ag

import Test.StrongCheck.Arbitrary (class Arbitrary, arbitrary)
import Test.StrongCheck.Gen as Gen
import Test.Property.ArbJson (runArbJCursor)

type GraphR =
  { source ∷ JCursor
  , target ∷ JCursor
  , size ∷ Maybe JCursor
  , color ∷ Maybe JCursor
  , minSize ∷ Number
  , maxSize ∷ Number
  , circular ∷ Boolean
  , axes ∷ Ax.Axes
  , sizeAggregation ∷ Maybe Ag.Aggregation
  }

type Model = Maybe GraphR

initialModel ∷ Model
initialModel = Nothing

eqGraphR ∷ GraphR → GraphR → Boolean
eqGraphR r1 r2 =
  F.and
    [ Ax.eqAxes r1.axes r2.axes
    , r1.source ≡ r2.source
    , r1.target ≡ r2.target
    , r1.size ≡ r2.size
    , r1.color ≡ r2.color
    , r1.minSize ≡ r2.minSize
    , r1.maxSize ≡ r2.maxSize
    , r1.circular ≡ r2.circular
    , r1.sizeAggregation ≡ r2.sizeAggregation
    ]

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
    axes ← Ax.genAxes
    pure { source
         , target
         , size
         , color
         , minSize
         , maxSize
         , circular
         , axes
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
  ~> "axes" := Ax.encodeAxes r.axes
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
    jsAxes ← obj .? "axes"
    axes ← Ax.decodeAxes jsAxes
    pure  { source
          , target
          , size
          , color
          , minSize
          , maxSize
          , circular
          , axes
          , sizeAggregation
          }
