module SlamData.Workspace.Card.Viz.Model where

import SlamData.Prelude

import Data.Argonaut ((~>), (:=), (.?))
import Data.Argonaut as J
import Data.Variant as V
import Data.Codec as C
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Variant as CAV

import Test.StrongCheck.Arbitrary (class Arbitrary, arbitrary)
import Test.StrongCheck.Gen as Gen
import SlamData.Workspace.Card.Viz.Renderer.PivotTable.Model as PM
import SlamData.Workspace.Card.Viz.Renderer.Select.Model as SM
import SlamData.Workspace.Card.Viz.Renderer.Input.Model as IM

import Test.StrongCheck.Gen as Gen

type Model = Variant
  ( pivot ∷ PM.Model
  , select ∷ SM.Model
  , input ∷ IM.Model
  , static ∷ Unit
  , metric ∷ Unit
  , chart ∷ Unit
  )

_pivot = SProxy ∷ SProxy "pivot"
_select = SProxy ∷ SProxy "select"
_input = SProxy ∷ SProxy "input"
_static = SProxy ∷ SProxy "static"
_metric = SProxy ∷ SProxy "metric"
_chart = SProxy ∷ SProxy "chart"

eq_ ∷ Model → Model → Boolean
eq_ r1 = V.default false
  # V.on _pivot (\r2 → V.on _pivot (PM.eq_ r2) ff r1)
  # V.on _select (\r2 → V.on _select (SM.eq_ r2) ff r1)
  # V.on _input (\r2 → V.on _input (IM.eq_ r2) ff r1)
  # V.on _static (V.on _static tt ff r1)
  # V.on _metric (V.on _metric tt ff r1)
  # V.on _chart (V.on _chart tt ff r1)

gen ∷ Gen.Gen Model
gen = Gen.oneOf (pure $ V.inj _static unit)
  [ pure $ V.inj _chart unit
  , pure $ V.inj _metric unit
  , map (V.inj _pivot) PM.gen
  , map (V.inj _select) SM.gen
  , map (V.inj _input) IM.gen
  ]

empty ∷ Model
empty = V.inj _chart unit

codec ∷ CA.JsonCodec Model
codec = CAV.variant
  # CAV.variantCase _pivot (Right PM.codec)
  # CAV.variantCase _select (Right SM.codec)
  # CAV.variantCase _input (Right IM.codec)
  # CAV.variantCase _static (Left unit)
  # CAV.variantCase _metric (Left unit)
  # CAV.variantCase _chart (Left unit)
