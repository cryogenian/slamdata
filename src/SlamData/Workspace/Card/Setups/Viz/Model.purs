module SlamData.Workspace.Card.Setups.Viz.Model where

import SlamData.Prelude

import Data.Array as A
import Data.Argonaut as J
import Data.Argonaut ((~>), (:=), (.?))
import Data.Map as Map
import Data.Newtype (un)
import Data.StrMap as SM
import Data.Foldable as F

import SlamData.Workspace.Card.Setups.Package.Types as T
import SlamData.Workspace.Card.CardType.VizType as VT
import SlamData.Workspace.Card.Setups.Dimension as D
import SlamData.Workspace.Card.Setups.Viz.Auxiliary as Aux

import Test.StrongCheck.Arbitrary (arbitrary)
import Test.StrongCheck.Gen as Gen
import Test.StrongCheck.Data.Argonaut (ArbJCursor(..))

type Model =
  { dimMaps ∷ Map.Map VT.VizType T.DimensionMap
  , vizType ∷ VT.VizType
  , auxes ∷ Map.Map VT.VizType Aux.State
  }

initialModel ∷ Model
initialModel =
  { dimMaps: Map.empty
  , vizType: VT.Chart VT.Pie
  , auxes: Map.empty
  }

eqModel ∷ Model → Model → Boolean
eqModel r1 r2 =
  r1.dimMaps ≡ r2.dimMaps
  ∧ eqAuxes r1.auxes r2.auxes
  ∧ r1.vizType ≡ r2.vizType
  where
  eqAuxes a1 a2 =
    F.and $ A.zipWith (\(k1 × v1) (k2 × v2) → k1 ≡ k2 ∧ Aux.eqState v1 v2)
      ( Map.toUnfoldable a1 )
      ( Map.toUnfoldable a2 )


genModel ∷ Gen.Gen Model
genModel = do
  vizType ← arbitrary
  vizTypes ← arbitrary
  pairs ← A.foldRecM foldMapFn [ ] vizTypes
  aPairs ← A.foldRecM aFoldMapFn [ ] vizTypes
  let
    dimMaps = Map.fromFoldable pairs
    auxes = Map.fromFoldable aPairs
  pure { vizType, dimMaps, auxes }
  where
  foldMapFn
    ∷ Array (VT.VizType × T.DimensionMap)
    → VT.VizType
    → Gen.Gen (Array (VT.VizType × T.DimensionMap))
  foldMapFn arr vt = do
    dm ← genDimMap
    pure $ A.cons (vt × dm) arr

  aFoldMapFn
    ∷ Array (VT.VizType × Aux.State)
    → VT.VizType
    → Gen.Gen (Array (VT.VizType × Aux.State))
  aFoldMapFn arr vt = do
    a ← Aux.genState
    pure $ A.cons (vt × a) arr

  foldFn
    ∷ Array (String × D.LabeledJCursor) → String → Gen.Gen (Array (String × D.LabeledJCursor))
  foldFn arr key = do
    val ← map (map (un ArbJCursor) ∘ un D.DimensionWithStaticCategory) arbitrary
    pure $ A.cons (key × val) arr

  genDimMap ∷ Gen.Gen T.DimensionMap
  genDimMap = do
    keys ← arbitrary
    tpls ← A.foldRecM foldFn [ ] keys
    pure $ SM.fromFoldable tpls


encode ∷ Model → J.Json
encode r =
  "vizType" := r.vizType
  ~> "dimMaps" := r.dimMaps
  ~> "auxes" := map Aux.encodeState r.auxes
  ~> J.jsonEmptyObject

decode ∷ J.Json → String ⊹ Model
decode = J.decodeJson >=> \obj → do
  vizType ← obj .? "vizType"
  dimMaps ← obj .? "dimMaps"
  aux ← traverse Aux.decodeState =<< ((obj .? "auxes") ∷ String ⊹ J.JObject)
  pure { vizType, dimMaps, auxes: Map.empty }
