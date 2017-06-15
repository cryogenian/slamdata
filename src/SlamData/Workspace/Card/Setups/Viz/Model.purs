module SlamData.Workspace.Card.Setups.Viz.Model where

import SlamData.Prelude

import Data.Array as A
import Data.Argonaut as J
import Data.Argonaut ((~>), (:=), (.?))
import Data.Map as Map
import Data.Newtype (un)
import Data.StrMap as SM

import SlamData.Workspace.Card.Setups.Package.Types as T
import SlamData.Workspace.Card.CardType.VizType as VT
import SlamData.Workspace.Card.Setups.Dimension as D

import Test.StrongCheck.Arbitrary (arbitrary)
import Test.StrongCheck.Gen as Gen
import Test.StrongCheck.Data.Argonaut (ArbJCursor(..))

type Model =
  { dimMaps ∷ Map.Map VT.VizType T.DimensionMap
  , vizType ∷ VT.VizType
  }

initialModel ∷ Model
initialModel =
  { dimMaps: Map.empty
  , vizType: VT.Chart VT.Pie
  }

eqModel ∷ Model → Model → Boolean
eqModel r1 r2 =
  r1.dimMaps ≡ r2.dimMaps
  ∧ r1.vizType ≡ r2.vizType

genModel ∷ Gen.Gen Model
genModel = do
  vizType ← arbitrary
  vizTypes ← arbitrary
  pairs ← A.foldRecM foldMapFn [ ] vizTypes
  let dimMaps = Map.fromFoldable pairs
  pure { vizType, dimMaps }
  where
  foldMapFn
    ∷ Array (VT.VizType × T.DimensionMap) → VT.VizType → Gen.Gen (Array (VT.VizType × T.DimensionMap))
  foldMapFn arr vt = do
    dm ← genDimMap
    pure $ A.cons (vt × dm) arr

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
  ~> J.jsonEmptyObject

decode ∷ J.Json → String ⊹ Model
decode = J.decodeJson >=> \obj → do
  vizType ← obj .? "vizType"
  dimMaps ← obj .? "dimMaps"
  pure { vizType, dimMaps }
