module Test.Property.ArbAggregation where

import SlamData.Prelude

import SlamData.Workspace.Card.BuildChart.Aggregation (Aggregation, allAggregations)

import Test.StrongCheck.Arbitrary (class Arbitrary)
import Test.StrongCheck.Gen (allInArray)

newtype ArbAggregation = ArbAggregation (Maybe Aggregation)

runArbAggregation ∷ ArbAggregation → (Maybe Aggregation)
runArbAggregation (ArbAggregation m) = m

instance arbitraryArbAggregation ∷ Arbitrary ArbAggregation where
  arbitrary = ArbAggregation <$> allInArray (map Just allAggregations)
