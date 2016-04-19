{-
Copyright 2015 SlamData, Inc.

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

module Test.SlamData.Property.Notebook.Card.Chart.ChartConfiguration where

import Prelude

import Data.Either (Either(..))
import Data.Foldable (fold)

import SlamData.Notebook.Card.Chart.ChartConfiguration as CC

import Test.Property.ArbJson (runArbJCursor)
import Test.StrongCheck (QC, Result(..), class Arbitrary, arbitrary, quickCheck, (<?>))
import Test.SlamData.Property.Form.Select (runArbSelect)
import Test.SlamData.Property.Notebook.Card.Chart.Aggregation (runArbAggregation)

newtype ArbChartConfiguration = ArbChartConfiguration CC.ChartConfiguration

runArbChartConfiguration :: ArbChartConfiguration -> CC.ChartConfiguration
runArbChartConfiguration (ArbChartConfiguration m) = m

instance arbitraryArbChartConfiguration :: Arbitrary ArbChartConfiguration where
  arbitrary = do
    series <- map (map runArbJCursor <<< runArbSelect) <$> arbitrary
    dimensions <- map (map runArbJCursor <<< runArbSelect) <$> arbitrary
    measures <- map (map runArbJCursor <<< runArbSelect) <$> arbitrary
    aggregations <- map (map runArbAggregation <<< runArbSelect) <$> arbitrary
    pure $ ArbChartConfiguration { series, dimensions, measures, aggregations }

check :: QC Unit
check = quickCheck $ runArbChartConfiguration >>> \cc ->
  case CC.decode (CC.encode cc) of
    Left err -> Failed $ "Decode failed: " ++ err
    Right cc' -> checkChartConfigEquality cc cc'

checkChartConfigEquality :: CC.ChartConfiguration -> CC.ChartConfiguration -> Result
checkChartConfigEquality cc cc' =
  fold
   [ cc.series == cc'.series <?> "series mismatch"
   , cc.dimensions == cc'.dimensions <?> "dimensions mismatch"
   , cc.measures == cc'.measures <?> "measures mismatch"
   , cc.aggregations == cc'.aggregations <?> "aggregations mismatch"
   ]
