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

module Test.SlamData.Property.Workspace.Card.ChartOptions.Model
  ( ArbModel
  , runArbModel
  , check
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Foldable (fold)

import SlamData.Workspace.Card.ChartOptions.Model as M

import Test.StrongCheck (class Arbitrary, QC, Result(Failed), quickCheck, arbitrary)
import Test.SlamData.Property.Workspace.Card.Chart.ChartConfiguration (runArbChartConfiguration, checkChartConfigEquality)
import Test.SlamData.Property.Workspace.Card.Chart.BuildOptions (runArbBuildOptions, checkBuildOptionsEquality)

newtype ArbModel = ArbModel M.Model

runArbModel :: ArbModel -> M.Model
runArbModel (ArbModel m) = m

instance arbitraryArbModel :: Arbitrary ArbModel where
  arbitrary = do
    options <- runArbBuildOptions <$> arbitrary
    chartConfig <- runArbChartConfiguration <$> arbitrary
    pure $ ArbModel { options, chartConfig }


check :: QC Unit
check = quickCheck $ runArbModel >>> \model ->
  case M.decode (M.encode model) of
    Left err -> Failed $ "Decode failed: " ++ err
    Right model' ->
      fold
       [ checkChartConfigEquality model.chartConfig model'.chartConfig
       , checkBuildOptionsEquality model.options model'.options
       ]
