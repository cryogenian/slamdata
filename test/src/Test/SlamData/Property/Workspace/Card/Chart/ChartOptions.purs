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

module Test.SlamData.Property.Workspace.Card.Chart.ChartOptions where

import Prelude

import Data.Either (Either(..))
import Data.Foldable (fold)

import SlamData.Workspace.Card.Chart.ChartOptions as CO

import Test.SlamData.Property.Workspace.Card.Chart.ChartType (runArbChartType)
import Test.StrongCheck (QC, Result(..), class Arbitrary, arbitrary, quickCheck, (<?>))

newtype ArbBuildOptions = ArbBuildOptions CO.BuildOptions

runArbBuildOptions :: ArbBuildOptions -> CO.BuildOptions
runArbBuildOptions (ArbBuildOptions m) = m

instance arbitraryArbBuildOptions :: Arbitrary ArbBuildOptions where
  arbitrary = do
    chartType <- runArbChartType <$> arbitrary
    axisLabelAngle <- arbitrary
    axisLabelFontSize <- arbitrary
    pure $ ArbBuildOptions { chartType, axisLabelAngle, axisLabelFontSize }

check :: QC Unit
check = quickCheck $ runArbBuildOptions >>> \co ->
  case CO.decode (CO.encode co) of
    Left err -> Failed $ "Decode failed: " ++ err
    Right co' -> checkBuildOptionsEquality co co'

checkBuildOptionsEquality :: CO.BuildOptions -> CO.BuildOptions -> Result
checkBuildOptionsEquality co co' =
  fold
   [ co.chartType == co'.chartType <?> "chart type mismatch"
   , co.axisLabelAngle == co'.axisLabelAngle <?> "axis label angle mismatch"
   , co.axisLabelFontSize == co'.axisLabelFontSize <?> "axis label font size mismatch"
   ]
