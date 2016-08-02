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

import SlamData.Prelude

import SlamData.Workspace.Card.ChartOptions.Model as M

import Test.SlamData.Property.Workspace.Card.Chart.BuildOptions (runArbBuildOptions, checkBuildOptionsEquality)
import Test.SlamData.Property.Workspace.Card.Chart.ChartConfiguration (runArbChartConfiguration, checkChartConfigEquality)
import Test.StrongCheck (SC, Result(..), quickCheck, (<?>))
import Test.StrongCheck.Arbitrary (class Arbitrary, arbitrary)

newtype ArbModel = ArbModel M.Model

runArbModel ∷ ArbModel → M.Model
runArbModel (ArbModel m) = m

instance arbitraryArbModel ∷ Arbitrary ArbModel where
  arbitrary = do
    options ← runArbBuildOptions <$> arbitrary
    needCC ← arbitrary
    chartConfig ←
      if needCC then Just <$> runArbChartConfiguration <$> arbitrary else pure Nothing
    pure $ ArbModel { options, chartConfig }


check ∷ forall eff. SC eff Unit
check = quickCheck $ runArbModel >>> \model →
  case M.decode (M.encode model) of
    Left err → Failed $ "Decode failed: " <> err
    Right model' →
      fold
       [ checkBuildOptionsEquality model.options model'.options
       , case model.chartConfig × model'.chartConfig of
           Nothing × Nothing → Success
           (Just opts) × (Just opts') →  checkChartConfigEquality opts opts'
           _ → Failed "models mismatch"
       ]
