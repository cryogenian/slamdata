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

module Test.SlamData.Property.Notebook.Card.Viz.Model
  ( ArbModel
  , runArbModel
  , check
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Foldable (fold)

import SlamData.Notebook.Card.Viz.Model as M

import Test.StrongCheck (QC, Result(..), class Arbitrary, arbitrary, quickCheck, (<?>))

import Test.SlamData.Property.Notebook.Card.Chart.ChartType (runArbChartType)
import Test.SlamData.Property.Notebook.Card.Chart.ChartConfiguration (runArbChartConfiguration, checkChartConfigEquality)

newtype ArbModel = ArbModel M.Model

runArbModel :: ArbModel -> M.Model
runArbModel (ArbModel m) = m

instance arbitraryArbModel :: Arbitrary ArbModel where
  arbitrary = do
    width <- arbitrary
    height <- arbitrary
    chartType <- runArbChartType <$> arbitrary
    chartConfig <- runArbChartConfiguration <$> arbitrary
    axisLabelFontSize <- arbitrary
    axisLabelAngle <- arbitrary
    pure $ ArbModel { width, height, chartType, chartConfig, axisLabelFontSize, axisLabelAngle }

check :: QC Unit
check = quickCheck $ runArbModel >>> \model ->
  case M.decode (M.encode model) of
    Left err -> Failed $ "Decode failed: " ++ err
    Right model' ->
      fold
       [ model.width == model'.width <?> "width mismatch"
       , model.height == model'.height <?> "height mismatch"
       , model.chartType == model'.chartType <?> "chartType mismatch"
       , checkChartConfigEquality model.chartConfig model'.chartConfig
       , model.axisLabelFontSize == model'.axisLabelFontSize <?> "axisLabelFontSize mismatch"
       , model.axisLabelAngle == model'.axisLabelAngle <?> "axisLabelAngle mismatch"
       ]
