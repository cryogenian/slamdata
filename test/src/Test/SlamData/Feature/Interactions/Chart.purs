module Test.SlamData.Feature.Interactions.Chart where

import SlamData.Prelude

import Test.Feature as Feature
import Test.SlamData.Feature.Monad (SlamFeature)
import Test.SlamData.Feature.XPaths as XPaths
import Test.SlamData.Feature.Interactions.Cards (selectInMillerColumns)

import XPath as XPath

selectBuildChart ∷ SlamFeature Unit
selectBuildChart =
  Feature.click $ XPaths.followingLastPreviousCardGripper XPaths.selectBuildChart

selectInChartBuilder ∷ Array String → SlamFeature Unit
selectInChartBuilder ps = do
  selectInMillerColumns ps
  Feature.click $ XPath.last $ XPath.anywhere $ XPath.anyWithExactText "Confirm"

activateCategoryForChartBuilder ∷ SlamFeature Unit
activateCategoryForChartBuilder =
  Feature.click $ XPath.last $ XPath.anywhere $ XPaths.chartCategorySelector

activateMeasureForChartBuilder ∷ SlamFeature Unit
activateMeasureForChartBuilder =
  Feature.click $ XPath.last $ XPath.anywhere $ XPaths.chartMeasureSelector

activateStackForChartBuilder ∷ SlamFeature Unit
activateStackForChartBuilder =
  Feature.click $ XPath.last $ XPath.anywhere $ XPaths.chartStackSelector
