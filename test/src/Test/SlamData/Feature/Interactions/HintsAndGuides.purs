module Test.SlamData.Feature.Interactions.HintsAndGuides where

import SlamData.Prelude


import Test.Feature as Feature
import Test.SlamData.Feature.Monad (SlamFeature)

import XPath as XPath

dismissHint ∷ SlamFeature Unit
dismissHint =
  Feature.click $ XPath.anywhere $ XPath.anyWithExactAriaLabel "Dismiss"

skipGuide ∷ SlamFeature Unit
skipGuide =
  Feature.click $ XPath.anywhere $ XPath.anyWithText "Skip"
