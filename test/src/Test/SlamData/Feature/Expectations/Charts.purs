module Test.SlamData.Feature.Expectations.Charts where

import SlamData.Prelude
import Test.SlamData.Feature.XPaths as XPaths
import XPath as XPath
import Test.Feature (expectPresented)
import Test.SlamData.Feature.Monad (SlamFeature)

lastEChart ∷ String → SlamFeature Unit
lastEChart =
  expectPresented
    ∘ XPath.last
    ∘ XPath.anywhere
    ∘ XPaths.eChart
