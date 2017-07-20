module Test.SlamData.Feature.Expectations.Charts where

import SlamData.Prelude

import Test.Feature (expectPresented)
import Test.Feature.Log (annotate)
import Test.SlamData.Feature.Monad (SlamFeature)
import Test.SlamData.Feature.XPaths as XPaths
import XPath as XPath

lastEChart ∷ String → SlamFeature Unit
lastEChart st =
  annotate "Found expected Echart value"
    $ (expectPresented
        ∘ XPath.last
        ∘ XPath.anywhere
        ∘ XPaths.eChart) st
