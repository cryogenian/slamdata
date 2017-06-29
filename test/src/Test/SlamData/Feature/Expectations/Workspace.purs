module Test.SlamData.Feature.Expectations.Workspace where

import SlamData.Prelude
import Data.Map as Map
import XPath as XPath
import Test.Feature (expectPresentedWithProperties)
import Test.SlamData.Feature.Monad (SlamFeature)

workspaceName ∷ String → SlamFeature Unit
workspaceName name =
  expectPresentedWithProperties
    (Map.singleton "value" $ Just name)
    (XPath.anywhere "input")
