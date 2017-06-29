module Test.SlamData.Feature.Expectations.Text where

import SlamData.Prelude
import XPath as XPath
import Test.Feature (expectPresented)
import Test.SlamData.Feature.Monad (SlamFeature)

text ∷ String → SlamFeature Unit
text = expectPresented ∘ XPath.anywhere ∘ XPath.anyWithText

textEventually ∷ String → SlamFeature Unit
textEventually = text
