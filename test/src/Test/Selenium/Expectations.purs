module Test.Selenium.Expectations where

import Prelude
import Test.Selenium.Monad (Check())
import Test.Selenium.Finders (findByXPath)

expectPresented :: String -> Check Unit
expectPresented = void <<< findByXPath

