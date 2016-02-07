module Test.Selenium.Expectations where

import Prelude

expectPresented :: String -> Check Unit
expectPresented = void <<< findByXPath

expectPresented :: String -> Check Unit
expectPresented = void <<< findByXPath

