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

module Test.Selenium.Log where

import Prelude
import Text.Chalky (red, green, magenta, yellow)
import Control.Monad.Trans (lift)
import Control.Monad.Aff.Console (log)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Error.Class (throwError)
import Test.Selenium.Monad (Check())

successMsg :: String -> Check Unit
successMsg msg = void $ lift $ log $ green msg

errorMsg :: forall a. String -> Check a
errorMsg msg = do
  lift $ log $ red msg
  throwError $ error msg

sectionMsg :: String -> Check Unit
sectionMsg msg = void $ lift $ log $ magenta $ "\n" <> msg

warnMsg :: String -> Check Unit
warnMsg msg = void $ lift $ log $ yellow msg
