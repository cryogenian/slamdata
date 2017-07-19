{-
Copyright 2017 SlamData, Inc.

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

module Test.Feature.Log where

import Prelude

import Control.Monad.Aff.Console (log)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Trans.Class (lift)
import Test.Feature.Monad (Feature)
import Text.Chalky (blue, green, magenta, red, yellow)

annotate :: forall e o. String → Feature e o Unit → Feature e o Unit
annotate txt action = do
  action
  debugMsg txt


successMsg ∷ forall eff o. String → Feature eff o Unit
successMsg msg = void $ lift $ log $ green msg

debugMsg  ∷ forall eff o. String → Feature eff o Unit
debugMsg  msg = void $ lift $ log $ blue msg

errorMsg ∷ forall a eff o. String → Feature eff o a
errorMsg msg = do
  lift $ log $ red msg
  throwError $ error msg

sectionMsg ∷ forall eff o. String → Feature eff o Unit
sectionMsg msg = void $ lift $ log $ magenta $ "\n" <> msg

warnMsg ∷ forall eff o. String → Feature eff o Unit
warnMsg msg = void $ lift $ log $ yellow msg
