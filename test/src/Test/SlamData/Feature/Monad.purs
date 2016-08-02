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

module Test.SlamData.Feature.Monad where

import Prelude

import Control.Monad.Reader.Class (ask)

import Selenium.Monad (later)

import Test.Feature.Log (warnMsg)
import Test.Feature.Monad (Feature)
import Test.SlamData.Feature.Config (Config)
import Test.SlamData.Feature.Effects (SlamFeatureEffects)

type SlamFeature = Feature (SlamFeatureEffects ()) (config :: Config)

getConfig :: SlamFeature Config
getConfig = _.config <$> ask

waitTime :: Int -> SlamFeature Unit
waitTime t = do
  warnMsg $ "Warning: Tests manually waited for " <> show t <> " milliseconds."
  later t $ pure unit
