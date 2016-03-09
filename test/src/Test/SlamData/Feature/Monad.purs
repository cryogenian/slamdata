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

import Control.Monad.Eff.Exception (error)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Reader.Class
import Control.Alt (alt)

import Data.Functor.Aff (liftAff)
import Data.List (List(), length, uncons)
import Data.Maybe (fromMaybe, Maybe(..))

import Graphics.ImageDiff as GI

import Platform (getPlatform, runOs, runPlatform)

import Selenium (showLocator)
import Selenium.Key (metaKey, controlKey)
import Selenium.Monad (Selenium(), byCss, byXPath, findElements)
import Selenium.Types (ControlKey(), Locator(), Element())

import Test.SlamData.Feature.Config (Config())
import Test.SlamData.Feature.Effects (SlamFeatureEffects())
import Test.Feature.Monad (Feature())

type SlamFeature = Feature (SlamFeatureEffects ()) (config :: Config)

getConfig :: SlamFeature Config
getConfig = _.config <$> ask

diff :: { shadow :: Boolean, diff :: Maybe String, expected :: String, actual :: String } -> SlamFeature Boolean
diff = liftAff <<< GI.diff
