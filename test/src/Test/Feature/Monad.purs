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

module Test.Feature.Monad where

import Control.Alt (alt)
import Control.Monad.Eff.Exception (EXCEPTION())
import Control.Monad.Reader.Class
import Data.Maybe (fromMaybe)
import Platform (PLATFORM(), getPlatform, runOs, runPlatform)
import Prelude
import Selenium.Key (metaKey, controlKey)
import Selenium.Monad (Selenium())
import Selenium.Types (ControlKey())

type Feature eff o = Selenium (platform :: PLATFORM, err :: EXCEPTION | eff) o

getPlatformString :: forall eff o. Feature eff o String
getPlatformString = do
  platform <- getPlatform
  pure $ fromMaybe ""
    $ platform
    >>= runPlatform
    >>> _.os
    >>> runOs
    >>> _.family

getModifierKey :: forall eff o. Feature eff o ControlKey
getModifierKey = map modifierKey getPlatformString
  where
  modifierKey "Darwin" = metaKey
  modifierKey _ = controlKey

notMindingIfItsNotPossible :: forall eff o. Feature eff o Unit -> Feature eff o Unit
notMindingIfItsNotPossible = flip alt (pure unit)
