{-
Copyright 2016 SlamData, Inc.

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

module SlamData.Effects where

import Ace.Types (ACE)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Now (NOW)
import Control.Monad.Eff.Random (RANDOM)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Timer (TIMER)
import Control.UI.File (READ_FILE)
import Data.JSDate (LOCALE)
import ECharts.Types (ECHARTS)
import Halogen (HalogenEffects)
import Network.HTTP.Affjax (AJAX)
import OIDC.Crypt (RSASIGNTIME)
import ZClipboard (ZCLIPBOARD)

type SlamDataEffects = HalogenEffects SlamDataRawEffects

type SlamDataRawEffects =
  ( ajax ∷ AJAX
  , random ∷ RANDOM
  , ace ∷ ACE
  , console ∷ CONSOLE
  , echarts ∷ ECHARTS
  , file ∷ READ_FILE
  , now ∷ NOW
  , ref ∷ REF
  , timer ∷ TIMER
  , rsaSignTime ∷ RSASIGNTIME
  , zClipboard ∷ ZCLIPBOARD
  , locale ∷ LOCALE
  )
