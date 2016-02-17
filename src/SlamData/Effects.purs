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

import Ace.Types (ACE())
import Control.Monad.Aff (Aff())
import Control.Monad.Eff.Console (CONSOLE())
import Control.Monad.Eff.Random (RANDOM())
import Control.Monad.Eff.Ref (REF())
import Control.UI.File (READ_FILE())
import Control.UI.ZClipboard (ZCLIPBOARD())
import DOM.Timer (Timer())
import Data.Date (Now())
import ECharts.Effects (ECHARTS_INIT(), ECHARTS_OPTION_SET(), ECHARTS_DISPOSE(), ECHARTS_RESIZE(), ECHARTS_REFRESH(), ECHARTS_CLEAR())
import Halogen (HalogenEffects())
import Network.HTTP.Affjax (AJAX())

type Slam = Aff SlamDataEffects

type SlamDataEffects = HalogenEffects SlamDataRawEffects

type SlamDataRawEffects =
  ( ajax :: AJAX
  , random :: RANDOM
  , ace :: ACE
  , console :: CONSOLE
  , echartClear :: ECHARTS_CLEAR
  , echartDispose :: ECHARTS_DISPOSE
  , echartInit :: ECHARTS_INIT
  , echartRefresh :: ECHARTS_REFRESH
  , echartResize :: ECHARTS_RESIZE
  , echartSetOption :: ECHARTS_OPTION_SET
  , file :: READ_FILE
  , now :: Now
  , ref :: REF
  , timer :: Timer
  , zClipboard :: ZCLIPBOARD
  )
