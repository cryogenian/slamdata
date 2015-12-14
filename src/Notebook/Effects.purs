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

module Notebook.Effects where

import Ace.Types (ACE())
import Control.Monad.Aff.AVar (AVAR())
import Control.Monad.Eff.Exception (EXCEPTION())
import Control.Monad.Eff.Random (RANDOM())
import Control.Monad.Eff.Ref (REF())
import Control.Monad.Eff.Ref (REF())
import Control.UI.ZClipboard (ZCLIPBOARD())
import DOM (DOM())
import Data.Date (Now())
import ECharts.Effects ( ECHARTS_INIT()
                       , ECHARTS_OPTION_SET()
                       , ECHARTS_DISPOSE()
                       , ECHARTS_RESIZE()
                       , ECHARTS_REFRESH()
                       , ECHARTS_CLEAR()
                       )
import Halogen (HalogenEffects())
import Network.HTTP.Affjax (AJAX())
import DOM.Timer (Timer())

type NotebookEffects = HalogenEffects NotebookRawEffects

type NotebookRawEffects =
  ( ajax :: AJAX
  , ace :: ACE
  , now :: Now
  , zClipboard :: ZCLIPBOARD
  , echartInit :: ECHARTS_INIT
  , echartSetOption :: ECHARTS_OPTION_SET
  , echartDispose :: ECHARTS_DISPOSE
  , echartResize :: ECHARTS_RESIZE
  , echartRefresh :: ECHARTS_REFRESH
  , echartClear :: ECHARTS_CLEAR
  , random :: RANDOM
  , ref :: REF
  , timer :: Timer
  )
