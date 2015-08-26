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

module EffectTypes where

import Data.Date (Now())
import Control.Monad.Eff.Random(RANDOM())
import Control.Timer (TIMER())
import Network.HTTP.Affjax (AJAX())
import Control.Monad.Aff.AVar (AVAR())
import Halogen (HalogenEffects())
import ECharts.Effects ( ECHARTS_INIT()
                       , ECHARTS_OPTION_SET()
                       , ECHARTS_DISPOSE()
                       , ECHARTS_RESIZE()
                       )
import Utils.File (READ_FILE())
import DOM (DOM())
import Control.UI.ZClipboard (ZCLIPBOARD())


type FileComponentEff e = ( timer :: TIMER
                          , ajax :: AJAX
                          , avar :: AVAR
                          , file :: READ_FILE
                          , zClipboard :: ZCLIPBOARD
                          , random :: RANDOM | e)

type FileAppEff e = HalogenEffects (FileComponentEff e)


type NotebookComponentEff e = ( timer :: TIMER
                              , echartInit :: ECHARTS_INIT
                              , echartSetOption :: ECHARTS_OPTION_SET
                              , echartDispose :: ECHARTS_DISPOSE
                              , echartResize :: ECHARTS_RESIZE
                              , now :: Now
                              , ajax :: AJAX
                              , avar :: AVAR
                              , zClipboard :: ZCLIPBOARD | e)

type NotebookAppEff e = HalogenEffects (NotebookComponentEff e)
