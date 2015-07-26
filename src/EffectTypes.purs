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
                              , zClipboard :: ZCLIPBOARD | e)

type NotebookAppEff e = HalogenEffects (NotebookComponentEff e)
