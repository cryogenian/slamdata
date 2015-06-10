module EffectTypes where

import Data.Date (Now())
import Control.Monad.Eff.Random
import qualified Control.Timer as Tm
import qualified Network.HTTP.Affjax as Af
import qualified Control.Monad.Aff.AVar as A
import qualified Halogen as Hl
import qualified ECharts.Effects as EC
import qualified Utils.File as Uf
import DOM
import qualified Control.UI.ZClipboard as Z

type FileComponentEff e = ( timer :: Tm.Timer
                          , ajax :: Af.AJAX
                          , avar :: A.AVAR
                          , file :: Uf.ReadFile
                          , zClipboard :: Z.ZCLIPBOARD
                          , random :: Random|e)
type FileAppEff e = Hl.HalogenEffects (FileComponentEff e)


type NotebookComponentEff e = ( timer :: Tm.Timer
                              , echartInit :: EC.ECHARTS_INIT
                              , echartSetOption :: EC.ECHARTS_OPTION_SET
                              , echartDispose :: EC.ECHARTS_DISPOSE
                              , echartResize :: EC.ECHARTS_RESIZE 
                              , now :: Now
                              , ajax :: Af.AJAX
                              , zClipboard :: Z.ZCLIPBOARD | e)

type NotebookAppEff e = Hl.HalogenEffects (NotebookComponentEff e)
