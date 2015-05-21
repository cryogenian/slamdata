module App.Notebook.ECharts (
  echartsPostRender,
  ref,
  EChartsKnot(),
  EChartsRec(),
  EContainer()) where

import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Ref (Ref(), RefVal(), readRef, newRef, modifyRef)
import Data.Tuple (Tuple(..))
import Data.Maybe (maybe, Maybe(..))
import Data.Foldable (for_)
import DOM (DOM(), NodeList())
import Data.DOM.Simple.Types (HTMLElement())
import Data.DOM.Simple.NodeList
import Data.DOM.Simple.Element
import qualified Data.StrMap as M
import Optic.Core (Lens(), lens, (%~))

import Halogen (Driver())
import qualified ECharts.Chart as EC
import qualified ECharts.Options as EC
import Data.Argonaut.Encode (encodeJson)

import EffectTypes (NotebookComponentEff(), NotebookAppEff())
import Input.Notebook (Input(..))


type EChartsKey = String

newtype EContainer = EContainer HTMLElement

foreign import containerEq """
function containerEq(a) {
  return function(b) {
    return a == b;
  };
}
""" :: EContainer -> EContainer -> Boolean 

instance eqEContainer :: Eq EContainer where
  (==) = containerEq
  (/=) a b = not $ a == b
  

type EChartsRec =
  { chart :: EC.EChart
  , container :: EContainer
  } 
type EChartsKnot = { charts :: M.StrMap EChartsRec
                   , options :: M.StrMap EC.Option
                   }

_charts :: forall a b r. Lens { charts :: a | r} { charts :: b | r} a b 
_charts = lens _.charts _{charts = _} 

_options :: forall a b r. Lens { options :: a | r} { options :: b |r} a b 
_options = lens _.options _{options = _} 

_chart :: forall a b r. Lens { chart :: a | r} { chart :: b |r} a b 
_chart = lens _.chart _{chart = _} 

_container :: forall a b r. Lens { container :: a | r} { container :: b |r} a b 
_container = lens _.container _{container = _}

initKnot :: EChartsKnot
initKnot =
  { charts: M.empty
  , options: M.empty
  } 

ref :: forall e. Eff (ref :: Ref | e) (RefVal EChartsKnot) 
ref = newRef initKnot

echartsPostRender :: forall e. RefVal EChartsKnot -> Input ->
                     HTMLElement -> Driver Input (NotebookComponentEff e) ->
                     Eff (NotebookAppEff e) Unit
echartsPostRender knotRef input node driver = do
  els  <- querySelectorAll "[data-echarts-id]" node >>= nodeListToArray
  knot <- readRef knotRef
  for_ els \el -> do
    eid <- getAttribute "data-echarts-id" el
    maybe (init el eid knotRef) (reinit el eid knotRef) $ M.lookup eid knot.charts
  case input of
    SetEChartsOption k newOpts -> do
      modifyRef knotRef (_options %~ M.insert k newOpts)
      maybe (pure unit) (_.chart >>> flip set newOpts) $ M.lookup k knot.charts
    _ -> pure unit

init :: forall e. HTMLElement -> String -> RefVal EChartsKnot ->
        Eff (NotebookAppEff e) Unit
init el key knotRef = do
  knot <- readRef knotRef 
  chart <- EC.init Nothing el
  modifyRef knotRef (_charts %~ M.insert key { chart: chart
                                             , container: EContainer el })
  maybe (pure unit) (set chart) $ M.lookup key knot.options

set :: forall e. EC.EChart -> EC.Option -> Eff (NotebookAppEff e) Unit 
set chart opts = void $ EC.setOption opts true chart

reinit :: forall e. HTMLElement -> String -> RefVal EChartsKnot -> EChartsRec ->
          Eff (NotebookAppEff e) Unit 
reinit el key knotRef r = do
  knot <- readRef knotRef
  if r.container == EContainer el 
    then pure unit
    else do
    chart <- EC.init Nothing el
    modifyRef knotRef (_charts %~ M.insert key { chart: chart
                                               , container: EContainer el })
    maybe (pure unit) (set chart) $ M.lookup key knot.options

