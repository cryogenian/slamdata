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

module Driver.Notebook.ECharts (
  echartsPostRender,
  ref,
  EChartsKnot(),
  EChartsRec(),
  EContainer()) where

import Prelude
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Ref (REF(), Ref(), readRef, newRef, modifyRef)
import Data.Tuple (Tuple(..))
import Data.Maybe (maybe, Maybe(..))
import Data.Foldable (for_)
import DOM (DOM(), NodeList())
import Data.DOM.Simple.Types (HTMLElement())
import Data.DOM.Simple.NodeList
import Data.DOM.Simple.Element
import qualified Data.StrMap as M
import Optic.Types (Lens())
import Optic.Lens (lens)
import Optic.Setter ((%~))

import Halogen (Driver())
import qualified ECharts.Chart as EC
import qualified ECharts.Options as EC
import Data.Argonaut.Encode (encodeJson)

import EffectTypes (NotebookComponentEff(), NotebookAppEff())
import Input.Notebook (Input(..))
import Control.Timer (Timeout(), timeout, clearTimeout)
import qualified Config as Config

type EChartsKey = String

newtype EContainer = EContainer HTMLElement

foreign import containerEq :: EContainer -> EContainer -> Boolean 

instance eqEContainer :: Eq EContainer where
  eq = containerEq
  

type EChartsRec =
  { chart :: EC.EChart
  , container :: EContainer
  } 
type EChartsKnot = { charts :: M.StrMap EChartsRec
                   , options :: M.StrMap EC.Option
                   , timeouts :: M.StrMap Timeout
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
  , timeouts: M.empty
  } 

ref :: forall e. Eff (ref :: REF | e) (Ref EChartsKnot) 
ref = newRef initKnot

echartsPostRender :: forall e. Ref EChartsKnot -> Input ->
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
    ResizeECharts k -> do
      flip (maybe (pure unit)) (M.lookup k knot.charts) \chart ->
        maybe (setTimeout chart k) (\t -> do
                                     clearTimeout t
                                     setTimeout chart k) $ M.lookup k knot.timeouts

    _ -> pure unit
  where
  setTimeout chart k = do
    t <- timeout Config.resizeEChartsTimeout do
      EC.resize chart.chart
    modifyRef knotRef (\s -> s{timeouts = M.insert k t s.timeouts})

init :: forall e. HTMLElement -> String -> Ref EChartsKnot ->
        Eff (NotebookAppEff e) Unit
init el key knotRef = do
  knot <- readRef knotRef 
  chart <- EC.init Nothing el
  modifyRef knotRef (_charts %~ M.insert key { chart: chart
                                             , container: EContainer el })
  maybe (pure unit) (set chart) $ M.lookup key knot.options

set :: forall e. EC.EChart -> EC.Option -> Eff (NotebookAppEff e) Unit 
set chart opts = void $ do
  EC.setOption opts true chart
  EC.resize chart
  

reinit :: forall e. HTMLElement -> String -> Ref EChartsKnot -> EChartsRec ->
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

