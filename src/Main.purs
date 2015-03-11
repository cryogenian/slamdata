module Main where

import Control.Monad.Eff

import Data.Tuple

import Halogen (runUIEff, Handler(..))
import Halide (ui, Input(..), Request(..))
import Data.Void
import Signal.Channel (Chan())
import DOM (DOM())
import Debug.Trace (Trace())
import Utils (bodyNode, onLoad, convertToElement, append, log)
import qualified Component as Component
import qualified View as View
import Control.Timer (interval, timeout, Timer())
import Debug.Trace

handler :: forall e. Handler Request Input (trace::Trace, timer::Timer|e)
handler r k =
  case r of
    Up -> do
      log "UP"
      void $ timeout 5000 $ do
        log "!!!!!"
        k Inc
        k Inc
        k Inc
        k Inc

main = onLoad $ void $ do
  Tuple node driver <- runUIEff ui absurd handler
  body <- bodyNode
  append body (convertToElement node)
  void $ interval 1000 $ do
    log "TICK"
    driver Inc



  

  
