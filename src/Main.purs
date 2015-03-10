module Main where

import Control.Monad.Eff

import Signal.Channel (Chan())
import DOM (DOM())
import Debug.Trace (Trace())
import Utils (bodyNode, onLoad)
import Control.Timer (Timer())
import qualified Component as Component
import qualified View as View 

main :: forall e. Eff (chan::Chan, timer::Timer, dom::DOM, trace::Trace|e) Unit
main = onLoad $ do
  -- construct
  view <- Component.define View.spec
  -- get node to insert
  body <- bodyNode
  -- start
  view.insert body



  

  
