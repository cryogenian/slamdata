module Main where

import Control.Monad.Eff
import Data.Maybe
import Signal

import Utils
import qualified Component as Component
--import qualified View.Navbar as View
import qualified View.List as List
import qualified View as View
import qualified Router as Router

import Signal
import Signal.Channel


main :: Eff _ Unit
main = onLoad $ do
  -- construct
  view <- Component.define View.spec
  -- get node to insert
  body <- bodyNode
  -- start
  view.insert body



  

  
