module Main where

import Control.Monad.Eff
import Data.Maybe
import Signal

import Utils
import qualified Component as Component
--import qualified View.Navbar as View
import qualified View.List as List
import qualified View as View

main :: Eff _ Unit
main = onLoad $ do
  view <- Component.define View.spec
  body <- bodyNode
  view.insert body


  

  
