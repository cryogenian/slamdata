module Main where

import Control.Monad.Eff
import Data.Maybe
import Signal

import Utils
import Halogen
import Halide (ui)

main :: Eff _ Unit
main = onLoad $ void $ do
  view <- convertToElement <$> runUI ui
  -- construct
--  view <- Component.define View.spec
  -- get node to insert
  body <- bodyNode
  -- start
  append body view
--  view.insert body



  

  
