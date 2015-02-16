module Test.Main where

import Control.Monad.Eff

import Test.Spec
import Test.Check 

main = do
  spec
  check
