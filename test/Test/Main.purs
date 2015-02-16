module Test.Main where

import Control.Monad.Eff

import Test.Spec
import Test.Check 




foreign import log """
function log(a) {return function() {console.log(a)}}
""" :: forall e a. a -> Eff e Unit

main = do
  spec
  check
