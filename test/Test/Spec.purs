module Test.Spec where

import Control.Monad.Eff

import Test.Mocha
import Test.Chai


import Hash

foreign import data Assert :: !

foreign import assertIt """
function assertIt(a) {
  return function() {
    return assert(a);
  };
}
""" :: forall a e. a -> Eff (assert::Assert|e) Unit

spec = describe "Nothing" $ do
  it "equality is equality" $ do
    expect 2 `toEqual` 2
       
