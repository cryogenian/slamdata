module Signal.Effectful where

import Signal
import Control.Monad.Eff
import Data.Function

foreign import foldpEP """
  function foldpEP (constant, upd, seed, sig) {
    return function () {
      var acc = seed;
      var out = constant(acc);
      sig.subscribe(function(val) {
        acc = upd(val)(acc)();
        out.set(acc);
      });
      return out;
    };
  }""" :: forall a b c e.
          Fn4
          (c -> Signal c)
          (a -> b -> Eff e b)
          b
          (Signal a)
          (Eff e (Signal b))

          
foldpE = runFn4 foldpEP constant
