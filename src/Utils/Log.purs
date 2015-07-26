module Utils.Log where

import Prelude 

spyM :: forall a m. (Applicative m) => a -> m Unit
spyM a = let x = spy a in pure unit

foreign import spy :: forall a. a -> a
