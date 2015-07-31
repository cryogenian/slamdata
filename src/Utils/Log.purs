module Utils.Log where

import Prelude 

spyF :: forall a m. (Applicative m) => a -> m Unit
spyF a = let x = spy a in pure unit

spyM :: forall a m. (Applicative m) => a -> m a
spyM a = pure $ spy a

foreign import spy :: forall a. a -> a
