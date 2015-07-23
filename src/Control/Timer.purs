-- Hope we can drop to use this module and switch to `later` from `purescript-aff`
module Control.Timer where

import Prelude
import Control.Monad.Eff (Eff())

foreign import data TIMER :: !
foreign import data Timeout :: *
foreign import data Interval :: *

foreign import timeout :: forall a e. Int -> Eff (timer :: TIMER|e) a -> Eff (timer ::TIMER|e) Timeout 
foreign import clearTimeout :: forall e. Timeout -> Eff (timer :: TIMER|e) Unit
foreign import interval :: forall a e. Int -> Eff (timer :: TIMER|e) a -> Eff (timer :: TIMER|e) Interval
foreign import clearInterval :: forall e. Interval -> Eff (timer :: TIMER|e) Unit
