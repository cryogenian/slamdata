module Utils.Array where

import Prelude (($))
import Data.Array (range, length, zip)
import Data.Tuple (Tuple())

enumerate :: forall a. Array a -> Array (Tuple Int a)
enumerate arr = zip (range 0 $ length arr) arr
