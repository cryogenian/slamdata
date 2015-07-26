module Data.Platform where

import Prelude (Eq)

data Platform = Mac | Win | Other

instance eqPlatform :: Eq Platform where
  eq Mac Mac = true
  eq Win Win = true
  eq Other Other = true
  eq _ _ = false

