module Data.Platform where

data Platform = Mac | Win | Other

instance eqPlatform :: Eq Platform where
  (==) Mac Mac = true
  (==) Win Win = true
  (==) Other Other = true
  (==) _ _ = false
  (/=) x y = not (x == y)
