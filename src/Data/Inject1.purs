module Data.Inject1 where

import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..))

class Inject1 a b where
  inj :: a -> b
  prj :: b -> Maybe a

instance inject1Reflexive :: Inject1 a a where
  inj = id
  prj = Just

instance inject1Left :: Inject1 a (Either a b) where
  inj = Left
  prj = either Just (const Nothing)

instance inject1Right :: (Inject1 a b) => Inject1 a (Either c b) where
  inj = Right <<< inj
  prj = either (const Nothing) prj
