module Test.Utils where

import Prelude

import Control.Alt (Alt, (<|>))
import Control.Apply ((*>))

ifFalse :: forall m. (Applicative m) => m Unit -> Boolean -> m Unit
ifFalse f boolean =
  if boolean then pure unit else f

ifTrue :: forall m. (Applicative m) => m Unit -> Boolean -> m Unit
ifTrue f boolean =
  if boolean then f else pure unit

passover :: forall a b m. (Applicative m) => (a -> m b) -> a -> m a
passover f x =
  f x *> pure x

orIfItFails :: forall a b m. (Alt m) => (a -> m b) -> (a -> m b) -> a -> m b
orIfItFails f g x =
  f x <|> g x

