module Test.Utils where

import Prelude

import Control.Alt (Alt, (<|>))
import Control.Apply ((*>))
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Exception (EXCEPTION(), throw)
import Data.Foldable (Foldable, foldr)

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

null :: forall a m. (Foldable m) => m a -> Boolean
null = foldr (\_ _ -> false) true

throwIfEmpty :: forall a m eff. (Foldable m) => String -> m a -> Eff (err :: EXCEPTION | eff) Unit
throwIfEmpty _ xs | null xs = pure unit
throwIfEmpty message _ = throw message

