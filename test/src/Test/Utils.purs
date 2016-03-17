module Test.Utils where

import Prelude

import Control.Alt (Alt, (<|>))
import Control.Apply ((*>))
import Control.Bind ((<=<))
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Exception (EXCEPTION(), throw)
import Data.Foldable (Foldable, foldr)
import Data.Maybe (Maybe(..), maybe)
import Data.Either (Either(..), either)
import Node.Process (PROCESS(), cwd)


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

isEmpty :: forall a m. (Foldable m) => m a -> Boolean
isEmpty =
  foldr (\_ _ -> false) true

singletonValue' :: forall a m. (Foldable m) => m a -> Either Int (Maybe a)
singletonValue' =
  foldr f initial
  where
  f x (Right (Just _)) = Left 1
  f x (Right Nothing) = Right $ Just x
  f x (Left i) = Left $ i + 1
  initial = Right Nothing

singletonValue :: forall a m n. (Applicative m, Foldable n) => m a -> (Int -> m a) -> n a -> m a
singletonValue noElements tooManyElements =
  either tooManyElements (maybe noElements pure) <<< singletonValue'

throwIfEmpty :: forall a m eff. (Foldable m) => String -> m a -> Eff (err :: EXCEPTION | eff) Unit
throwIfEmpty message xs | isEmpty xs = throw message
throwIfEmpty _ _ = pure unit

throwIfNotEmpty :: forall a m eff. (Foldable m) => String -> m a -> Eff (err :: EXCEPTION | eff) Unit
throwIfNotEmpty _ xs | isEmpty xs = pure unit
throwIfNotEmpty message _ = throw message


appendToCwd :: forall eff. String -> Eff (process :: PROCESS | eff) String
appendToCwd s = (flip append s <<< flip append "/") <$> cwd
