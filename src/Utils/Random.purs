module Utils.Random where

import Prelude
import Control.Monad.Eff.Class (liftEff, MonadEff)
import Control.Monad.Eff.Random (random, RANDOM())
import Data.Monoid (Monoid, mempty)
import Data.Tuple (Tuple(..), snd)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Foldable (Foldable, foldl)


-- | Getting random element from any `Foldable` in any `MonadEff`
-- | Returns `Nothing` if foldable is empty
randomIn :: forall a m f e.
            (Foldable f, Monad m, MonadEff (random :: RANDOM|e) m) =>
            f a -> m (Maybe a)
randomIn fa =
  map snd $ foldl foldFn (pure $ Tuple zero Nothing) fa
  where
  foldFn :: m (Tuple Number (Maybe a)) -> a -> m (Tuple Number (Maybe a))
  foldFn mMa a = do
    ma <- mMa
    prob <- liftEff random
    pure case ma of
      Tuple _ Nothing -> Tuple prob $ Just a
      Tuple p (Just b) -> if prob > p
                          then Tuple prob $ Just a
                          else Tuple p $ Just b


-- | same as `randomIn` but returns `mempty` instead of `Nothing` 
randomInM :: forall a m f e.
                (Foldable f, Monad m, MonadEff (random :: RANDOM|e) m, Monoid a) =>
                f a -> m a
randomInM fa = fromMaybe mempty <$> randomIn fa
