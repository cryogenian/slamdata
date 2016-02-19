{-
Copyright 2016 SlamData, Inc.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
-}

module Utils.Random where

import Prelude

import Control.Monad.Eff.Random (random, RANDOM())

import Data.Date (nowEpochMilliseconds, Now())
import Data.Foldable (Foldable, foldl)
import Data.Functor.Eff (FunctorEff, liftEff)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Monoid (Monoid, mempty)
import Data.String.Regex as Rgx
import Data.Time (Milliseconds(..))
import Data.Tuple (Tuple(..), snd)

-- | Getting random element from any `Foldable` in any `MonadEff`
-- | Returns `Nothing` if foldable is empty
randomIn
  :: forall a m f e
   . (Foldable f, Monad m, FunctorEff (random :: RANDOM|e) m)
  => f a
  -> m (Maybe a)
randomIn fa =
  map snd $ foldl foldFn (pure $ Tuple zero Nothing) fa
  where
  foldFn :: m (Tuple Number (Maybe a)) -> a -> m (Tuple Number (Maybe a))
  foldFn mMa a = do
    ma <- mMa
    prob <- liftEff random
    pure case ma of
      Tuple _ Nothing -> Tuple prob $ Just a
      Tuple p (Just b) ->
        if prob > p
          then Tuple prob $ Just a
          else Tuple p $ Just b


-- | same as `randomIn` but returns `mempty` instead of `Nothing`
randomInM
  :: forall a m f e
   . (Foldable f, Monad m, FunctorEff (random :: RANDOM|e) m, Monoid a)
   => f a
   -> m a
randomInM fa = fromMaybe mempty <$> randomIn fa

randomString
  :: forall e g
   . (FunctorEff (random :: RANDOM|e) g)
  => g String
randomString =
  liftEff
    $ random
    <#> mkString

foreign import toString
  :: forall e
   . Int
  -> Number
  -> String

mkString :: Number -> String
mkString = toString 36 >>> Rgx.replace numAndDot ""
  where
  numAndDot = Rgx.regex "(\\d|\\.)" Rgx.noFlags{global=true}

-- | Generate unique key for component
genKey
  :: forall eff g
   . (FunctorEff (now :: Now, random :: RANDOM|eff) g)
  => g String
genKey = liftEff do
  rn1 <- randomString
  rn2 <- randomString
  (Milliseconds time) <- nowEpochMilliseconds
  pure $ rn1 <> mkString time <> rn2
