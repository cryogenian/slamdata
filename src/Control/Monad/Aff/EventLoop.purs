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

module Control.Monad.Aff.EventLoop
  ( make
  , make'
  , forever
  , forever'
  , break
  , break'
  , Breaker
  ) where

import Prelude
import Control.Monad.Aff.AVar (AffAVar, AVAR, makeVar', takeVar, putVar)
import Control.Monad.Aff.Free (class Affable, fromAff)
import Control.Monad.Rec.Class (class MonadRec, tailRecM)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), maybe)

newtype Breaker a = Breaker (∀ eff. a → AffAVar eff Unit)

-- | Creates a new runnable event loop, which can be started using `run`. The
-- | event loop will run forever
make
  ∷ ∀ eff a
  . AffAVar eff (Maybe a)
  → AffAVar eff { breaker ∷ Breaker a, run ∷ AffAVar eff a }
make go = do
  breaker ← makeVar' Nothing
  pure
    { breaker: Breaker \a → putVar breaker (Just a)
    , run: tailRecM (loop breaker) unit
    }

  where
  loop breaker _ = do
    res ← takeVar breaker
    putVar breaker Nothing
    case res of
      Nothing → maybe (Left unit) Right <$> go
      Just a → pure (Right a)

-- | Like `make` but with an Affable constraint.
make'
  ∷ ∀ eff m a
  . (Affable (avar ∷ AVAR | eff) m, MonadRec m)
  ⇒ m (Maybe a)
  → m { breaker ∷ Breaker a, run ∷ m a }
make' go = fromAff do
  breaker ← makeVar' Nothing
  pure
    { breaker: Breaker \a → putVar breaker (Just a)
    , run: tailRecM (loop breaker) unit
    }

  where
  loop breaker _ = do
    res ← fromAff (takeVar breaker)
    fromAff (putVar breaker Nothing)
    case res of
      Nothing → maybe (Left unit) Right <$> go
      Just a → pure (Right a)

forever
  ∷ ∀ eff a
  . AffAVar eff a
  → AffAVar eff { breaker ∷ Breaker Unit, run ∷ AffAVar eff Unit }
forever go =
  make (go $> Nothing)

forever'
  ∷ ∀ eff m a
  . (Affable (avar ∷ AVAR | eff) m, MonadRec m)
  ⇒ m a
  → m { breaker ∷ Breaker Unit, run ∷ m Unit }
forever' go =
  make' (go $> Nothing)

break ∷ ∀ eff a. Breaker a → a → AffAVar eff Unit
break (Breaker run) = run

break' ∷ ∀ eff. Breaker Unit → AffAVar eff Unit
break' (Breaker run) = run unit
