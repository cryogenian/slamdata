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

module Control.Monad.Fork where

import Prelude
import Control.Monad.Aff as Aff
import Control.Monad.Eff.Exception (Error)
import Control.Monad.Reader.Trans (ReaderT(..))
import Control.Monad.Trans (lift)
import Data.Monoid (class Monoid)

class Monad m ⇐ MonadFork m where
  fork ∷ ∀ a. m a → m (Canceler m)
  cancelWith ∷ ∀ a. m a → Canceler m → m a

instance monadForkAff ∷ MonadFork (Aff.Aff eff) where
  fork aff = do
    ac ← Aff.forkAff aff
    pure $ Canceler \reason → Aff.cancel ac reason
  cancelWith aff (Canceler f) =
    Aff.cancelWith aff (Aff.Canceler f)

instance monadForkReaderT ∷ MonadFork m => MonadFork (ReaderT r m) where
  fork (ReaderT ma) =
    ReaderT \r → hoistCanceler lift <$> fork (ma r)
  cancelWith (ReaderT ma) c =
    ReaderT \r → cancelWith (ma r) (hoistCanceler (\(ReaderT mb) → mb r) c)

newtype Canceler m = Canceler (Error → m Boolean)

instance semigroupCanceler :: Apply m => Semigroup (Canceler m) where
  append (Canceler f1) (Canceler f2) = Canceler (\e -> (||) <$> f1 e <*> f2 e)

instance monoidCanceler :: Applicative m => Monoid (Canceler m) where
  mempty = Canceler (const (pure true))

cancel ∷ ∀ m. Error → Canceler m → m Boolean
cancel e (Canceler f) = f e

hoistCanceler ∷ forall m n. (m ~> n) → Canceler m -> Canceler n
hoistCanceler nat (Canceler f) = Canceler (nat <<< f)
