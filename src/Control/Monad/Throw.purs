module Control.Monad.Throw where

import Prelude

class Monad m ⇐ MonadThrow e m where
  throw ∷ ∀ a. e → m a
