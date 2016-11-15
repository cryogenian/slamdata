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

module Utils.Aff where

import Prelude
import Control.Monad.Aff (Aff)
import Control.Monad.Rec.Class (Step(..), tailRecM)
import Control.Monad.Eff.Exception as Exception
import Control.Monad.Error.Class (throwError)

-- | Loop until a condition becomes `true`.
-- |
-- | `untilE b` is an effectful computation which repeatedly runs the effectful
-- | computation `b`, until its return value is `true`.
untilA ∷ ∀ eff. Aff eff Boolean → Aff eff Unit
untilA aff =
  tailRecM go unit
  where
  go = const $ (if _ then pure (Done unit) else pure (Loop unit)) =<< aff

throw ∷ ∀ a eff. String → Aff eff a
throw = throwError <<< Exception.error
