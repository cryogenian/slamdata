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

module Utils.At where

import Control.Monad.Aff (Aff, makeAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Data.Date (Now)
import Data.Date as Date
import Data.Time (Milliseconds)
import DOM (DOM)
import DOM.HTML as DOMHTML
import DOM.HTML.Types (Window)
import SlamData.Prelude

foreign import data INTERVAL ∷ !
foreign import data Interval ∷ *
foreign import setInterval
  ∷ ∀ eff
  . Window
  → Int
  → (Interval → Eff (interval ∷ INTERVAL | eff) Unit)
  → Eff (interval ∷ INTERVAL | eff) Interval

foreign import clearInterval
  ∷ ∀ eff
  . Interval → Eff (interval ∷ INTERVAL | eff) Unit

type AtEffects eff = (interval ∷ INTERVAL, now ∷ Now, dom ∷ DOM | eff)

at ∷ ∀ eff a. Milliseconds → Eff (AtEffects eff) a → Aff (AtEffects eff) a
at timeEpochMilliseconds action =
  makeAff \_ success → do
    windowReferenceObject ← DOMHTML.window
    setInterval windowReferenceObject 100 \interval → do
      now ← Date.nowEpochMilliseconds
      if now >= timeEpochMilliseconds
        then (clearInterval interval) *> (success =<< liftEff action)
        else pure unit
    pure unit
