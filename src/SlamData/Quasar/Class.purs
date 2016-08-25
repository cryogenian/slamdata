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

module SlamData.Quasar.Class where

import SlamData.Prelude

import Control.Monad.Free (Free, liftF)

import Halogen.Query.EventSource as ES
import Halogen.Query.HalogenF as HF

import Quasar.Advanced.QuasarAF as QF

class QuasarDSL m where
  liftQuasar ∷ QF.QuasarAFC ~> m

instance quasarDSLMaybeT ∷ (Monad m, QuasarDSL m) ⇒ QuasarDSL (MaybeT m) where
  liftQuasar = lift ∘ liftQuasar

instance quasarDSLExceptT ∷ (Monad m, QuasarDSL m) ⇒ QuasarDSL (ExceptT e m) where
  liftQuasar = lift ∘ liftQuasar

instance quasarDSLFree ∷ QuasarDSL m ⇒ QuasarDSL (Free m) where
  liftQuasar = liftF ∘ liftQuasar

instance quasarDSLHFC ∷ QuasarDSL g ⇒ QuasarDSL (HF.HalogenFP ES.EventSource s f g) where
  liftQuasar = HF.QueryHF ∘ liftQuasar

instance quasarDSLHFP ∷ QuasarDSL g ⇒ QuasarDSL (HF.HalogenFP ES.ParentEventSource s f (Free (HF.HalogenFP ES.EventSource s' f' g))) where
  liftQuasar = HF.QueryHF ∘ liftQuasar
