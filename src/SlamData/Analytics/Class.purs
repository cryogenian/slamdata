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

module SlamData.Analytics.Class
  ( class AnalyticsDSL
  , track
  ) where

import SlamData.Prelude

import Control.Monad.Free (Free, liftF)

import Halogen.Query.EventSource as ES
import Halogen.Query.HalogenF as HF

import SlamData.Analytics.Event (Event)

class AnalyticsDSL m where
  track ∷ Event → m Unit

instance analyticsDSLMaybeT ∷ (Monad m, AnalyticsDSL m) ⇒ AnalyticsDSL (MaybeT m) where
  track = lift ∘ track

instance analyticsDSLExceptT ∷ (Monad m, AnalyticsDSL m) ⇒ AnalyticsDSL (ExceptT e m) where
  track = lift ∘ track

instance analyticsDSLFree ∷ AnalyticsDSL m ⇒ AnalyticsDSL (Free m) where
  track = liftF ∘ track

instance analyticsDSLHFC ∷ AnalyticsDSL g ⇒ AnalyticsDSL (HF.HalogenFP ES.EventSource s f g) where
  track = HF.QueryHF ∘ track

instance analyticsDSLHFP ∷ AnalyticsDSL g ⇒ AnalyticsDSL (HF.HalogenFP ES.ParentEventSource s f (Free (HF.HalogenFP ES.EventSource s' f' g))) where
  track = HF.QueryHF ∘ track
