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

module Utils.AffableProducer where

import SlamData.Prelude

import Control.Coroutine (Producer)
import Control.Coroutine.Aff as CCA
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Free (class Affable, fromAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Free.Trans (hoistFreeT)

produce
  :: forall a r m eff
   . (Functor m, Affable (avar :: AVAR | eff) m)
  => ((Either a r -> Eff (avar :: AVAR | eff) Unit) -> Eff (avar :: AVAR | eff) Unit)
  -> Producer a m r
produce = hoistFreeT fromAff <<< CCA.produce
