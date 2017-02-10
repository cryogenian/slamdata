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

module SlamData.Workspace.Card.Component.State
  ( CardState(..)
  , initialCardState
  , _sub
  , _pending
  ) where

import SlamData.Prelude

import Control.Monad.Aff.Bus (BusRW)

import Data.Lens (Lens', lens)

import Halogen as H

import SlamData.Workspace.Eval.Card as Card

-- | The common state value for deck cards.
type CardState =
  { sub ∷ H.SubscribeStatus
  , pending ∷ Boolean
  , bus ∷ Maybe (BusRW Card.EvalMessage)
  }

-- | Creates an initial `CardState` value for an editor card.
initialCardState ∷ CardState
initialCardState =
  { sub: H.Listening
  , pending: true
  , bus: Nothing
  }

_sub ∷ Lens' CardState H.SubscribeStatus
_sub = lens _.sub _{sub = _}

_pending ∷ Lens' CardState Boolean
_pending = lens _.pending _{pending = _}
