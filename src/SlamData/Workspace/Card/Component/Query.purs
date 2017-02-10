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

module SlamData.Workspace.Card.Component.Query
  ( CardQuery(..)
  , InnerCardQuery
  , _CardEvalQuery
  , _CardQuery
  , module EQ
  ) where

import SlamData.Prelude

import Data.Lens (Prism')
import Data.Lens.Prism.Coproduct (_Left, _Right)

import Halogen as H

import SlamData.Workspace.Card.Common.EvalQuery as EQ
import SlamData.Workspace.Eval.Card as Card

-- | The common query algebra for a card.
data CardQuery a
  = Initialize a
  | Finalize a
  | ActivateCard a
  | DeactivateCard a
  | UpdateDimensions a
  | HandleEvalMessage (Card.EvalMessage) (H.SubscribeStatus → a)
  | HandleCardMessage (EQ.CardEvalMessage) a

type InnerCardQuery f = Coproduct EQ.CardEvalQuery f

_CardEvalQuery ∷ ∀ f a. Prism' (InnerCardQuery f a) (EQ.CardEvalQuery a)
_CardEvalQuery = _Left

_CardQuery ∷ ∀ f a. Prism' (InnerCardQuery f a) (f a)
_CardQuery = _Right
