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

module SlamData.Workspace.Card.Component.Def
  ( CardDef
  , makeQueryPrism
  , makeQueryPrism'
  ) where

import SlamData.Prelude

import Data.Lens (APrism', Prism', prism', review, preview)

import Halogen (Component)

import SlamData.Workspace.Card.Common (CardOptions)
import SlamData.Workspace.Card.Common.EvalQuery (CardEvalQuery)
import SlamData.Workspace.Card.Component.Query (AnyCardQuery)
import SlamData.Workspace.Card.Component.State (AnyCardState)
import SlamData.Workspace.Card.CardType (CardType)
import SlamData.Monad (Slam)

-- | The properties required by both types of card definition.
type CardDef s f r =
  { options ∷ CardOptions
  , component ∷ Component s f Slam
  , cardType ∷ CardType
  , initialState ∷ s
  , _State ∷ APrism' AnyCardState s
  , _Query ∷ ∀ a. APrism' (Coproduct CardEvalQuery AnyCardQuery a) (f a)
  | r
  }

-- | Makes a prism for `_Query` for card components with a query algebra of the
-- | form `Coproduct CardEvalQuery f`.
-- |
-- | This applies to two types of card components:
-- |
-- | 1. Parent components where the parent uses `CardEvalQuery` directly, in
-- |    which case `f` will be some `ChildF`.
-- | 2. Self contained components with an enriched query algebra, where `f`
-- |    will be the component's own internal algebra.
makeQueryPrism
  ∷ ∀ a f
   . Prism'
       (AnyCardQuery a)
       (Coproduct CardEvalQuery f a)
  → Prism'
       (Coproduct CardEvalQuery AnyCardQuery a)
       (Coproduct CardEvalQuery f a)
makeQueryPrism base = prism' to fro
  where
  to
    ∷ Coproduct CardEvalQuery f a
    → Coproduct CardEvalQuery AnyCardQuery a
  to = coproduct left (right ∘ review base ∘ right)
  fro
    ∷ Coproduct CardEvalQuery AnyCardQuery a
    → Maybe (Coproduct CardEvalQuery f a)
  fro = coproduct (Just ∘ left) (preview base)

-- | Makes a prism for `_Query` for card components with a query algebra of the
-- | form `Coproduct (Coproduct CardEvalQuery f) g`.
-- |
-- | This will occurs when a card component is a parent component and also has
-- | an enriched query algebra, where `f` is the component's internal query
-- | algebra and `g` will be some `ChildF`.
makeQueryPrism'
  ∷ ∀ a f g
  . Prism'
      (AnyCardQuery a)
      (Coproduct (Coproduct CardEvalQuery f) g a)
  → Prism'
      (Coproduct CardEvalQuery AnyCardQuery a)
      (Coproduct (Coproduct CardEvalQuery f) g a)
makeQueryPrism' base = prism' to fro
  where
  to
    ∷ Coproduct (Coproduct CardEvalQuery f) g a
    → Coproduct CardEvalQuery AnyCardQuery a
  to =
    coproduct
      (coproduct
         left
         (right ∘ review base ∘ left ∘ right))
      (right ∘ review base ∘ right)
  fro
    ∷ Coproduct CardEvalQuery AnyCardQuery a
    → Maybe (Coproduct (Coproduct CardEvalQuery f) g a)
  fro =
    coproduct
      (Just ∘ left ∘ left)
      (preview base)
