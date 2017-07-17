module SlamData.Workspace.Card.Setups.Auxiliary.Algebra where

import SlamData.Prelude

data SetF a = Set String a
derive instance functorSetF ∷ Functor SetF

data MinMaxF a = Max String a | Min String a
derive instance functorMinMaxF ∷ Functor MinMaxF

data ToggleF a = Toggle a
derive instance functorToggleF ∷ Functor ToggleF

data ChooseF val a = Choose val a
derive instance functorChooseF ∷ Functor (ChooseF a)
