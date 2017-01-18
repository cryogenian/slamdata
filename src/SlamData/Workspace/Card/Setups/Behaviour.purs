module SlamData.Workspace.Card.Setups.Behaviour where

import SlamData.Prelude

type Behaviour s m =
  { synchronize ∷ s → s
  , load ∷ m → s → s
  , save ∷ s → m
  }

defaultModel ∷ ∀ m s. Behaviour s m → m → s → m
defaultModel b m s = b.save $ b.synchronize $ b.load m s
