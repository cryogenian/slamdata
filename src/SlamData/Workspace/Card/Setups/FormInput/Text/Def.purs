module SlamData.Workspace.Card.Setups.FormInput.Text.Def where

import SlamData.Prelude

import SlamData.Workspace.Card.Setups.FormInput.TextLike.Def (TextLikeDef)
import SlamData.Workspace.Card.Component.State as CCS
import SlamData.Workspace.Card.Component.Query as CCQ
import SlamData.Workspace.Card.Component.Def (makeQueryPrism')

def ∷ TextLikeDef
def =
  { _State: CCS._SetupTextState
  , _Query: makeQueryPrism' CCQ._SetupTextQuery
  , valueProjection: \ax →
      ax.value
      ⊕ ax.category
      ⊕ ax.time
      ⊕ ax.date
      ⊕ ax.datetime
  }
