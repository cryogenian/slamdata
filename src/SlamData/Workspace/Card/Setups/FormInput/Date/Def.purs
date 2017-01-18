module SlamData.Workspace.Card.Setups.FormInput.Date.Def where

import SlamData.Workspace.Card.Setups.FormInput.TextLike.Def (TextLikeDef)
import SlamData.Workspace.Card.Component.State as CCS
import SlamData.Workspace.Card.Component.Query as CCQ
import SlamData.Workspace.Card.Component.Def (makeQueryPrism')

def ∷ TextLikeDef
def =
  { _State: CCS._SetupDateState
  , _Query: makeQueryPrism' CCQ._SetupDateQuery
  , valueProjection: \ax →
      ax.date
  }
