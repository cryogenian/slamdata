module SlamData.Workspace.Card.Setups.FormInput.Time.Def where

import SlamData.Workspace.Card.Setups.FormInput.TextLike.Def (TextLikeDef)
import SlamData.Workspace.Card.Component.State as CCS
import SlamData.Workspace.Card.Component.Query as CCQ
import SlamData.Workspace.Card.Component.Def (makeQueryPrism')

def ∷ TextLikeDef
def =
  { _State: CCS._SetupTimeState
  , _Query: makeQueryPrism' CCQ._SetupTimeQuery
  , valueProjection: \ax →
      ax.time
  }
