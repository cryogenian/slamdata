module SlamData.Workspace.Card.SetupFormInput.Date.Component
  ( dateSetupComponent
  ) where

import Halogen as H

import SlamData.Monad (Slam)
import SlamData.Workspace.Card.CardType.FormInputType as FIT
import SlamData.Workspace.Card.Component as CC
import SlamData.Workspace.Card.Component.State as CCS
import SlamData.Workspace.Card.Component.Query as CCQ
import SlamData.Workspace.Card.SetupFormInput.TextLike.Component (textLikeSetupComponent)

dateSetupComponent ∷ H.Component CC.CardStateP CC.CardQueryP Slam
dateSetupComponent =
  textLikeSetupComponent
    FIT.Date
    { _State: CCS._SetupDateState
    , _Query: CC.makeQueryPrism' CCQ._SetupDateQuery
    , valueProjection: \ax →
        ax.date
    , labelProjection: \ax →
        ax.category
    }
