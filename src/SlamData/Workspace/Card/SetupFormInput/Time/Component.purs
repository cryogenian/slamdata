module SlamData.Workspace.Card.SetupFormInput.Time.Component
  ( timeSetupComponent
  ) where

import Halogen as H

import SlamData.Monad (Slam)
import SlamData.Workspace.Card.CardType.FormInputType as FIT
import SlamData.Workspace.Card.Component as CC
import SlamData.Workspace.Card.Component.State as CCS
import SlamData.Workspace.Card.Component.Query as CCQ
import SlamData.Workspace.Card.SetupFormInput.TextLike.Component (textLikeSetupComponent)

timeSetupComponent ∷ H.Component CC.CardStateP CC.CardQueryP Slam
timeSetupComponent =
  textLikeSetupComponent
    FIT.Time
    { _State: CCS._SetupTimeState
    , _Query: CC.makeQueryPrism' CCQ._SetupTimeQuery
    , valueProjection: \ax →
        ax.time
    , labelProjection: \ax →
        ax.category
    }
