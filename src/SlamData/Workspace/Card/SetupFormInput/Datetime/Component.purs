module SlamData.Workspace.Card.SetupFormInput.Datetime.Component
  ( datetimeSetupComponent
  ) where

import Halogen as H

import SlamData.Monad (Slam)
import SlamData.Workspace.Card.CardType.FormInputType as FIT
import SlamData.Workspace.Card.Component as CC
import SlamData.Workspace.Card.Component.State as CCS
import SlamData.Workspace.Card.Component.Query as CCQ
import SlamData.Workspace.Card.SetupFormInput.TextLike.Component (textLikeSetupComponent)

datetimeSetupComponent ∷ H.Component CC.CardStateP CC.CardQueryP Slam
datetimeSetupComponent =
  textLikeSetupComponent
    FIT.Datetime
    { _State: CCS._SetupDatetimeState
    , _Query: CC.makeQueryPrism' CCQ._SetupDatetimeQuery
    , valueProjection: \ax →
        ax.datetime
    , labelProjection: \ax →
        ax.category
    }
