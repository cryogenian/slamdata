module SlamData.Workspace.Card.SetupFormInput.Numeric.Component
  ( numericSetupComponent
  ) where

import Halogen as H

import SlamData.Monad (Slam)
import SlamData.Workspace.Card.CardType.FormInputType as FIT
import SlamData.Workspace.Card.Component as CC
import SlamData.Workspace.Card.Component.State as CCS
import SlamData.Workspace.Card.Component.Query as CCQ
import SlamData.Workspace.Card.SetupFormInput.TextLike.Component (textLikeSetupComponent)

numericSetupComponent ∷ CC.CardOptions → H.Component CC.CardStateP CC.CardQueryP Slam
numericSetupComponent =
  textLikeSetupComponent
    FIT.Numeric
    { _State: CCS._SetupNumericState
    , _Query: CC.makeQueryPrism' CCQ._SetupNumericQuery
    , valueProjection: \ax →
        ax.value
    }
