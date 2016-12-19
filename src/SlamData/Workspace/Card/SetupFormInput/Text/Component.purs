module SlamData.Workspace.Card.SetupFormInput.Text.Component
  ( textSetupComponent
  ) where

import SlamData.Prelude

import Halogen as H

import SlamData.Monad (Slam)
import SlamData.Workspace.Card.CardType.FormInputType as FIT
import SlamData.Workspace.Card.Component as CC
import SlamData.Workspace.Card.Component.State as CCS
import SlamData.Workspace.Card.Component.Query as CCQ
import SlamData.Workspace.Card.SetupFormInput.TextLike.Component (textLikeSetupComponent)

textSetupComponent ∷ CC.CardOptions → H.Component CC.CardStateP CC.CardQueryP Slam
textSetupComponent =
  textLikeSetupComponent
    FIT.Text
    { _State: CCS._SetupTextState
    , _Query: CC.makeQueryPrism' CCQ._SetupTextQuery
    , valueProjection: \ax →
        ax.value
        ⊕ ax.category
        ⊕ ax.time
        ⊕ ax.date
        ⊕ ax.datetime
    }
