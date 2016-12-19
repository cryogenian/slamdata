module SlamData.Workspace.Card.SetupFormInput.Radio.Component
  ( radioSetupComponent
  ) where

import Halogen as H

import SlamData.Monad (Slam)
import SlamData.Workspace.Card.CardType.FormInputType as FIT
import SlamData.Workspace.Card.Component as CC
import SlamData.Workspace.Card.Component.State as CCS
import SlamData.Workspace.Card.Component.Query as CCQ
import SlamData.Workspace.Card.SetupFormInput.Labeled.Component (labeledSetupComponent)

radioSetupComponent ∷ CC.CardOptions → H.Component CC.CardStateP CC.CardQueryP Slam
radioSetupComponent =
  labeledSetupComponent
    FIT.Radio
    { _State: CCS._SetupRadioState
    , _Query: CC.makeQueryPrism' CCQ._SetupRadioQuery
    }
