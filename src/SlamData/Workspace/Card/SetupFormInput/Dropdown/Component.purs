module SlamData.Workspace.Card.SetupFormInput.Dropdown.Component
  ( dropdownSetupComponent
  ) where

import Halogen as H

import SlamData.Monad (Slam)
import SlamData.Workspace.Card.CardType.FormInputType as FIT
import SlamData.Workspace.Card.Component as CC
import SlamData.Workspace.Card.Component.State as CCS
import SlamData.Workspace.Card.Component.Query as CCQ
import SlamData.Workspace.Card.SetupFormInput.Labeled.Component (labeledSetupComponent)

dropdownSetupComponent ∷ CC.CardOptions → H.Component CC.CardStateP CC.CardQueryP Slam
dropdownSetupComponent =
  labeledSetupComponent
    FIT.Dropdown
    { _State: CCS._SetupDropdownState
    , _Query: CC.makeQueryPrism' CCQ._SetupDropdownQuery
    }
