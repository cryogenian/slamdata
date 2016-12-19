module SlamData.Workspace.Card.SetupFormInput.Checkbox.Component
  ( checkboxSetupComponent
  ) where

import Halogen as H

import SlamData.Monad (Slam)
import SlamData.Workspace.Card.CardType.FormInputType as FIT
import SlamData.Workspace.Card.Component as CC
import SlamData.Workspace.Card.Component.State as CCS
import SlamData.Workspace.Card.Component.Query as CCQ
import SlamData.Workspace.Card.SetupFormInput.Labeled.Component (labeledSetupComponent)

checkboxSetupComponent ∷ CC.CardOptions → H.Component CC.CardStateP CC.CardQueryP Slam
checkboxSetupComponent =
  labeledSetupComponent
    FIT.Checkbox
    { _State: CCS._SetupCheckboxState
    , _Query: CC.makeQueryPrism' CCQ._SetupCheckboxQuery
    }
