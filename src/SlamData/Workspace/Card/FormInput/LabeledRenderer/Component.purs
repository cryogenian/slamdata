module SlamData.Workspace.Card.FormInput.LabeledRenderer.Component where

import SlamData.Prelude

import Halogen as H
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP
import Halogen.Themes.Bootstrap3 as B
import Halogen.CustomProps as Cp

import SlamData.Monad (Slam)
import SlamData.Workspace.Card.BuildChart.Semantics as Sem
import SlamData.Workspace.Card.Port (SetupLabeledFormInputPort)
import SlamData.Workspace.Card.CardType.FormInputType (FormInputType(..))

type State =
  {
  }

initialState ∷ State
initialState =
  {
  }

data Query a
  = Setup SetupLabeledFormInputPort a
  | ItemSelected Sem.Semantics a

type DSL = H.ComponentDSL State Query Slam
type HTML = H.ComponentHTML Query

comp ∷ H.Component State Query Slam
comp = H.component { render, eval }

render ∷ State → HTML
render state = HH.text "TODO"

eval ∷ Query ~> DSL
eval (Setup _ next) = pure next
eval (ItemSelected _ next) = pure next
