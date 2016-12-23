module SlamData.Workspace.Card.FormInput.LabeledRenderer.Component where

import SlamData.Prelude

import Data.Set as Set
import Data.Map as Map

import Halogen as H
--import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Indexed as HH
--import Halogen.HTML.Properties.Indexed as HP
--import Halogen.Themes.Bootstrap3 as B
import Halogen.CustomProps as Cp

import SlamData.Monad (Slam)
import SlamData.Workspace.Card.BuildChart.Semantics as Sem
import SlamData.Workspace.Card.Port (SetupLabeledFormInputPort)
import SlamData.Workspace.Card.CardType.FormInputType (FormInputType(..))

type State =
  { formInputType ∷ FormInputType
  , selected ∷ Set.Set Sem.Semantics
  , valueLabelMap ∷ Map.Map Sem.Semantics (Maybe String)
  , label ∷ Maybe String
  }

initialState ∷ State
initialState =
  { formInputType: Dropdown
  , selected: Set.empty
  , valueLabelMap: Map.empty
  , label: Nothing
  }

data Query a
  = Setup SetupLabeledFormInputPort a
  | ItemSelected Sem.Semantics a

type DSL = H.ComponentDSL State Query Slam
type HTML = H.ComponentHTML Query

comp ∷ H.Component State Query Slam
comp = H.component { render, eval }

render ∷ State → HTML
render state =
  HH.form [ Cp.nonSubmit ]
    $ foldMap (\n → [ HH.h3_ [ HH.text n ] ]) state.label
    ⊕ case state.formInputType of
      Dropdown → renderDropdown state
      Checkbox → renderCheckbox state
      Radio → renderRadio state
      _ → [ ]

renderDropdown ∷ State → Array HTML
renderDropdown state =
  [ HH.text "TODO" ]

renderCheckbox ∷ State → Array HTML
renderCheckbox state =
  [ HH.text "TODO" ]

renderRadio ∷ State → Array HTML
renderRadio state =
  [ HH.text "TODO" ]

eval ∷ Query ~> DSL
eval (Setup conf next) = do
  H.modify _
    { formInputType = conf.formInputType
    , selected =  conf.selectedValues
    , valueLabelMap = conf.valueLabelMap
    , label = if conf.name ≡ "" then Nothing else Just conf.name
    }
  pure next
eval (ItemSelected sem next) = do
  st ← H.get
  case st.formInputType of
    Checkbox → H.modify _{ selected = Set.insert sem st.selected }
    _ → H.modify _{ selected = Set.singleton sem }
  pure next
