module SlamData.Workspace.Card.FormInput.TextLikeRenderer.Component where

import SlamData.Prelude

import Halogen as H
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP
import Halogen.Themes.Bootstrap3 as B
import Halogen.CustomProps as Cp

import SlamData.Monad (Slam)
import SlamData.Workspace.Card.Port (SetupTextLikeFormInputPort)
import SlamData.Workspace.Card.CardType.FormInputType (FormInputType(..))

type State =
  { label ∷ Maybe String
  , value ∷ String
  , inputType ∷ HP.InputType
  }

initialState ∷ State
initialState =
  { label: Nothing
  , value: ""
  , inputType: HP.InputText
  }

data Query a
  = Setup SetupTextLikeFormInputPort a
  | ValueChanged String a
  | GetValue (String → a)

type DSL = H.ComponentDSL State Query Slam
type HTML = H.ComponentHTML Query

comp ∷ H.Component State Query Slam
comp = H.component { render, eval }

render ∷ State → HTML
render state =
  HH.form
    [ Cp.nonSubmit ]
    $ foldMap (\n → [ HH.label [ HP.classes [ B.controlLabel ] ] [ HH.text n ] ]) state.label
    ⊕ [ HH.input
          [ HP.classes [ B.formControl ]
          , HP.value state.value
          , HP.inputType state.inputType
          , HE.onValueInput $ HE.input ValueChanged
          ]
      ]

eval ∷ Query ~> DSL
eval (ValueChanged s next) = do
  H.modify _{ value = s }
  pure next
eval (Setup p next) = do
  H.modify _
    { label = if p.name ≡ "" then Nothing else Just p.name
    , inputType = case p.formInputType of
         Numeric → HP.InputNumber
         Date → HP.InputDate
         Time → HP.InputTime
         Datetime → HP.InputDatetime
         _ → HP.InputText
    }
  pure next
eval (GetValue cont) =
  map cont $ H.gets _.value
