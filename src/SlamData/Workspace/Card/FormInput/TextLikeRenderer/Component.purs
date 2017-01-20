{-
Copyright 2016 SlamData, Inc.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
-}

module SlamData.Workspace.Card.FormInput.TextLikeRenderer.Component where

import SlamData.Prelude

import Data.Argonaut (JCursor(..))
import Data.Time.Duration (Milliseconds(..))

import Halogen as H
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP
import Halogen.Themes.Bootstrap3 as B
import Halogen.CustomProps as Cp
import Halogen.Component.Utils (raise, sendAfter)

import SlamData.Monad (Slam)
import SlamData.Workspace.Card.Port (SetupTextLikeFormInputPort)
import SlamData.Workspace.Card.FormInput.TextLikeRenderer.Model as M
import SlamData.Workspace.Card.CardType.FormInputType (FormInputType(..))

type State =
  { label ∷ Maybe String
  , value ∷ String
  , formInputType ∷ FormInputType
  , cursor ∷ JCursor
  }

initialState ∷ State
initialState =
  { label: Nothing
  , value: ""
  , formInputType: Text
  , cursor: JCursorTop
  }

data Query a
  = Setup SetupTextLikeFormInputPort a
  | ValueChanged String a
  | Save (M.Model → a)
  | Load M.Model a
  | Updated a

type DSL = H.ComponentDSL State Query Slam
type HTML = H.ComponentHTML Query

comp ∷ H.Component State Query Slam
comp = H.component { render, eval }

render ∷ State → HTML
render state =
  HH.form
    [ Cp.nonSubmit ]
    $ foldMap (\n → [ HH.h3_  [ HH.text n ] ]) state.label
    ⊕ [ HH.input
          [ HP.classes [ B.formControl ]
          , HP.value state.value
          , HP.inputType $ inputTypeFromFIT state.formInputType
          , HE.onValueInput $ HE.input ValueChanged
          ]
      ]

eval ∷ Query ~> DSL
eval (ValueChanged s next) = do
  H.modify _{ value = s }
  raise $ H.action Updated
  pure next
eval (Setup p next) = do
  st ← H.get
  H.modify _
    { label = if p.name ≡ "" then Nothing else Just p.name
    , formInputType = p.formInputType
    , cursor = p.cursor
    -- If cursor field is changed we remove user selected value
    -- I'm not sure that this is the most convenient way of
    -- doing this, but empty data produced by incorrect `value` here is kinda weird
    , value = if p.cursor ≠ st.cursor then "" else st.value
    }
  when (st.cursor ≠ p.cursor) $ void $ sendAfter (Milliseconds 200.0) $ H.action Updated
  pure next
eval (Save cont) = do
  st ← H.get
  pure
    $ cont
      { value: st.value
      , cursor: st.cursor
      , formInputType: st.formInputType
      }
eval (Load m next) = do
  H.modify _
    { value = m.value
    , formInputType = m.formInputType
    , cursor = m.cursor
    }
  pure next
eval (Updated next) =
  pure next

inputTypeFromFIT ∷ FormInputType → HP.InputType
inputTypeFromFIT = case _ of
  Numeric → HP.InputNumber
  Date → HP.InputDate
  Time → HP.InputTime
  Datetime → HP.InputDatetime
  _ → HP.InputText
