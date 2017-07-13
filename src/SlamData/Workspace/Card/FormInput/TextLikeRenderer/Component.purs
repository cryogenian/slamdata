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
import Data.Variant (on)

import DOM.Classy.Event as DOM
import DOM.Event.Types (Event)

import Halogen as H
import Halogen.Component.Utils (sendAfter)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

import SlamData.Monad (Slam)
import SlamData.Render.ClassName as CN
import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.FormInput.TextLikeRenderer.Model as M
import SlamData.Workspace.Card.Port (SetupInputPort)

type State =
  { label ∷ Maybe String
  , value ∷ String
  , formInputType ∷ CT.Input ()
  , cursor ∷ JCursor
  }

initialState ∷ State
initialState =
  { label: Nothing
  , value: ""
  , formInputType: CT.text
  , cursor: JCursorTop
  }

data Query a
  = Setup SetupInputPort a
  | ValueChanged String a
  | Save (M.Model → a)
  | Load M.Model a
  | PreventDefault Event a
  | RaiseUpdated a

data Message = Updated

type DSL = H.ComponentDSL State Query Message Slam
type HTML = H.ComponentHTML Query

comp ∷ H.Component HH.HTML Query Unit Message Slam
comp =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }

render ∷ State → HTML
render state =
  HH.form
    [ HE.onSubmit (HE.input PreventDefault) ]
    $ foldMap (\n → [ HH.h3_  [ HH.text n ] ]) state.label
    ⊕ [ HH.input
          [ HP.classes [ CN.formControl ]
          , HP.value state.value
          , HP.type_ $ inputTypeFromFIT state.formInputType
          , HE.onValueInput $ HE.input ValueChanged
          ]
      ]

eval ∷ Query ~> DSL
eval = case _ of
  ValueChanged s next → do
    H.modify _{ value = s }
    H.raise Updated
    pure next
  Setup p next → do
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
    when (st.cursor ≠ p.cursor) $
      void $ sendAfter (Milliseconds 200.0) RaiseUpdated
    pure next
  Save cont → do
    st ← H.get
    pure
      $ cont
        { value: st.value
        , cursor: st.cursor
        , formInputType: st.formInputType
        }
  Load m next → do
    H.modify _
      { value = m.value
      , formInputType = m.formInputType
      , cursor = m.cursor
      }
    pure next
  PreventDefault ev next → do
    H.liftEff $ DOM.preventDefault ev
    pure next
  RaiseUpdated next → do
    H.raise Updated
    pure next

inputTypeFromFIT ∷ CT.Input () → HP.InputType
inputTypeFromFIT = case_
  # on CT._numeric (const $ HP.InputNumber)
  # on CT._date (const $ HP.InputDate)
  # on CT._time (const $ HP.InputTime)
  # on CT._datetime (const $ HP.InputDatetime)
  # on CT._text (const $ HP.InputText)
