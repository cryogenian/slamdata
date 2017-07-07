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

import DOM.Classy.Event as DOM
import DOM.Event.Types (Event)
import Data.Argonaut (JCursor(..))
import Data.BrowserFeatures.InputType (InputType)
import Data.BrowserFeatures.InputType as IT
import Data.Either.Nested (Either3)
import Data.Functor.Coproduct.Nested (Coproduct3)
import Data.Time.Duration (Milliseconds(..))
import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.Component.Utils (sendAfter)
import Halogen.Datepicker.Component.Date as DatePicker
import Halogen.Datepicker.Component.DateTime as DateTimePicker
import Halogen.Datepicker.Component.Time as TimePicker
import Halogen.Datepicker.Component.Types (PickerMessage(..), setValue, value)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import SlamData.Monad (Slam)
import SlamData.Render.ClassName as CN
import SlamData.Wiring as Wiring
import SlamData.Workspace.Card.CardType.FormInputType (FormInputType(..))
import SlamData.Workspace.Card.FormInput.TextLikeRenderer.Model as M
import SlamData.Workspace.Card.Port (SetupTextLikeFormInputPort)
import SlamData.Workspace.PickerUtils (datePickerFormat, dateTimePickerFormat, formatDate, formatDateTime, formatTime, timePickerFormat, unformatDate, unformatDateTime, unformatTime)
import Text.Markdown.SlamDown.Halogen.Component (defaultBrowserFeatures)

type State =
  { label ∷ Maybe String
  , value ∷ String
  , formInputType ∷ FormInputType
  , inputTypeSupported ∷ InputType → Boolean
  , cursor ∷ JCursor
  }

initialState ∷ State
initialState =
  { label: Nothing
  , value: ""
  , formInputType: Text
  , cursor: JCursorTop
  , inputTypeSupported: defaultBrowserFeatures.inputTypeSupported
  }

data Query a
  = Setup SetupTextLikeFormInputPort a
  | ValueChanged String a
  | Save (M.Model → a)
  | Load M.Model a
  | PreventDefault Event a
  | RaiseUpdated a
  | Init a

data Message = Updated

type ChildQuery = Coproduct3 DatePicker.Query TimePicker.Query DateTimePicker.Query
type Slot = Either3 Unit Unit Unit

cpDatePicker ∷ CP.ChildPath DatePicker.Query ChildQuery Unit Slot
cpDatePicker = CP.cp1

cpTimePicker ∷ CP.ChildPath TimePicker.Query ChildQuery Unit Slot
cpTimePicker = CP.cp2

cpDateTimePicker ∷ CP.ChildPath DateTimePicker.Query ChildQuery Unit Slot
cpDateTimePicker = CP.cp3

type DSL = H.ParentDSL State Query ChildQuery Slot Message Slam
type HTML m = H.ParentHTML Query ChildQuery Slot m

comp ∷ H.Component HH.HTML Query Unit Message Slam
comp =
  H.lifecycleParentComponent
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    , initializer: Just (H.action Init)
    , finalizer: Nothing
    }


render ∷ ∀ m
  . State
  → HTML m
render state =
  HH.form
    [ HE.onSubmit (HE.input PreventDefault) ]
    $ foldMap (\n → [ HH.h3_  [ HH.text n ] ]) state.label
    ⊕ [
      case state.formInputType of
        Datetime | not state.inputTypeSupported IT.DateTimeLocal →
          HH.slot'
            cpDateTimePicker
            unit
            (DateTimePicker.picker dateTimePickerFormat)
            unit
            $ HE.input
            $ \(NotifyChange n) →
              ValueChanged $ fromMaybe "" $ value n >>= formatDateTime
        Date | not state.inputTypeSupported IT.Date →
          HH.slot'
            cpDatePicker
            unit
            (DatePicker.picker datePickerFormat)
            unit
            $ HE.input
            $ \(NotifyChange n) →
              ValueChanged $ fromMaybe "" $ value n >>= formatDate
        Time | not state.inputTypeSupported IT.Time →
          HH.slot'
            cpTimePicker
            unit
            (TimePicker.picker timePickerFormat)
            unit
            $ HE.input
            $ \(NotifyChange n) →
              ValueChanged $ fromMaybe "" $ value n >>= formatTime
        _ →
          HH.input
            [ HP.classes [ CN.formControl ]
            , HP.value state.value
            , HP.type_ $ inputTypeFromFIT state.formInputType
            , HE.onValueInput $ HE.input ValueChanged
            ]
      ]

eval ∷ Query ~> DSL
eval = case _ of
  Init next → do
    w ← H.lift Wiring.expose
    H.modify _{inputTypeSupported = w.browserFeatures.inputTypeSupported}
    pure next
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
    s ← H.get
    void case s.formInputType of
      Datetime | not s.inputTypeSupported IT.DateTimeLocal →
        H.query' cpDateTimePicker unit
        $ setValue
        $ Right <$> unformatDateTime s.value
      Date | not s.inputTypeSupported IT.Date →
        H.query' cpDatePicker unit
        $ setValue
        $ Right <$> unformatDate s.value
      Time | not s.inputTypeSupported IT.Time →
        H.query' cpTimePicker unit
        $ setValue
        $ Right <$> unformatTime s.value
      _ -> pure (Just unit)
    pure next
  PreventDefault ev next → do
    H.liftEff $ DOM.preventDefault ev
    pure next
  RaiseUpdated next → do
    H.raise Updated
    pure next

inputTypeFromFIT ∷ FormInputType → HP.InputType
inputTypeFromFIT = case _ of
  Numeric → HP.InputNumber
  Date → HP.InputDate
  Time → HP.InputTime
  Datetime → HP.InputDatetime
  _ → HP.InputText
