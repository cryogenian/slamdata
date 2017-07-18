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

module SlamData.Workspace.FormBuilder.Item.Component
  ( Query(..)
  , Message(..)
  , module State
  , component
  ) where

import SlamData.Prelude

import DOM.HTML.Indexed as DI
import DOM.HTML.Indexed.StepValue (StepValue(..))
import Data.BrowserFeatures.InputType as IT
import Data.DateTime as DT
import Data.Either.Nested (Either3)
import Data.Formatter.DateTime as FD
import Data.Functor.Coproduct.Nested (Coproduct3)
import Data.Lens ((^?))
import Data.Lens as Lens
import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.Datepicker.Component.Date as DatePicker
import Halogen.Datepicker.Component.DateTime as DateTimePicker
import Halogen.Datepicker.Component.Time as TimePicker
import Halogen.Datepicker.Component.Types (PickerMessage(..), setValue, value)
import Halogen.HTML as HH
import Halogen.HTML.Core as HC
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as ARIA
import SlamData.Monad (Slam)
import SlamData.Wiring as Wiring
import SlamData.Workspace.FormBuilder.Item.Component.State (FieldName(..), Model, State, getModel, putModel)
import SlamData.Workspace.FormBuilder.Item.Component.State as State
import SlamData.Workspace.FormBuilder.Item.FieldType (FieldType(..), _FieldTypeDisplayName, allFieldTypes, fieldTypeToInputType)
import SlamData.Workspace.PickerUtils (config, datePickerFormat, dateTimePickerFormat, timePickerFormat)


data Query a
  = UpdateName String a
  | UpdateFieldType String a
  | UpdateDefaultValue String a
  | SetModel Model a
  | GetModel (Model → a)
  | Init a

data Message
  = NameChanged String
  | FieldTypeChanged String
  | DefaultValueChanged String

type ChildQuery = Coproduct3 DatePicker.Query TimePicker.Query DateTimePicker.Query
type Slot = Either3 Unit Unit Unit

cpDatePicker ∷ CP.ChildPath DatePicker.Query ChildQuery Unit Slot
cpDatePicker = CP.cp1

cpTimePicker ∷ CP.ChildPath TimePicker.Query ChildQuery Unit Slot
cpTimePicker = CP.cp2

cpDateTimePicker ∷ CP.ChildPath DateTimePicker.Query ChildQuery Unit Slot
cpDateTimePicker = CP.cp3


type HTML m = H.ParentHTML Query ChildQuery Slot m
type DSL = H.ParentDSL State Query ChildQuery Slot Message Slam

component ∷ H.Component HH.HTML Query Unit Message Slam
component =
  H.lifecycleParentComponent
    { initialState: const State.initialState
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
  HH.tr_
    [ HH.td_ [ nameField ]
    , HH.td_ [ typeField ]
    , HH.td_ [ defaultField ]
    ]

  where
  nameField ∷ HTML m
  nameField =
    HH.input
      [ HP.type_ HP.InputText
      , HP.title "Field Name"
      , ARIA.label "Variable name"
      , HP.value (unwrap state.name)
      , HE.onValueInput (HE.input UpdateName)
      , HP.placeholder "Variable name"
      ]

  quotedName ∷ FieldName → String
  quotedName (FieldName "") = ""
  quotedName (FieldName s) = "\"" <> s <> "\""

  typeField ∷ HTML m
  typeField =
    HH.select
      [ HE.onValueChange (HE.input UpdateFieldType)
      , ARIA.label $ "Type of " <> (quotedName state.name) <> " variable"
      ]
      (typeOption <$> allFieldTypes)

  typeOption
    ∷ FieldType
    → HTML m
  typeOption ty =
    HH.option
    [ HP.selected $ ty == state.fieldType ]
    [ HH.text $ Lens.review _FieldTypeDisplayName ty ]
  defaultField ∷ HTML m
  defaultField =
    case state.fieldType of
      DateTimeFieldType | not state.inputTypeSupported IT.DateTimeLocal →
        HH.slot'
          cpDateTimePicker
          unit
          (DateTimePicker.pickerWithConfig config dateTimePickerFormat)
          unit
          $ HE.input
          $ \(NotifyChange n) →
            UpdateDefaultValue $ fromMaybe "" $ value n >>= formatDateTime
      DateFieldType | not state.inputTypeSupported IT.Date →
        HH.slot'
          cpDatePicker
          unit
          (DatePicker.pickerWithConfig config datePickerFormat)
          unit
          $ HE.input
          $ \(NotifyChange n) →
            UpdateDefaultValue $ fromMaybe "" $ value n >>= formatDate
      TimeFieldType | not state.inputTypeSupported IT.Time →
        HH.slot'
          cpTimePicker
          unit
          (TimePicker.pickerWithConfig config timePickerFormat)
          unit
          $ HE.input
          $ \(NotifyChange n) →
            UpdateDefaultValue $ fromMaybe "" $ value n >>= formatTime
      BooleanFieldType →
        HH.label
          [ HP.class_ (HH.ClassName "sd-option") ]
          [ HH.input
              [ HP.type_ inputType
              , HP.checked
                  $ fromMaybe false
                  $ Lens.preview _StringBoolean
                  =<< state.defaultValue
              , HE.onChecked
                  $ HE.input (UpdateDefaultValue ∘ Lens.review _StringBoolean)
              , ARIA.label
                  $ "Default value of "
                  <> (quotedName state.name)
                  <> " variable is \"true\""
              ]
          , HH.span_ [ HH.text (unwrap state.name) ]
          ]
      _ →
          HH.input
            $ fieldType
            <> [ HP.value (maybe "" (State.sanitiseValueForForm state.fieldType) state.defaultValue)
               , HE.onValueInput (HE.input UpdateDefaultValue ∘ State.sanitiseValueFromForm state.fieldType)
               , ARIA.label lbl
               , HP.placeholder lbl
               ]

    where
    lbl ∷ String
    lbl
      = "Default value"
      <> if state.name /= (FieldName "") then " for " <> (quotedName state.name) <> " variable" else ""
    inputType =
      fieldTypeToInputType state.fieldType

    fieldType ∷ ∀ i. Array (HP.IProp DI.HTMLinput i)
    fieldType = case state.fieldType of
      DateTimeFieldType →
        [ HP.type_ inputType
        , secondsStep
        ]
      TimeFieldType →
        [ HP.type_ inputType
        , secondsStep
        ]
      _ →
        [ HP.type_ inputType ]

    secondsStep ∷ ∀ r i. HP.IProp (step ∷ StepValue | r) i
    secondsStep = HP.prop (HC.PropName "step") (Step 1.0)

    _StringBoolean ∷ Lens.Prism' String Boolean
    _StringBoolean = Lens.prism re pre
      where
        re b = if b then "true" else "false"
        pre "true" = Right true
        pre "false" = Right false
        pre str = Left str

eval ∷ Query ~> DSL
eval = case _ of
  Init next → do
    w ← H.lift Wiring.expose
    H.modify _{inputTypeSupported = w.browserFeatures.inputTypeSupported}
    pure next
  UpdateName str next → do
    H.modify $ _ { name = FieldName str }
    H.raise $ NameChanged str
    pure next
  UpdateFieldType str next → do
    for_ (str ^? _FieldTypeDisplayName) \ty → do
      H.modify $ _ { fieldType = ty }
    H.raise $ FieldTypeChanged str
    pure next
  UpdateDefaultValue str next → do
    H.modify $ _ { defaultValue = Just str }
    H.raise $ DefaultValueChanged str
    pure next
  SetModel m next → do
    s ← H.get
    H.modify $ putModel m
    void case m.fieldType of
      DateTimeFieldType | not s.inputTypeSupported IT.DateTimeLocal →
        H.query' cpDateTimePicker unit
        $ setValue
        $ m.defaultValue >>= unformatDateTime <#> Right
      DateFieldType | not s.inputTypeSupported IT.Date →
        H.query' cpDatePicker unit
        $ setValue
        $ m.defaultValue >>= unformatDate <#> Right
      TimeFieldType | not s.inputTypeSupported IT.Time →
        H.query' cpTimePicker unit
        $ setValue
        $ m.defaultValue >>= unformatTime <#> Right
      _ -> pure (Just unit)
    pure next
  GetModel k →
    k <$> H.gets getModel


-- We need to sanitise value (`sanitiseValue{From,For}Form`) for native
-- date, time and dateTime input, but we don't need to do it with picker
-- as by using this formats we match what database takes and returns.

-- NOTE prism can be used here
formatDateTime ∷ DT.DateTime → Maybe String
formatDateTime x = hush $ FD.formatDateTime "YYYY-MM-DDTHH:mm:ssZ" x

unformatDateTime ∷ String → Maybe DT.DateTime
unformatDateTime x = hush $ FD.unformatDateTime "YYYY-MM-DDTHH:mm:ssZ" x

formatDate ∷ DT.Date → Maybe String
formatDate x = hush $ FD.formatDateTime "YYYY-MM-DDTHH:mm:ssZ" $ DT.DateTime x bottom

unformatDate ∷ String → Maybe DT.Date
unformatDate x = hush $ map DT.date $ FD.unformatDateTime "YYYY-MM-DDTHH:mm:ssZ" x

formatTime ∷ DT.Time → Maybe String
formatTime x = hush $ FD.formatDateTime "HH:mm:ss.SSS" $ DT.DateTime bottom x

unformatTime ∷ String → Maybe DT.Time
unformatTime x = hush $ map DT.time $ FD.unformatDateTime "HH:mm:ss.SSS" x
