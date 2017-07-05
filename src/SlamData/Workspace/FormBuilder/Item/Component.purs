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
import Data.BrowserFeatures (BrowserFeatures)
import Data.BrowserFeatures.InputType as IT
import Data.DateTime as DT
import Data.Either (hush)
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
import Halogen.Datepicker.Format.Date as DatePickerF
import Halogen.Datepicker.Format.DateTime as DateTimePickerF
import Halogen.Datepicker.Format.Time as TimePickerF
import Halogen.HTML as HH
import Halogen.HTML.Core as HC
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as ARIA
import SlamData.Workspace.FormBuilder.Item.Component.State (FieldName(..), Model, State)
import SlamData.Workspace.FormBuilder.Item.Component.State as State
import SlamData.Workspace.FormBuilder.Item.FieldType (FieldType(..), _FieldTypeDisplayName, allFieldTypes, fieldTypeToInputType)

data Query a
  = UpdateName String a
  | UpdateFieldType String a
  | UpdateDefaultValue String a
  | SetModel Model a
  | GetModel (Model → a)

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
type DSL m = H.ParentDSL State Query ChildQuery Slot Message m

component ∷ ∀ m. BrowserFeatures → H.Component HH.HTML Query Unit Message m
component browserFeatures =
  H.parentComponent
    { initialState: const State.initialState
    , render: render browserFeatures -- TODO pass `BrowserFeatures` as argument
    , eval: eval browserFeatures -- TODO pass `BrowserFeatures` as argument to component
    , receiver: const Nothing
    }

dateTimePickerFormat ∷ DateTimePickerF.Format
dateTimePickerFormat = unsafePartial fromRight
  $ DateTimePickerF.fromString dateTimeStringFormat

datePickerFormat ∷ DatePickerF.Format
datePickerFormat = unsafePartial fromRight
  $ DatePickerF.fromString dateStringFormat

timePickerFormat ∷ TimePickerF.Format
timePickerFormat = unsafePartial fromRight
  $ TimePickerF.fromString timeStringFormat

dateTimeStringFormat ∷ String
dateTimeStringFormat = dateStringFormat <> timeStringFormat

dateStringFormat ∷ String
dateStringFormat = "YYYY-MMMM-DD"

timeStringFormat ∷ String
timeStringFormat = "HH:mm:ss"

formatDateTime ∷ DT.DateTime → Maybe String
formatDateTime x = hush $ FD.formatDateTime timeStringFormat x

formatDate ∷ DT.Date → Maybe String
formatDate x = hush $ FD.formatDateTime dateStringFormat $ DT.DateTime x bottom

formatTime ∷ DT.Time → Maybe String
formatTime x = hush $ FD.formatDateTime timeStringFormat $ DT.DateTime bottom x

unformatDateTime ∷ String → Maybe DT.DateTime
unformatDateTime x = hush $ FD.unformatDateTime timeStringFormat x

unformatDate ∷ String → Maybe DT.Date
unformatDate x = map DT.date $ hush $ FD.unformatDateTime dateStringFormat x

unformatTime ∷ String → Maybe DT.Time
unformatTime x = map DT.time $ hush $ FD.unformatDateTime timeStringFormat x

orEmpty ∷ Maybe String → String
orEmpty = maybe "" id

render ∷ ∀ m
  . BrowserFeatures
  → State
  → HTML m
render {inputTypeSupported} model =
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
      , HP.value (unwrap model.name)
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
      , ARIA.label $ "Type of " <> (quotedName model.name) <> " variable"
      ]
      (typeOption <$> allFieldTypes)

  typeOption
    ∷ FieldType
    → HTML m
  typeOption ty =
    HH.option
    [ HP.selected $ ty == model.fieldType ]
    [ HH.text $ Lens.review _FieldTypeDisplayName ty ]
  defaultField ∷ HTML m
  defaultField =
    case model.fieldType of
      DateTimeFieldType | not inputTypeSupported IT.DateTimeLocal →
        HH.slot'
          cpDateTimePicker
          unit
          (DateTimePicker.picker dateTimePickerFormat)
          unit
          $ HE.input
          $ \(NotifyChange n) →
            UpdateDefaultValue $ orEmpty $ value n >>= formatDateTime
      DateFieldType | not inputTypeSupported IT.Date →
        HH.slot'
          cpDatePicker
          unit
          (DatePicker.picker datePickerFormat)
          unit
          $ HE.input
          $ \(NotifyChange n) →
            UpdateDefaultValue $ orEmpty $ value n >>= formatDate
      TimeFieldType | not inputTypeSupported IT.Time →
        HH.slot'
          cpTimePicker
          unit
          (TimePicker.picker timePickerFormat)
          unit
          $ HE.input
          $ \(NotifyChange n) →
            UpdateDefaultValue $ orEmpty $ value n >>= formatTime
      BooleanFieldType →
        HH.label
          [ HP.class_ (HH.ClassName "sd-option") ]
          [ HH.input
              [ HP.type_ inputType
              , HP.checked
                  $ fromMaybe false
                  $ Lens.preview _StringBoolean
                  =<< model.defaultValue
              , HE.onChecked
                  $ HE.input (UpdateDefaultValue ∘ Lens.review _StringBoolean)
              , ARIA.label
                  $ "Default value of "
                  <> (quotedName model.name)
                  <> " variable is \"true\""
              ]
          , HH.span_ [ HH.text (unwrap model.name) ]
          ]
      _ →
          HH.input
            $ fieldType
            <> [ HP.value (maybe "" (State.sanitiseValueForForm model.fieldType) model.defaultValue)
               , HE.onValueInput (HE.input UpdateDefaultValue ∘ State.sanitiseValueFromForm model.fieldType)
               , ARIA.label lbl
               , HP.placeholder lbl
               ]

    where
    lbl ∷ String
    lbl
      = "Default value"
      <> if model.name /= (FieldName "") then " for " <> (quotedName model.name) <> " variable" else ""
    inputType =
      fieldTypeToInputType model.fieldType

    fieldType ∷ ∀ i. Array (HP.IProp DI.HTMLinput i)
    fieldType = case model.fieldType of
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

eval ∷ ∀ m. BrowserFeatures → Query ~> DSL m
eval {inputTypeSupported} = case _ of
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
    H.put m
    void case m.fieldType of
      DateTimeFieldType | not inputTypeSupported IT.DateTimeLocal →
        H.query' cpDateTimePicker unit
        $ setValue
        $ m.defaultValue >>= unformatDateTime <#> Right
      DateFieldType | not inputTypeSupported IT.Date →
        H.query' cpDatePicker unit
        $ setValue
        $ m.defaultValue >>= unformatDate <#> Right
      TimeFieldType | not inputTypeSupported IT.Time →
        H.query' cpTimePicker unit
        $ setValue
        $ m.defaultValue >>= unformatTime <#> Right
      _ -> pure (Just unit)
    pure next
  GetModel k →
    k <$> H.get
