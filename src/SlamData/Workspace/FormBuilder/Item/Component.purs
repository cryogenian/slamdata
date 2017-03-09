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
import Data.Lens as Lens
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Core as HC
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as ARIA
import DOM.HTML.Indexed.StepValue (StepValue(..))
import Data.Lens ((^?), (.~), (?~))
import SlamData.Workspace.FormBuilder.Item.Component.State (Model, State)
import SlamData.Workspace.FormBuilder.Item.Component.State as State
import SlamData.Workspace.FormBuilder.Item.FieldType (FieldType(..), _FieldTypeDisplayName, allFieldTypes, fieldTypeToInputType)

data Query a
  = UpdateName String a
  | UpdateFieldType String a
  | UpdateDefaultValue String a
  | SetModel Model a
  | GetModel (Model → a)
  | EnableInput a
  | DisableInput a

data Message
  = NameChanged String
  | FieldTypeChanged String
  | DefaultValueChanged String

type HTML = H.ComponentHTML Query
type DSL = H.ComponentDSL State Query Message

component ∷ ∀ g. H.Component HH.HTML Query Unit Message g
component =
  H.component
    { initialState: const State.initialState
    , render
    , eval
    , receiver: const Nothing
    }

render
  ∷ State
  → HTML
render { model, enabled } =
  HH.tr_
    [ HH.td_ [ nameField ]
    , HH.td_ [ typeField ]
    , HH.td_ [ defaultField ]
    ]

  where
  nameField ∷ HTML
  nameField =
    HH.input
      [ HP.type_ HP.InputText
      , HP.title "Field Name"
      , ARIA.label "Variable name"
      , HP.value model.name
      , HE.onValueInput (HE.input UpdateName)
      , HP.placeholder "Variable name"
      , HP.disabled $ not enabled
      ]

  quotedName ∷ String → String
  quotedName "" = ""
  quotedName s = "\"" <> s <> "\""

  typeField ∷ HTML
  typeField =
    HH.select
      [ HE.onValueChange (HE.input UpdateFieldType)
      , ARIA.label $ "Type of " <> (quotedName model.name) <> " variable"
      , HP.disabled $ not enabled
      ]
      (typeOption <$> allFieldTypes)

  typeOption
    ∷ FieldType
    → HTML
  typeOption ty =
    HH.option
    [ HP.selected $ ty == model.fieldType ]
    [ HH.text $ Lens.review _FieldTypeDisplayName ty ]

  defaultField ∷ HTML
  defaultField =
    case model.fieldType of
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
              , HP.disabled $ not enabled
              ]
          , HH.span_ [ HH.text model.name ]
          ]
      _ →
          HH.input
            $ fieldType
            <> [ HP.value (fromMaybe "" model.defaultValue)
               , HE.onValueInput (HE.input UpdateDefaultValue)
               , ARIA.label lbl
               , HP.placeholder lbl
               , HP.disabled $ not enabled
               ]

    where
    lbl ∷ String
    lbl
      = "Default value"
      <> if model.name /= "" then " for " <> (quotedName model.name) <> " variable" else ""
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

eval ∷ ∀ m. Query ~> DSL m
eval = case _ of
  UpdateName str next → do
    H.modify $ State._model ∘ State._name .~ str
    H.raise $ NameChanged str
    pure next
  UpdateFieldType str next → do
    for_ (str ^? _FieldTypeDisplayName) \ty → do
      H.modify $ State._model ∘ State._fieldType .~ ty
    H.raise $ FieldTypeChanged str
    pure next
  UpdateDefaultValue str next → do
    H.modify $ State._model ∘ State._defaultValue ?~ str
    H.raise $ DefaultValueChanged str
    pure next
  SetModel m next → do
    H.modify $ State._model .~ m
    pure next
  GetModel k →
    k ∘ Lens.view State._model <$> H.get
  EnableInput next → do
    H.modify (State._enabled .~ true)
    pure next
  DisableInput next → do
    H.modify (State._enabled .~ false)
    pure next
