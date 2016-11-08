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
  , UpdateQuery(..)
  , module SlamData.Workspace.FormBuilder.Item.Component.State
  , itemComponent
  ) where

import SlamData.Prelude

import Data.Lens ((^?), (.~), (?~))
import Data.Lens as Lens

import Halogen as H
import Halogen.HTML.Core as HC
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP
import Halogen.HTML.Properties.Indexed.ARIA as ARIA

import SlamData.Workspace.FormBuilder.Item.Component.State (Model, State, EqModel(..), _defaultValue, _fieldType, _model, _name, decode, defaultValueToVarMapValue, emptyValueOfFieldType, encode, initialModel, initialState, runEqModel)
import SlamData.Workspace.FormBuilder.Item.FieldType (FieldType(..), _FieldTypeDisplayName, allFieldTypes, fieldTypeToInputType)

import Unsafe.Coerce (unsafeCoerce)

data UpdateQuery a
  = UpdateName String a
  | UpdateFieldType String a
  | UpdateDefaultValue String a

data Query a
  = Update (UpdateQuery a)
  | SetModel Model a
  | GetModel (Model -> a)

type ItemHTML = H.ComponentHTML Query
type ItemDSL g = H.ComponentDSL State Query g

itemComponent :: forall g. H.Component State Query g
itemComponent = H.component { render, eval }

render
  :: State
  -> ItemHTML
render { model } =
  HH.tr_ $
    [ HH.td_ [ nameField ]
    , HH.td_ [ typeField ]
    , HH.td_ [ defaultField ]
    ]

  where
  nameField :: ItemHTML
  nameField =
    HH.input
      [ HP.inputType HP.InputText
      , HP.title "Field Name"
      , ARIA.label "Variable name"
      , HP.value model.name
      , HE.onValueInput (HE.input \str -> Update <<< UpdateName str)
      , HP.placeholder "Variable name"
      ]

  quotedName :: String -> String
  quotedName "" = ""
  quotedName s = "\"" <> s <> "\""

  typeField :: ItemHTML
  typeField =
    HH.select
      [ HE.onValueChange (HE.input \str -> Update <<< UpdateFieldType str)
      , ARIA.label $ "Type of " <> (quotedName model.name) <> " variable"
      ]
      (typeOption <$> allFieldTypes)

  typeOption
    :: FieldType
    -> ItemHTML
  typeOption ty =
    HH.option
    [ HP.selected $ ty == model.fieldType ]
    [ HH.text $ Lens.review _FieldTypeDisplayName ty ]

  defaultField :: ItemHTML
  defaultField =
    case model.fieldType of
      BooleanFieldType ->
        HH.label
          [ HP.class_ (HH.className "sd-option") ]
          [ HH.input
              [ HP.inputType inputType
              , HP.checked
                  $ fromMaybe false
                  $ Lens.preview _StringBoolean
                  =<< model.defaultValue
              , HE.onChecked
                $ HE.input \str ->
                    Update <<< UpdateDefaultValue (Lens.review _StringBoolean str)
              , ARIA.label
                  $ "Default value of "
                  <> (quotedName model.name)
                  <> " variable is \"true\""
              ]
          , HH.span_ [ HH.text model.name ]
          ]
      _ ->
          HH.input
            $ fieldType
            <> [ HP.value (fromMaybe "" model.defaultValue)
               , HE.onValueInput
                   $ HE.input \str -> Update <<< UpdateDefaultValue str
               , ARIA.label lbl
               , HP.placeholder lbl
               ]

    where
    lbl :: String
    lbl
      = "Default value"
      <> if model.name /= "" then " for " <> (quotedName model.name) <> " variable" else ""
    inputType =
      fieldTypeToInputType model.fieldType

    fieldType ∷ ∀ r i. Array (HP.IProp (inputType ∷ HP.I|r) i)
    fieldType = case model.fieldType of
      DateTimeFieldType ->
        [ HP.inputType inputType
        , secondsStep
        ]
      TimeFieldType ->
        [ HP.inputType inputType
        , secondsStep
        ]
      _ ->
        [ HP.inputType inputType ]

    secondsStep ∷ ∀ i r. HP.IProp r i
    secondsStep = refine $ HC.Attr Nothing (HC.attrName "step") "1"
      where
      refine :: HC.Prop i -> HP.IProp r i
      refine = unsafeCoerce

    _StringBoolean :: Lens.Prism' String Boolean
    _StringBoolean = Lens.prism re pre
      where
        re b = if b then "true" else "false"
        pre "true" = Right true
        pre "false" = Right false
        pre str = Left str

eval :: forall g. Query ~> ItemDSL g
eval q =
  case q of
    Update q ->
      evalUpdate q
    SetModel m next -> do
      H.modify $ _model .~ m
      pure next
    GetModel k ->
      k <<< Lens.view _model <$> H.get

evalUpdate :: forall g. UpdateQuery ~> ItemDSL g
evalUpdate q =
  case q of
    UpdateName str next -> do
      H.modify $ _model <<< _name .~ str
      pure next
    UpdateFieldType str next -> do
      for_ (str ^? _FieldTypeDisplayName) \ty -> do
        H.modify $ _model <<< _fieldType .~ ty
      pure next
    UpdateDefaultValue str next -> do
      H.modify $ _model <<< _defaultValue ?~ str
      pure next
