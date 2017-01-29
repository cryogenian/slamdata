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

module SlamData.Workspace.Card.Setups.Inputs where

import SlamData.Prelude
import Data.Array as Array

import Halogen as H
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Properties.Indexed as HP
import Halogen.HTML.Properties.Indexed.ARIA as ARIA
import Halogen.Themes.Bootstrap3 as B

import SlamData.Form.Select (Select(..), stringVal, class OptionVal)

type Select' a = Boolean × Select a

data SelectAction a
  = Open (Array a)
  | Choose (Maybe a)

type PickerConfig a i =
  SelectConfig
    ( showValue ∷ a → String
    , query ∷ SelectAction a → H.Action i
    )

type PickerOptions a f =
  { options ∷ Array a
  , select ∷ f (Const Unit)
  }

type NewPickerOptions option action =
  { options ∷ Array option
  , action ∷ action
  }

type AggregationConfig a i =
  SelectConfig
    ( query ∷ SelectAction a → H.Action i
    )

type SelectConfig r =
  { disableWhen ∷ Int → Boolean
  , defaultWhen ∷ Int → Boolean
  , ariaLabel ∷ Maybe String
  , defaultOption ∷ String
  | r
  }

primary
  ∷ ∀ a i
  . Show a
  ⇒ Maybe String
  → (SelectAction a → H.Action i)
  → PickerConfig a i
primary lbl =
  { disableWhen: (_ < 2)
  , defaultWhen: (_ < 1)
  , ariaLabel: lbl
  , defaultOption: "Choose " ⊕ fromMaybe "source" lbl
  , showValue: show
  , query: _
  }

secondary
  ∷ ∀ a i
  . Show a
  ⇒ Maybe String
  → (SelectAction a → H.Action i)
  → PickerConfig a i
secondary lbl =
  { disableWhen: (_ < 1)
  , defaultWhen: const true
  , ariaLabel: lbl
  , defaultOption: "Choose " ⊕ fromMaybe "source" lbl
  , showValue: show
  , query: _
  }

dropdown
  ∷ ∀ a i
  . OptionVal a
  ⇒ Maybe String
  → (SelectAction a → H.Action i)
  → PickerConfig a i
dropdown =
  { disableWhen: (_ < 1)
  , defaultWhen: const false
  , ariaLabel: _
  , defaultOption: ""
  , showValue: stringVal
  , query: _
  }

aggregation ∷ ∀ a i. OptionVal a ⇒ Maybe String → (SelectAction a → H.Action i) → PickerConfig a i
aggregation =
  { disableWhen: (_ < 1)
  , defaultWhen: const false
  , ariaLabel: _
  , defaultOption: ""
  , showValue: stringVal
  , query: _
  }

pickerInput
  ∷ ∀ a i p
  . PickerConfig a i
  → Select a
  → H.HTML p i
pickerInput conf (Select { options, value }) =
  let
    len = Array.length options
    isDefault = isNothing value
    isDisabled = conf.disableWhen len
  in
   HH.div [ HP.classes [ HH.className "sd-picker-input" ] ]
     [ HH.button
         ([ HP.classes
            $ [ B.formControl, HH.className "sd-picker-main-button" ]
            ⊕ ( HH.className "default" <$ guard isDefault )
          , HP.disabled isDisabled
          , ARIA.label (fromMaybe "" conf.ariaLabel)
          ]
          <> (HE.onClick (HE.input_ (conf.query (Open options))) <$ guard (not isDisabled)))
         ( foldMap (pure ∘ HH.p_ ∘ pure ∘ HH.text ∘ flip append ":") (value *> conf.ariaLabel)
           ⊕ [ HH.text (maybe conf.defaultOption conf.showValue (guard (not isDefault) *> value)) ]
         )
     , if conf.defaultWhen len && not isDefault
          then
            HH.button
              [ HP.classes [ HH.className "sd-dismiss-button" ]
              , HE.onClick (HE.input_ (conf.query (Choose Nothing)))
              , HP.disabled isDisabled
              ]
              [ HH.text "×" ]
          else
            HH.text ""
      ]

selectInput
  ∷ ∀ a i p
  . Eq a
  ⇒ PickerConfig a i
  → Select a
  → H.HTML p i
selectInput conf (Select { options, value }) =
  let
    len = Array.length options
    defaultWhen = conf.defaultWhen len
    isDefault = isNothing value
    isDisabled = conf.disableWhen len
    renderedOptions = map renderOption options
  in
    HH.select
      [ HP.classes [ B.formControl ]
      , HP.disabled isDisabled
      , HE.onSelectedIndexChange \ix →
          case defaultWhen, ix of
            true, 0 → pure (Just (conf.query (Choose Nothing) unit))
            true, _ → pure (Array.index options (ix - 1) <#> \val → conf.query (Choose (Just val)) unit)
            _   , _ → pure (Array.index options ix <#> \val → conf.query (Choose (Just val)) unit)
      ]
      if defaultWhen
        then
          [ HH.option
              [ HP.selected isDefault ]
              [ HH.text conf.defaultOption ]
          ] ⊕ renderedOptions
        else
          renderedOptions

  where
  renderOption option =
    HH.option
      [ HP.selected (Just option ≡ value) ]
      [ HH.text (conf.showValue option) ]

pickerWithSelect
  ∷ ∀ a b i p
  . Eq b
  ⇒ PickerConfig a i
  → Select a
  → PickerConfig b i
  → Select b
  → H.HTML p i
pickerWithSelect conf1 sel1@(Select { options, value }) conf2 sel2 =
  let
    isDisabled = conf1.disableWhen (Array.length options)
  in
    HH.div [ HP.classes [ HH.className "sd-picker-with-select" ] ]
      [ pickerInput conf1 sel1
      , if isDisabled
          then selectInput (conf2 { disableWhen = const true }) sel2
          else selectInput conf2 sel2
      ]
