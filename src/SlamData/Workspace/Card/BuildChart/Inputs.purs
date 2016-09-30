module SlamData.Workspace.Card.BuildChart.Inputs where

import SlamData.Prelude
import Data.Array as Array

import Halogen as H
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Properties.Indexed as HP
import Halogen.HTML.Properties.Indexed.ARIA as ARIA
import Halogen.Themes.Bootstrap3 as B

import SlamData.Render.CSS as Rc
import SlamData.Form.Select (Select(..), stringVal, class OptionVal)
import SlamData.Form.Select.Component (SelectConfig)

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

type AggregationConfig a i =
  SelectConfig
    ( query ∷ SelectAction a → H.Action i
    )

primary ∷ ∀ a i. Show a ⇒ Maybe String → (SelectAction a → H.Action i) → PickerConfig a i
primary =
  { disableWhen: (_ < 2)
  , defaultWhen: (_ < 1)
  , ariaLabel: _
  , defaultOption: "Select source"
  , showValue: show
  , query: _
  }

secondary ∷ ∀ a i. Show a ⇒ Maybe String → (SelectAction a → H.Action i) → PickerConfig a i
secondary =
  { disableWhen: (_ < 1)
  , defaultWhen: const true
  , ariaLabel: _
  , defaultOption: "Select source"
  , showValue: show
  , query: _
  }

dropdown ∷ ∀ a i. Maybe String → (SelectAction a → H.Action i) → AggregationConfig a i
dropdown =
  { disableWhen: (_ < 1)
  , defaultWhen: const false
  , ariaLabel: _
  , defaultOption: ""
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
    HH.span
      [ HP.classes
          ([ HH.className "sd-picker-input" ]
            <> (HH.className "default" <$ guard isDefault))
      ]
      [ HH.button
          ([ HP.classes [ B.formControl ]
          , HP.disabled isDisabled
          , ARIA.label (fromMaybe "" conf.ariaLabel)
          ] <> (HE.onClick (HE.input_ (conf.query (Open options))) <$ guard (not isDisabled)))
          [ HH.text
              if isDefault
                then conf.defaultOption
                else maybe conf.defaultOption conf.showValue value
          ]
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

aggregationInput
  ∷ ∀ a i p
  . OptionVal a
  ⇒ AggregationConfig a i
  → Select' a
  → H.HTML p i
aggregationInput conf (isOpen × Select { options, value }) =
  let
    len = Array.length options
    isDefault = isNothing value || conf.defaultWhen len
    isDisabled = conf.disableWhen len
  in
    HH.span
      [ HP.classes
          ([ HH.className "sd-select-input" ]
            <> (HH.className "default" <$ guard isDefault))
      ]
      [ HH.button
          ([ HP.classes ([ Rc.aggregation, B.formControl, B.btn, B.btnPrimary ] <> clsValOrEmpty)
          , HP.disabled isDisabled
          ] <> (HE.onClick (HE.input_ (conf.query (if isOpen then Choose value else Open options))) <$ guard (not isDisabled)))
          []
      , HH.span
          [ HP.classes
              ([ B.listGroup
              , B.fade
              , Rc.fileListGroup
              , Rc.aggregation
              ] <> (B.in_ <$ guard isOpen))
          ]
          if isOpen
            then (map renderOption options)
            else []
      ]
  where
  renderOption val =
    HH.button
      [ HP.classes [ B.listGroupItem ]
      , HE.onClick (HE.input_ (conf.query (Choose (Just val))))
      ]
      [ HH.text (stringVal val) ]

  clsValOrEmpty =
    foldMap (pure ∘ HH.className ∘ stringVal) value
