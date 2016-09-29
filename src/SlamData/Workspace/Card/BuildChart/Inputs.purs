module SlamData.Workspace.Card.BuildChart.Inputs where

import SlamData.Prelude
import Data.Array as Array

import Halogen as H
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Properties.Indexed as HP
import Halogen.Themes.Bootstrap3 as B

import SlamData.Render.CSS as Rc
import SlamData.Form.Select (Select(..), stringVal, class OptionVal)
import SlamData.Form.Select.Component (SelectConfig)

data SelectAction a
  = Open (Array a)
  | Choose (Maybe a)

type PickerConfig a i =
  SelectConfig
    ( showValue ∷ a → String
    , query ∷ SelectAction a → H.Action i
    )

type AggregationInput a i =
  SelectConfig
    ( query ∷ SelectAction a → H.Action i
    , open ∷ Boolean
    )

pickerInput
  ∷ ∀ a i p
  . PickerConfig a i
  → Select a
  → H.HTML p i
pickerInput conf (Select { options, value }) =
  let
    len = Array.length options
    isDefault = isNothing value || conf.defaultWhen len
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
          ] <> (HE.onClick (HE.input_ (conf.query (Open options))) <$ guard (not isDisabled)))
          [ HH.text
              if isDefault
                then conf.defaultOption
                else maybe conf.defaultOption conf.showValue value
          ]
      ]

aggregationInput
  ∷ ∀ a i p
  . OptionVal a
  ⇒ AggregationInput a i
  → Select a
  → H.HTML p i
aggregationInput conf (Select { options, value }) =
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
          ] <> (HE.onClick (HE.input_ (conf.query (if conf.open then Choose value else Open options))) <$ guard (not isDisabled)))
          []
      , HH.span
          [ HP.classes
              ([ B.listGroup
              , B.fade
              , Rc.fileListGroup
              , Rc.aggregation
              ] <> (B.in_ <$ guard conf.open))
          ]
          if conf.open
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
