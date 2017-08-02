{-
Copyright 2017 SlamData, Inc.

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

module SlamData.Workspace.Dialog.Theme.Component where

import SlamData.Prelude

import Data.Lens.Fold (preview)
import Data.Lens.Prism (review)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import SlamData.Monad (Slam)
import SlamData.Render.ClassName as CN
import SlamData.Theme.Option as Option
import SlamData.Theme.Theme as Theme
import Utils.DOM as DOM

type State =
  { custom ∷ String
  , option ∷ Option.Option
  , submitting ∷ Boolean
  }

--_Option = SProxy ∷ SProxy "option"

--_Custom = SProxy ∷ SProxy "custom"

initialState ∷ Maybe Theme.Theme → State
initialState mt =
  { custom: maybe mempty Theme.getCustomValue mt
  , option: maybe Option.Default Option.fromTheme mt
  , submitting: false
  }

toTheme ∷ State → Maybe Theme.Theme
toTheme s = case s.option of
  Option.Light →
    Just Theme.Light
  Option.Dark →
    Just Theme.Dark
  -- TODO: Custom
  _ →
    Nothing

data Query a
  = PreventDefaultAndSave DOM.Event a
  | UpdateOption Option.Option a
  | UpdateCustom String a
  | Cancel a

data Message
  = Dismiss
  | Theme Theme.Theme

component ∷ H.Component HH.HTML Query (Maybe Theme.Theme) Message Slam
component =
  H.component
    { initialState
    , render
    , eval
    , receiver: const Nothing
    }

render ∷ State → H.ComponentHTML Query
render { custom, option, submitting } =
  let
    option' ∷ ∀ p i. Option.Option → H.HTML p i
    option' o =
      HH.option
        [ HP.value $ review Option.valuePrism' o
        , HP.selected $ option == o
        ]
        [ HH.text $ Option.toLabel o ]

    nameThemeOption ∷ String
    nameThemeOption = "themeOption"

    nameThemeCustomURL ∷ String
    nameThemeCustomURL = "themeCustomURL"
  in
    HH.form
      [ HP.class_ $ HH.ClassName "deck-dialog-theme"
      --, HE.onSubmit
      ]
      [ HH.h4_ [ HH.text "Theme deck" ]
      , HH.fieldset
          [ HP.class_ $ HH.ClassName "deck-dialog-body"
          , HP.disabled $ submitting
          ]
          [ HH.label
              [ HP.for nameThemeOption ]
              [ HH.text "Theme" ]
          , HH.select
              [ HP.class_ CN.formControl
              , HP.name nameThemeOption
              , HP.required true
              , HE.onValueChange
                  $ HE.input
                  $ UpdateOption ∘ fromMaybe Option.Default ∘ preview Option.valuePrism'
              ]
              $ option' <$> Option.options
          , HH.label
              [ HP.for nameThemeCustomURL ]
              [ HH.text "Custom Theme URL" ]
          , HH.input
              [ HP.class_ CN.formControl
              , HP.type_ HP.InputUrl
              , HP.name nameThemeCustomURL
              , HP.placeholder "Custom CSS URL"
              , HP.disabled $ option /= Option.Custom
              , HE.onValueInput $ HE.input UpdateCustom
              ]
          ]
    , HH.div
        [ HP.classes [ HH.ClassName "deck-dialog-footer" ] ]
        [ HH.button
            [ HP.classes [ CN.btn, CN.btnDefault ]
            , HE.onClick $ HE.input_ Cancel
            , HP.type_ HP.ButtonButton
            ]
            [ HH.text "Dismiss" ]
        , HH.button
            [ HP.classes [ CN.btn, CN.btnPrimary ]
            , HP.type_ HP.ButtonSubmit
            ]
            [ HH.text "Save" ]
        ]
      ]

eval ∷ Query ~> H.ComponentDSL State Query Message Slam
eval = case _ of
  PreventDefaultAndSave ev next → do
    H.liftEff (DOM.preventDefault ev)
    maybeTheme ← H.gets toTheme
    for_ maybeTheme (H.raise ∘ Theme)
    pure next
  UpdateOption option next →
    H.modify _ { option = option } $> next
  UpdateCustom custom next →
    H.modify _ { custom = custom } $> next
  Cancel next →
    H.raise Dismiss $> next
