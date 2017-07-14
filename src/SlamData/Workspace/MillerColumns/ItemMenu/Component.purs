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

module SlamData.Workspace.MillerColumns.ItemMenu.Component where

import SlamData.Prelude

import CSS as CSS
import CSS.Common as CSSC
import Data.Array as Array
import Data.List.NonEmpty as NE
import Data.Newtype (un)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as HCSS
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import SlamData.Monad (Slam)
import SlamData.Render.Icon as I
import Utils.DOM as DOM

type MenuActions msg = NE.NonEmptyList (MenuItem msg)
type MenuItem msg = { icon ∷ H.HTML Void (Const Void), label ∷ String, action ∷ msg }

data Query msg a
  = ToggleOpen Boolean a
  | StopPropagation (Query msg a) DOM.MouseEvent
  | RaiseAction msg a

type State = { open ∷ Boolean }
data Message msg = ActionMsg msg

type HTML msg = H.ComponentHTML (Query msg)
type DSL msg = H.ComponentDSL State (Query msg) (Message msg) Slam

component ∷ ∀ msg. MenuActions msg → H.Component HH.HTML (Query msg) Unit (Message msg) Slam
component config =
  H.component
    { initialState: \_ → { open: false }
    , render
    , eval
    , receiver: const Nothing
    }
    where
      render ∷ State → HTML msg
      render { open } =
        HH.div
          [ HP.class_ (HH.ClassName "sd-miller-slider")
          , HCSS.style sliderStyle
          ]
          ([ HH.button
               [ HP.class_ (HH.ClassName "sd-slider-toggle")
               , HP.title "Toggle menu"
               , HE.onClick $ Just ∘ StopPropagation (H.action (ToggleOpen (not open)))
               ]
               [ I.cog ]
           ] <> actions)
          where
            actions = Array.fromFoldable $ config <#> \ { icon, label, action} →
              HH.button
                [ HP.class_ (HH.ClassName "sd-slider-button")
                , HP.title label
                , HE.onClick $ Just ∘ StopPropagation (H.action (RaiseAction action))
                ]
                [ bimap absurd (absurd ∘ un Const) icon ]
            sliderStyle =
              if open
                then
                  CSS.width CSSC.auto
                else
                  CSS.width (CSS.rem 2.5)

      eval ∷ Query msg ~> DSL msg
      eval = case _ of
        StopPropagation q e → do
          H.liftEff (DOM.stopPropagation e)
          eval q
        RaiseAction msg next → do
          H.raise (ActionMsg msg)
          pure next
        ToggleOpen b next → do
          H.modify (_ { open = b })
          pure next
