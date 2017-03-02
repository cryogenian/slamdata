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

module SlamData.Header.Component where

import Data.Coyoneda (Coyoneda)
import Data.Coyoneda as Coyoneda

import SlamData.Prelude
import Halogen as H
import Halogen.Query.HalogenM as HQ
import Halogen.Component.ChildPath as CP
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as ARIA
import SlamData.Config as Config
import SlamData.Config.Version as CV
import SlamData.GlobalMenu.Component as GlobalMenu
import SlamData.Header.Gripper.Component as Gripper
import SlamData.Render.CSS as Rc
import SlamData.Monad (Slam)

type State
  = Boolean

data Query a
  = HandleGripper Gripper.Message a
  | QueryGripper (Coyoneda Gripper.Query a)
  | QueryGlobalMenu (Coyoneda GlobalMenu.Query a)
  | Dismiss a

type ChildQuery = Gripper.Query ⨁ GlobalMenu.Query ⨁ Const Void

type ChildSlot = Unit ⊹ Unit ⊹ Void

type HTML = H.ParentHTML Query ChildQuery ChildSlot Slam

component ∷ H.Component HH.HTML Query Unit Void Slam
component = H.parentComponent
  { initialState: const false
  , render
  , eval
  , receiver: const Nothing
  }

render ∷ State → HTML
render open =
  HH.nav
    [ HP.classes
        [ HH.ClassName "sd-nav"
        , HH.ClassName if open then "open" else "closed" ]
        ]
    [ HH.div_
        [ HH.div_
            [ HH.div [ HP.classes [ Rc.header ] ]
                [ logo CV.shortVersion
                , HH.slot' CP.cp2 unit GlobalMenu.component unit absurd
                , HH.slot' CP.cp1 unit (Gripper.component "nav") unit $ HE.input HandleGripper
                ]
            ]
        ]
    ]

logo ∷ Maybe String → HTML
logo mbVersion =
  HH.div
    [ HP.class_ Rc.navLogo ]
    $ [ HH.a
          [ HP.href Config.slamDataHome
          , ARIA.label "Browse root folder"
          , HP.title "Browse root folder"
          ]
          [ HH.img [ HP.src "img/logo.svg" ] ]
      ]
    ⊕ foldMap (pure ∘ HH.div_ ∘ pure ∘ HH.text) mbVersion

eval ∷ Query ~> H.ParentDSL State Query ChildQuery ChildSlot Void Slam
eval = case _ of
  HandleGripper (Gripper.Notify st) next → do
    H.put case st of
      Gripper.Closed → false
      _ → true
    pure next
  QueryGripper iq → do
    iq # Coyoneda.unCoyoneda \k q -> do
      result <- H.query' CP.cp1 unit q
      case result of
        Nothing -> HQ.halt "Inner component query failed (this should be impossible)"
        Just a -> pure (k a)
  QueryGlobalMenu iq → do
    iq # Coyoneda.unCoyoneda \k q -> do
      result <- H.query' CP.cp2 unit q
      case result of
        Nothing -> HQ.halt "Inner component query failed (this should be impossible)"
        Just a -> pure (k a)
  Dismiss next → do
    H.query' CP.cp2 unit $ H.action GlobalMenu.DismissSubmenu
    pure next
