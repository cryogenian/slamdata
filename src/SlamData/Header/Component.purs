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

import SlamData.Prelude

import Halogen as H
import Halogen.Component.ChildPath (ChildPath, cpL, cpR)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as ARIA

import SlamData.Config as Config
import SlamData.Config.Version as CV
import SlamData.GlobalMenu.Component as GlobalMenu
import SlamData.Header.Gripper.Component as Gripper
import SlamData.Monad (Slam)
import SlamData.Render.CSS as Rc

type State = Boolean
initialState ∷ State
initialState = false

type Query = Const Void

type ChildState =
  Either
    Gripper.State
    GlobalMenu.StateP

type ChildQuery =
  Coproduct
    Gripper.Query
    GlobalMenu.QueryP

type ChildSlot =
  Either
    Unit
    Unit

cpGripper
  ∷ ChildPath
      Gripper.State ChildState
      Gripper.Query ChildQuery
      Unit ChildSlot
cpGripper = cpL

cpGlobalMenu
  ∷ ChildPath
      GlobalMenu.StateP ChildState
      GlobalMenu.QueryP ChildQuery
      Unit ChildSlot
cpGlobalMenu = cpR

type StateP = H.ParentState State ChildState Query ChildQuery Slam ChildSlot
type QueryP = Coproduct Query (H.ChildF ChildSlot ChildQuery)
type DSL = H.ParentDSL State ChildState Query ChildQuery Slam ChildSlot
type HTML = H.ParentHTML ChildState Query ChildQuery Slam ChildSlot

comp ∷ H.Component StateP QueryP Slam
comp = H.parentComponent { render, eval, peek: Just peek }

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
                , HH.slot' cpGlobalMenu unit \_ →
                     { component: GlobalMenu.comp
                     , initialState: H.parentState GlobalMenu.initialState
                     }
                , HH.slot' cpGripper unit \_ →
                      { component: Gripper.comp "nav"
                      , initialState: Gripper.initialState
                      }
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

eval ∷ Query ~> DSL
eval = absurd ∘ unwrap

peek ∷ ∀ a. H.ChildF ChildSlot ChildQuery a → DSL Unit
peek (H.ChildF s q) =
  peekGripper ⨁ (const $ pure unit) $ q

  where
  peekGripper (Gripper.Notify st _) = do
    H.put
      case st of
        Gripper.Closed → false
        _ → true
  peekGripper _ =
    pure unit
