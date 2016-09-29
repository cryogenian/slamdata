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
import Halogen.HTML.Indexed as HH
import Halogen.Component.ChildPath (ChildPath, cpL, cpR)
import Halogen.HTML.Properties.Indexed as HP

import SlamData.Monad (Slam)
import SlamData.Header.Gripper.Component as Gripper
import SlamData.Render.Common (logo)
import SlamData.Render.CSS as Rc
import SlamData.GlobalMenu.Component as GlobalMenu

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
        [ HH.className "sd-nav"
        , HH.className if open then "open" else "closed" ]
        ]
    [ HH.div_
        [ HH.div_
            [ HH.div [ HP.classes [ Rc.header ] ]
                [ logo $ Just "3.0"
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

eval ∷ Query ~> DSL
eval = absurd ∘ getConst

peek ∷ ∀ a. H.ChildF ChildSlot ChildQuery a → DSL Unit
peek (H.ChildF s q) =
  peekGripper ⨁ (const $ pure unit) $ q

  where
  peekGripper (Gripper.Notify st _) = do
    H.set
      case st of
        Gripper.Closed → false
        _ → true
  peekGripper _ =
    pure unit
