module SlamData.Header.Component where

import SlamData.Prelude

import Data.Identity (Identity(..))

import Halogen as H
import Halogen.HTML.Indexed as HH
import Halogen.Component.ChildPath (ChildPath, cpL, cpR)
import Halogen.HTML.Properties.Indexed as HP
import Halogen.Themes.Bootstrap3 as B

import SlamData.Effects (Slam)
import SlamData.Header.Gripper.Component as Gripper
import SlamData.SignIn.Component as SignIn
import SlamData.Render.CSS as Rc
import SlamData.Render.Common (logo)

type State = Unit
initialState ∷ State
initialState = unit

type Query = Identity

type ChildState =
  Either
    Gripper.State
    SignIn.StateP

type ChildQuery =
  Coproduct
    Gripper.Query
    SignIn.QueryP

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

cpSignIn
  ∷ ChildPath
      SignIn.StateP ChildState
      SignIn.QueryP ChildQuery
      Unit ChildSlot
cpSignIn = cpR

type StateP = H.ParentState State ChildState Query ChildQuery Slam ChildSlot
type QueryP = Coproduct Query (H.ChildF ChildSlot ChildQuery)
type DSL = H.ParentDSL State ChildState Query ChildQuery Slam ChildSlot
type HTML = H.ParentHTML ChildState Query ChildQuery Slam ChildSlot

comp ∷ H.Component StateP QueryP Slam
comp = H.parentComponent { render, eval, peek: Nothing }

render ∷ Unit → HTML
render _ =
  HH.nav_
    [ HH.div_
        [ HH.div_
            [ HH.div [ HP.classes [ Rc.header ] ]
                [ logo $ Just "3.0"
                , HH.div
                    [ HP.classes [ B.pullRight ] ]
                    [ HH.slot' cpSignIn unit \_ →
                         { component: SignIn.comp
                         , initialState: H.parentState SignIn.initialState
                         }
                    ]
                , HH.slot' cpGripper unit \_ →
                      { component: Gripper.comp "nav"
                      , initialState: Gripper.initialState
                      }
                ]
            ]
        ]
    ]

eval ∷ Identity ~> DSL
eval (Identity next) = pure next
