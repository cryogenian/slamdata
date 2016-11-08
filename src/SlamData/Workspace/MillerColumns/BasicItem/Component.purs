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

module SlamData.Workspace.MillerColumns.BasicItem.Component
  ( BasicColumnOptions
  , BasicColumnsQuery
  , BasicColumnsState
  , Query
  , State
  , ItemSpec
  , component
  ) where

import SlamData.Prelude

import Data.List as L

import Halogen as H
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP
import Halogen.HTML.Properties.Indexed.ARIA as ARIA

import SlamData.Workspace.MillerColumns.Component as MC
import SlamData.Monad (Slam)

type Query i = MC.ChildQuery i (Const Void)

type State =
  { selected :: Boolean
  }

type HTML i = H.ComponentHTML (Query i)
type DSL i m p = H.ComponentDSL State (Query i) m p

type ItemSpec a =
  { label ∷ a → String
  , render ∷ a → H.ComponentHTML (Const Void)
  }

type BasicColumnOptions a i = MC.ColumnOptions a i State (Const Void)
type BasicColumnsQuery i = MC.Query' i (Const Void)
type BasicColumnsState a i = MC.State' a i State (Const Void)

component
  ∷ ∀ a i
  . Eq i
  ⇒ ItemSpec a
  → L.List i
  → a
  → MC.InitialItemState
  → { component :: H.Component State (Query i) Slam
    , initialState :: State
    }
component ispec path item itemState =
  { component: H.component { render, eval }
  , initialState: { selected: itemState == MC.Selected }
  }
  where
  render ∷ State → HTML i
  render state =
    let
      label = ispec.label item
    in
      HH.li
        [ HE.onClick $ HE.input_ $ left <<< MC.RaisePopulate path
        , HP.title label
        , ARIA.label ("Select " <> label)
        , HP.classes $ (guard state.selected $> HH.className "selected")
        ]
        [ absurd ∘ getConst <$> ispec.render item ]

  eval ∷ Query i ~> DSL i Slam
  eval = coproduct evalItemQuery (absurd <<< getConst)

  evalItemQuery ∷ MC.ItemQuery i ~> DSL i Slam
  evalItemQuery = case _ of
    MC.RaisePopulate path next →
      pure next
    MC.ToggleSelected b next → do
      H.modify (_ { selected = b })
      pure next
