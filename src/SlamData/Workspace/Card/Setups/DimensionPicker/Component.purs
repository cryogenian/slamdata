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

module SlamData.Workspace.Card.Setups.DimensionPicker.Component where

import SlamData.Prelude

import Data.List (List)
import Data.List as List

import Halogen as H
import Halogen.HTML.Events as HE
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as ARIA
import Halogen.Themes.Bootstrap3 as B

import SlamData.Monad (Slam)
import SlamData.Workspace.MillerColumns.BasicItem.Component as MCI
import SlamData.Workspace.MillerColumns.Component as MC
import SlamData.Workspace.MillerColumns.TreeData as MCT

data Query s a
  = UpdateSelection (List s) a
  | RaiseMessage (Message s) a

data Message s
  = Dismiss
  | Confirm (List s)

type State s =
  { selection ∷ Maybe (List s)
  }

initialState ∷ ∀ s. State s
initialState =
  { selection: Nothing
  }

type ChildQuery s = MC.Query s s Void
type ChildSlot = Unit

type HTML s = H.ParentHTML (Query s) (ChildQuery s) ChildSlot Slam
type DSL s = H.ParentDSL (State s) (Query s) (ChildQuery s) ChildSlot (Message s) Slam

type PickerOptions s =
  { label  ∷ s → String
  , render ∷ s → MCI.BasicItemHTML
  , title  ∷ String
  , values ∷ MCT.Tree s
  , isSelectable ∷ List s → Boolean
  }

pickerOptionsToColumnOptions ∷ ∀ s. Eq s ⇒ PickerOptions s → MCI.BasicColumnOptions s s
pickerOptionsToColumnOptions { label, render, values, isSelectable } =
  { render: MCI.component { render, label }
  , label: label
  , load: MCT.loadFromTree id label values
  , id: id
  , isLeaf: isSelectable
  }

labelNode ∷ ∀ s. (s → String) → Either s s → String
labelNode f = either f f

renderNode ∷ ∀ s. (s → String) → Either s s → MCI.BasicItemHTML
renderNode f node =
  HH.div
    [ HP.classes
        [ HH.ClassName "sd-miller-column-item-inner"
        , either
            (const $ HH.ClassName "sd-miller-column-item-node")
            (const $ HH.ClassName "sd-miller-column-item-leaf")
            node
        ]
    ]
    [ HH.span_ [ HH.text (either f f node) ] ]

isLeafPath ∷ ∀ a. List (Either a a) → Boolean
isLeafPath = maybe true isRight ∘ List.head

picker
  ∷ ∀ s
  . Ord s
  ⇒ PickerOptions s
  → H.Component HH.HTML (Query s) Unit (Message s) Slam
picker opts =
  H.parentComponent
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  columnOptions = pickerOptionsToColumnOptions opts
  columnState = MCT.initialStateFromTree columnOptions.id opts.values

  render ∷ State s → HTML s
  render st =
    HH.div
      [ HP.classes [ HH.ClassName "sd-dimension-picker" ] ]
      [ HH.div
          [ HP.classes [ HH.ClassName "sd-dimension-picker-title" ] ]
          [ HH.h1_ [ HH.text opts.title ]
          , HH.button
              [ HP.classes [ HH.ClassName "sd-dismiss-button" ]
              , HP.title "Dismiss"
              , ARIA.label "Dismiss"
              , HE.onClick $ HE.input_ $ RaiseMessage Dismiss
              ]
              [ HH.text "×"]
          ]
      , HH.div
          [ HP.classes [ HH.ClassName "sd-dimension-picker-content" ] ]
          [ HH.slot unit (MC.component columnOptions) columnState handleMessage ]
      , HH.div
          [ HP.classes [ HH.ClassName "sd-dimension-picker-toolbar" ] ]
          [ HH.button
              [ HP.classes [ B.btn, B.btnDefault ]
              , ARIA.label "Dismiss"
              , HE.onClick $ HE.input_ $ RaiseMessage Dismiss
              ]
              [ HH.text "Dismiss" ]
          , HH.button
              ([ HP.classes [ B.btn, B.btnPrimary ]
              , ARIA.label ""
              ] <>
                case st.selection of
                  Just sel | opts.isSelectable sel →
                    [ HE.onClick $ HE.input_ $ RaiseMessage $ Confirm (List.reverse sel)
                    , HP.disabled false
                    ]
                  _ →
                    [ HP.disabled true ])
              [ HH.text "Confirm" ]
          ]
      ]

  handleMessage ∷ MC.Message' s s Void → Maybe (Query s Unit)
  handleMessage =
    either
      (\(MC.SelectionChanged sel _) → Just $ H.action $ UpdateSelection sel)
      absurd

  eval ∷ Query s ~> DSL s
  eval = case _ of
    UpdateSelection sel next → do
      H.modify (_ { selection = Just sel })
      pure next
    RaiseMessage msg next → do
      H.raise msg
      pure next
