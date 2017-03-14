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

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

import SlamData.Monad (Slam)
import SlamData.Workspace.Card.Setups.Dialog as CSD
import SlamData.Workspace.MillerColumns.BasicItem.Component as MCI
import SlamData.Workspace.MillerColumns.Column.Component as MCC
import SlamData.Workspace.MillerColumns.Component as MC
import SlamData.Workspace.MillerColumns.TreeData as MCT

data Query s a
  = UpdateSelection (Maybe s) a
  | RaiseMessage (Message s) a

data Message s
  = Dismiss
  | Confirm s

type State s =
  { selection ∷ Maybe s
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
  , isSelectable ∷ s → Boolean
  }

pickerOptionsToColumnOptions ∷ ∀ s. Ord s ⇒ PickerOptions s → MCI.BasicColumnOptions s s (Const Void)
pickerOptionsToColumnOptions { label, render, values, isSelectable } =
  MC.ColumnOptions
    { renderColumn: MCC.component
    , renderItem: MCI.component { render, label }
    , label
    , load: MCT.loadFromTree label values
    , id
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

isLeafPath ∷ ∀ a. Either a a → Boolean
isLeafPath = isRight

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
  columnState = MCT.initialStateFromTree opts.values

  render ∷ State s → HTML s
  render st =
    CSD.pickerDialog
      { onDismiss: RaiseMessage Dismiss
      , onConfirm: RaiseMessage ∘ Confirm
      , selection: st.selection
      , isSelectable: opts.isSelectable
      , classes: [ HH.ClassName "sd-dimension-picker" ]
      , title: [ HH.text opts.title ]
      , content: [ HH.slot unit (MC.component columnOptions) columnState handleMessage ]
      }

  handleMessage ∷ MC.Message' s s Void → Maybe (Query s Unit)
  handleMessage =
    either
      (\(MC.SelectionChanged _ sel) → Just $ H.action $ UpdateSelection sel)
      absurd

  eval ∷ Query s ~> DSL s
  eval = case _ of
    UpdateSelection sel next → do
      H.modify (_ { selection = sel })
      pure next
    RaiseMessage msg next → do
      H.raise msg
      pure next
