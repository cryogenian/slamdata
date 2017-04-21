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

module SlamData.Workspace.Card.StructureEditor.Column.Component where

import SlamData.Prelude

import Halogen as H
import Halogen.Component.Proxy (proxyQL)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Query.HalogenM as HQ

import SlamData.Monad (Slam)
import SlamData.Workspace.Card.StructureEditor.Common (ColumnItem, ColumnPath)
import SlamData.Workspace.Card.StructureEditor.Message (Message)
import SlamData.Workspace.MillerColumns.Column.Component as MCC

type ColumnOptions = MCC.ColumnOptions ColumnItem ColumnPath Message
type ColumnQuery = MCC.Query ColumnItem ColumnPath Message

type Message' = MCC.Message' ColumnItem ColumnPath Message

component
  ∷ ColumnOptions
  → ColumnPath
  → H.Component HH.HTML (MCC.Query' ColumnItem ColumnPath Message) (Maybe ColumnItem) Message' Slam
component opts = proxyQL ∘ component' opts

data Query a = Raise Message' a

type Query' = Coproduct ColumnQuery Query

type State = Maybe ColumnItem

type DSL = H.ParentDSL State Query' ColumnQuery Unit Message' Slam
type HTML = H.ParentHTML Query' ColumnQuery Unit Slam

component'
  ∷ ColumnOptions
  → ColumnPath
  → H.Component HH.HTML Query' (Maybe ColumnItem) Message' Slam
component' opts path =
  H.parentComponent
    { initialState: id
    , render
    , eval
    , receiver: Just ∘ left ∘ H.action ∘ MCC.SetSelection
    }
  where

  column ∷ H.Component HH.HTML ColumnQuery (Maybe ColumnItem) Message' Slam
  column = MCC.component' opts path

  render ∷ State → HTML
  render st =
    HH.div_
      [ HH.div
          [ HP.class_ (HH.ClassName "sd-structure-editor-column") ]
          [ HH.slot unit column st (Just ∘ right ∘ H.action ∘ Raise) ]
      ]

  eval ∷ Query' ~> DSL
  eval = coproduct evalInner evalOuter

  evalInner ∷ ColumnQuery ~> DSL
  evalInner =
    maybe (HQ.halt "Inner component query failed") pure <=< H.query unit

  evalOuter ∷ Query ~> DSL
  evalOuter = case _ of
    Raise msg next → do
      H.raise msg
      pure next
