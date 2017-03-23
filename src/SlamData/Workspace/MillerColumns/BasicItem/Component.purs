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

module SlamData.Workspace.MillerColumns.BasicItem.Component where

import SlamData.Prelude

import Halogen as H
import Halogen.HTML.Events as HE
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as ARIA

import SlamData.Workspace.MillerColumns.Component as MC
import SlamData.Workspace.MillerColumns.Column.Component.Item as MCI
import SlamData.Monad (Slam)

data Query a
  = UpdateState MCI.ItemState a
  | Selected a

type HTML = H.ComponentHTML Query
type DSL a = H.ComponentDSL Boolean Query (MCI.ItemMessage' a Void) Slam

type ItemSpec a =
  { label ∷ a → String
  , render ∷ a → BasicItemHTML
  }

type BasicItemHTML = H.ComponentHTML (Const Void)

type BasicColumnOptions a i = MC.ColumnOptions a i Query Void

component
  ∷ ∀ a i
  . Eq i
  ⇒ ItemSpec a
  → i
  → a
  → H.Component HH.HTML Query MCI.ItemState (MCI.ItemMessage' a Void) Slam
component ispec path item =
  H.component
    { initialState: (_ == MCI.Selected)
    , render
    , eval
    , receiver: HE.input UpdateState
    }
  where
  render ∷ Boolean → HTML
  render selected =
    let
      label = ispec.label item
    in
      HH.li
        [ HP.title label
        , HE.onClick $ HE.input_ Selected
        , ARIA.label ("Select " <> label)
        , HP.classes $ (guard selected $> HH.ClassName "selected")
        ]
        [ absurd ∘ unwrap <$> ispec.render item ]

  eval ∷ Query ~> DSL a
  eval = case _ of
    UpdateState state next → do
      let new = state == MCI.Selected
      old ← H.get
      when (old /= new) $ H.put new
      pure next
    Selected next → do
      H.raise $ Left $ MCI.RaisePopulate item
      pure next
