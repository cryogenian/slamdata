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

module SlamData.AdminUI.Group.Item where

import SlamData.Prelude

import Halogen as H
import Halogen.Component.Proxy (proxy)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as ARIA
import SlamData.AdminUI.Types as AT
import SlamData.Workspace.MillerColumns.Column.Component.Item as MCI

component
  ∷ ∀ m
  . AT.GroupIndex
  → AT.GroupItem
  → H.Component HH.HTML (MCI.Query AT.GroupItem AT.GroupMessage) MCI.State (MCI.Message' AT.GroupItem AT.GroupMessage) m
component i = proxy ∘ component' i

data Query a
  = UpdateState MCI.State a
  | Selected a

type HTML = H.ComponentHTML Query
type DSL m = H.ComponentDSL Boolean Query (MCI.Message' AT.GroupItem AT.GroupMessage) m

component'
  ∷ ∀ m
  . AT.GroupIndex
  → AT.GroupItem
  → H.Component HH.HTML Query MCI.State (MCI.Message' AT.GroupItem AT.GroupMessage) m
component' path item =
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
      label = AT.groupItemName item
    in
      HH.li
        [ HP.title label
        , HE.onClick $ HE.input_ Selected
        , ARIA.label ("Select " <> label)
        , HP.classes (guard selected $> HH.ClassName "selected")
        ]
        [ renderItem item ]

  eval ∷ Query ~> DSL m
  eval = case _ of
    UpdateState state next → do
      let new = state == MCI.Selected
      old ← H.get
      when (old /= new) $ H.put new
      pure next
    Selected next → do
      H.raise $ Left $ MCI.RaisePopulate item
      pure next

renderItem ∷ AT.GroupItem → HTML
renderItem ci =
  HH.div
    [ HP.class_ (H.ClassName "sd-structure-editor-item") ]
    [ HH.div
        [ HP.class_ (H.ClassName "sd-structure-editor-item-weight")
          -- TODO(Christoph): Calculate some actual weights
        -- , HCSS.style $ CSS.width (CSS.pct (unwrap (columnItemWeight ci) * 100.0))
        ]
        []
    , HH.div_ [ HH.text (AT.groupItemName ci) ]
    ]
