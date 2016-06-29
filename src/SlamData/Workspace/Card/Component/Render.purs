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

module SlamData.Workspace.Card.Component.Render where

import SlamData.Prelude

import Halogen (ParentHTML)
import Halogen.HTML.Indexed as H
import Halogen.HTML.Properties.Indexed as P
import Halogen.HTML.Properties.Indexed.ARIA as ARIA

import SlamData.Workspace.Card.Component.Query (CardQuery, InnerCardQuery)
import SlamData.Workspace.Card.Component.State (CardState, AnyCardState)
import SlamData.Effects (Slam)
import SlamData.Render.CSS as CSS
import SlamData.Workspace.Card.CardType (CardType(PendingCard), cardName)

type CardHTML = ParentHTML AnyCardState CardQuery InnerCardQuery Slam Unit

renderHeader
  ∷ CardType
  → CardState
  → Array CardHTML
renderHeader cty cs =
  guard (cty ≠ PendingCard) $>
    H.div
      [ P.classes [CSS.cardHeader]
      , ARIA.label $ (cardName cty) ⊕ " card"
      ]
      [ H.div
          [ P.class_ CSS.cardName ]
          [ H.text $ cardName cty ]
      ]
