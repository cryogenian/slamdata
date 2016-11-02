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

module SlamData.Workspace.Card.Pending.Component where

import SlamData.Prelude

import SlamData.Monad (Slam)
import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.Component as CC
import SlamData.Workspace.Card.Model as Card
import SlamData.Workspace.Card.Pending.Component.Query as PCQ
import SlamData.Workspace.Card.Pending.Component.State as PCS

import Halogen as H
import Halogen.HTML.Indexed as HH

type DSL = H.ComponentDSL PCS.State PCQ.QueryP Slam
type HTML = H.ComponentHTML PCQ.QueryP

comp ∷ CC.CardOptions → CC.CardComponent
comp options =
  CC.makeCardComponent
    { options
    , cardType: CT.PendingCard
    , component: H.component { render, eval }
    , initialState: PCS.initialState
    , _State: CC._PendingState
    , _Query: CC.makeQueryPrism CC._PendingQuery
    }

render ∷ PCS.State → HTML
render st =
  HH.div_
    [ HH.i_ []
    , HH.span_ [ HH.text st.message ]
    ]

eval ∷ PCQ.QueryP ~> DSL
eval = coproduct cardEval PCQ.initiality

cardEval ∷ CC.CardEvalQuery ~> DSL
cardEval = case _ of
  CC.Activate next →
    pure next
  CC.Deactivate next →
    pure next
  CC.Save k →
    pure $ k Card.PendingCard
  CC.Load _ next →
    pure next
  CC.ReceiveInput _ next → do
    pure next
  CC.ReceiveOutput _ next → do
    pure next
  CC.ReceiveState _ next → do
    pure next
  CC.ReceiveDimensions _ next →
    pure next
  CC.ModelUpdated _ next →
    pure next
  CC.ZoomIn next →
    pure next
