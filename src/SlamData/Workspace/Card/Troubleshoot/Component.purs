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

module SlamData.Workspace.Card.Troubleshoot.Component where

import SlamData.Prelude

import Data.Lens as Lens
import Data.StrMap as SM

import Halogen as H
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP
import Halogen.Themes.Bootstrap3 as B

import SlamData.Monad (Slam)
import SlamData.Workspace.Card.Troubleshoot.Component.Query (QueryP)
import SlamData.Workspace.Card.Troubleshoot.Component.State (State, initialState)
import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.Component as CC
import SlamData.Workspace.Card.Model as Card
import SlamData.Workspace.Card.Port as Port

type TroubleshootDSL = H.ComponentDSL State QueryP Slam

troubleshootComponent ∷ CC.CardOptions → H.Component CC.CardStateP CC.CardQueryP Slam
troubleshootComponent options =
  CC.makeCardComponent
    { options
    , cardType: CT.Troubleshoot
    , component: H.component { render, eval }
    , initialState: initialState
    , _State: CC._TroubleshootState
    , _Query: CC.makeQueryPrism CC._TroubleshootQuery
    }

render ∷ State → H.ComponentHTML QueryP
render { varMap } =
  HH.table
    [ HP.classes
        [ HH.className "form-builder"
        , B.table
        , B.tableStriped
        ]
    ]
    [ HH.thead_
        [ HH.tr_
            [ HH.th_ [ HH.text "Name" ]
            , HH.th_ [ HH.text "Value" ]
            ]
        ]
    , HH.tbody_ $ SM.foldMap renderItem varMap
    ]

  where
    renderItem ∷ String → Port.VarMapValue → Array (H.ComponentHTML QueryP)
    renderItem name val =
      [ HH.tr_
          [ HH.td_ [ HH.text name ]
          , HH.td_ [ HH.code_ [ HH.text $ Port.renderVarMapValue val ] ]
          ]
      ]

eval ∷ QueryP ~> TroubleshootDSL
eval = coproduct evalCard (absurd ∘ unwrap)

evalCard ∷ CC.CardEvalQuery ~> TroubleshootDSL
evalCard = case _ of
  CC.Activate next →
    pure next
  CC.Deactivate next →
    pure next
  CC.Save k →
    pure $ k Card.Troubleshoot
  CC.Load _ next →
    pure next
  CC.ReceiveInput input next → do
    for (Lens.preview Port._VarMap input) \varMap →
      H.modify (_ { varMap = varMap })
    pure next
  CC.ReceiveOutput _ next →
    pure next
  CC.ReceiveState _ next →
    pure next
  CC.ReceiveDimensions _ next →
    pure next
  CC.ModelUpdated _ next →
    pure next
  CC.ZoomIn next →
    pure next
