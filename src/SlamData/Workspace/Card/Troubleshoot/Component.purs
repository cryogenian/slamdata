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

import Data.StrMap as SM

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Themes.Bootstrap3 as B

import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.Component as CC
import SlamData.Workspace.Card.Model as Card
import SlamData.Workspace.Card.Port as Port
import SlamData.Workspace.Card.Troubleshoot.Component.Query (Query)
import SlamData.Workspace.Card.Troubleshoot.Component.State (State, initialState)
import SlamData.Workspace.LevelOfDetails as LOD

type DSL = CC.InnerCardDSL State Query
type HTML = CC.InnerCardHTML Query

troubleshootComponent ∷ CC.CardOptions → CC.CardComponent
troubleshootComponent =
  CC.makeCardComponent CT.Troubleshoot $ H.component
    { render: render
    , eval: evalCard ⨁ (absurd ∘ unwrap)
    , initialState: const initialState
    , receiver: const Nothing
    }

render ∷ State → HTML
render { varMap } =
  HH.table
    [ HP.classes
        [ HH.ClassName "form-builder"
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
    renderItem ∷ String → Port.VarMapValue → Array HTML
    renderItem name val =
      [ HH.tr_
          [ HH.td_ [ HH.text name ]
          , HH.td_ [ HH.code_ [ HH.text $ Port.renderVarMapValue val ] ]
          ]
      ]

evalCard ∷ CC.CardEvalQuery ~> DSL
evalCard = case _ of
  CC.Activate next →
    pure next
  CC.Deactivate next →
    pure next
  CC.Save k →
    pure $ k Card.Troubleshoot
  CC.Load _ next →
    pure next
  CC.ReceiveInput _ varMap next → do
    H.modify (_ { varMap = Port.flattenResources varMap })
    pure next
  CC.ReceiveOutput _ _ next →
    pure next
  CC.ReceiveState _ next →
    pure next
  CC.ReceiveDimensions _ reply →
    pure $ reply LOD.High
