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

module SlamData.Workspace.Card.Next.Component
 ( nextCardComponent
 , module SlamData.Workspace.Card.Next.Component.State
 , module SlamData.Workspace.Card.Next.Component.Query
 ) where

import SlamData.Prelude

import Data.Array as Arr
import Data.Lens ((.~), (?~))

import Halogen as H
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP
import Halogen.HTML.Properties.Indexed.ARIA as ARIA
import Halogen.Themes.Bootstrap3 as B

import SlamData.Effects (Slam)
import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.Component as CC
import SlamData.Workspace.Card.Model as Card
import SlamData.Workspace.Card.Next.Component.Query (QueryP, Query(..), _AddCardType)
import SlamData.Workspace.Card.Next.Component.State (State, _message, _types, initialState)
import SlamData.Workspace.Card.Port as Port

type NextHTML = H.ComponentHTML QueryP
type NextDSL = H.ComponentDSL State QueryP Slam

nextCardComponent :: CC.CardComponent
nextCardComponent = CC.makeCardComponent
  { cardType: CT.NextAction
  , component: H.component {render, eval}
  , initialState: initialState
  , _State: CC._NextState
  , _Query: CC.makeQueryPrism CC._NextQuery
  }

render :: State → NextHTML
render state =
  case state.message of
    Nothing →
      HH.ul_
        $ map nextButton state.types
        ⊕ map disabledButton (CT.insertableCardTypes Arr.\\ state.types)

    Just msg →
      HH.div
        [ HP.classes [ B.alert, B.alertInfo ] ]
        [ HH.h4_ [ HH.text msg ] ]
  where
  cardTitle ∷ CT.CardType → String
  cardTitle cty = "Insert " ⊕ CT.cardName cty ⊕ " card"

  disabledTitle ∷ CT.CardType → String
  disabledTitle cty = CT.cardName cty ⊕ " is unavailable as next action"

  nextButton ∷ CT.CardType → NextHTML
  nextButton cty =
    HH.li_
      [ HH.button
          [ HP.title $ cardTitle cty
          , ARIA.label $ cardTitle cty
          , HE.onClick (HE.input_ (right ∘ AddCard cty))
          ]
          [ CT.cardGlyph cty
          , HH.p_ [ HH.text (CT.cardName cty) ]
          ]
      ]

  disabledButton ∷ CT.CardType → NextHTML
  disabledButton cty =
    HH.li_
      [ HH.button
          [ HP.title $ disabledTitle cty
          , ARIA.label $ disabledTitle cty
          , HP.disabled true
          ]
          [ CT.cardGlyph cty
          , HH.p_ [ HH.text (CT.cardName cty) ]
          ]
      ]

eval :: QueryP ~> NextDSL
eval = coproduct cardEval nextEval

cardEval :: CC.CardEvalQuery ~> NextDSL
cardEval = case _ of
  CC.EvalCard value output next → do
    case value.input of
      Nothing →
        H.set initialState
      Just port →
        updatePort port
    pure next
  CC.Save k →
    pure $ k Card.NextAction
  CC.Load _ next →
    pure next
  CC.SetDimensions _ next →
    pure next
  CC.ModelUpdated _ next →
    pure next
  CC.ZoomIn next →
    pure next

updatePort ∷ Port.Port → NextDSL Unit
updatePort = case _ of
  Port.Blocked →
    H.modify
      $ _message ?~ "There are no available next actions"
  Port.CardError _ →
    H.modify
      $ _message ?~ "There are no available next actions (parent cards have errors)"
  Port.DownloadOptions _ →
    H.modify
      $ (_types .~ [ CT.Download ])
      ∘ (_message .~ Nothing)
  Port.VarMap _ →
    H.modify
      $ (_types .~ [ CT.Ace CT.SQLMode, CT.APIResults ])
      ∘ (_message .~ Nothing)
  Port.SlamDown _ →
    H.modify
      $ (_types .~ [ CT.Markdown ])
      ∘ (_message .~ Nothing)
  Port.ChartOptions _ →
    H.modify
      $ (_types .~ [ CT.Chart ])
      ∘ (_message .~ Nothing)
  Port.TaggedResource _ →
    H.modify
      $ (_types .~
           [ CT.JTable
           , CT.DownloadOptions
           , CT.Search
           , CT.Ace CT.SQLMode
           , CT.Viz
           , CT.Cache
           ])
      ∘ (_message .~ Nothing)
  Port.Draftboard →
    H.modify
      $ (_types .~ [ ])
      ∘ (_message .~ Nothing)

nextEval :: Query ~> NextDSL
nextEval (AddCard _ next) = pure next
