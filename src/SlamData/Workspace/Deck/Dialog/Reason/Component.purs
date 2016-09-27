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

module SlamData.Workspace.Deck.Dialog.Reason.Component where

import SlamData.Prelude

import Data.Array as Array
import Halogen as H
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP
import Halogen.Themes.Bootstrap3 as B
import SlamData.Workspace.Card.CardType (CardType)
import SlamData.Workspace.Card.CardType as CardType
import SlamData.Workspace.Card.InsertableCardType (InsertableCardType, print)

type State =
  { attemptedCardType ∷ CardType
  , reason ∷ String
  , cardPaths ∷ Array (Array InsertableCardType)
  }

data Query a = Dismiss a

comp ∷ ∀ g. Functor g ⇒ H.Component State Query g
comp = H.component { render, eval } where

  render ∷ State → H.ComponentHTML Query
  render state =
    HH.div
    [ HP.classes [ HH.className "deck-dialog-embed" ] ]
    [ HH.h4_
        [ HH.text
          $ "Couldn't insert a "
          <> CardType.cardName state.attemptedCardType
          <> " card into this deck"
        ]
    , HH.div
        [ HP.classes [ HH.className "deck-dialog-body" ] ]
        [ HH.p_
            [ HH.text state.reason
            ]
        , HH.p_ renderCardPathsMessage
        , HH.div_ $ map renderCardPath state.cardPaths
        ]
    , HH.div
        [ HP.classes [ HH.className "deck-dialog-footer" ] ]
        [ HH.button
            [ HP.classes [ B.btn ]
            , HE.onClick (HE.input_ $ Dismiss)
            ]
            [ HH.text "Dismiss" ]
        ]
    ]
    where
    renderCardPathsMessage =
      case Array.length state.cardPaths of
        0 → []
        i →
          [ HH.text
              $ "To be able to insert a "
              <> show (CardType.cardName state.attemptedCardType)
              <> " card here you can add " <> setsOfCardsText i <> " first:"
          ]

    onlySingleCardPaths = Array.nub (Array.length <$> state.cardPaths) == [1]

    setsOfCardsText =
      case _ of
        1 → if onlySingleCardPaths then "this card" else "these cards in order"
        _ → if onlySingleCardPaths then "any of these cards" else "any of these sets of cards in order"

    renderCardPath cardPath =
      HH.p
        [ HP.classes [ HH.className "deck-dialog-cardpath" ] ]
        (map renderCard cardPath)

    renderCard card =
      HH.span
        [ HP.classes [HH.className "deck-dialog-cardpath-card" ] ]
        [ HH.text $ print card ]

  eval ∷ Query ~> H.ComponentDSL State Query g
  eval (Dismiss next) = pure next
