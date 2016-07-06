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

type State =
  { attemptedCardType ∷ CardType
  , reason ∷ String
  , cardPaths ∷ Array (Array CardType)
  }

data Query a = Dismiss a

comp ∷ ∀ g. Functor g ⇒ H.Component State Query g
comp = H.component { render, eval } where

  render ∷ State → H.ComponentHTML Query
  render state =
    HH.div [ HP.classes [ HH.className "deck-dialog-embed" ] ]
    [ HH.h4_ [ HH.text $ "Couldn't insert a " ++ CardType.cardName state.attemptedCardType ++ " card into this deck" ]
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
        1 → [ HH.text $ "To insert a " ++ CardType.cardName state.attemptedCardType ++ " card try adding these cards in order first." ]
        n → [ HH.text $ "To insert a " ++ CardType.cardName state.attemptedCardType ++ " card try adding one of these sets of cards in order first." ]

    renderCardPath cardPath = HH.div_ $ map renderCard cardPath

    renderCard card = HH.text ""

  eval ∷ Natural Query (H.ComponentDSL State Query g)
  eval (Dismiss next) = pure next
