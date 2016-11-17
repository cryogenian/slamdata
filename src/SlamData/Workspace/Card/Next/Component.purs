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
 , module SlamData.Workspace.Card.Next.Component.Query
 , module SlamData.Workspace.Card.Next.Component.State
 ) where

import SlamData.Prelude

import Data.Foldable as F
import Data.Lens ((.~))
import Data.String as S

import Halogen as H
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP
import Halogen.HTML.Properties.Indexed.ARIA as ARIA
import Halogen.Themes.Bootstrap3 as B
import Halogen.Component.Utils as HU

import SlamData.Monad (Slam)
import SlamData.Render.Common (glyph)
import SlamData.Guide as Guide
import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.Next.NextAction as NA
import SlamData.Workspace.Card.Next.Component.Query (Query(..), _AddCardType, _PresentReason)
import SlamData.Workspace.Card.Next.Component.State (State, initialState)
import SlamData.Workspace.Card.Next.Component.State as State
import SlamData.Workspace.Card.Port as Port
import SlamData.Workspace.Card.InsertableCardType as ICT

import Utils.LocalStorage as LocalStorage

type HTML = H.ComponentHTML Query
type DSL = H.ComponentDSL State Query Slam

nextCardComponent ∷ H.Component State Query Slam
nextCardComponent = H.component { render, eval }

render ∷ State → HTML
render state =
  HH.div_
    $ (guard (not state.presentAddCardGuide) $>
      HH.form_
        [ HH.div_
            [ HH.input
                [ HP.value state.filterString
                , HE.onValueInput (HE.input UpdateFilter)
                , ARIA.label "Filter next actions"
                , HP.placeholder "Filter actions"
                ]
            , HH.button
                [ HP.buttonType HP.ButtonButton
                , HE.onClick (HE.input_ (UpdateFilter ""))
                ]
                [ glyph B.glyphiconRemove ]
            ]
        ])
    ⊕ (guard state.presentAddCardGuide $>
        Guide.render
          Guide.DownArrow
          (HH.className "sd-add-card-guide")
          DismissAddCardGuide
          (addCardGuideText state.input))
    ⊕ [ HH.ul_ $ map nextButton state.actions ]
  where

  filterString ∷ String
  filterString = S.toLower state.filterString

  cardTitle ∷ NA.NextAction → String
  cardTitle (NA.Insert cty) = "Insert a " ⊕ CT.cardName cty ⊕ " card"
  cardTitle (NA.FindOutHowToInsert cty) = "Find out how to insert a " ⊕ CT.cardName cty ⊕ " card"
  cardTitle (NA.Drill name _ _) = "Select " ⊕ name ⊕ " card category"
  cardTitle (NA.GoBack) = "Go back"

  nextButton ∷ NA.NextAction → HTML
  nextButton action =
    HH.li_
      [ HH.button attrs
          [ NA.actionGlyph action
          , HH.p_ [ HH.text $ NA.actionLabel action ]
          ]
      ]
    where
    enabled ∷ Boolean
    enabled = case action of
      NA.GoBack → true
      _ → F.any (S.contains (S.Pattern filterString) ∘ S.toLower) $ NA.searchFilters action

    attrs =
      [ HP.title $ cardTitle action
      , HP.disabled $ not enabled
      , ARIA.label $ cardTitle action
      , HP.classes classes
      , HE.onClick (HE.input_ (Selected action))
      ]

    classes ∷ Array HH.ClassName
    classes = case action of
      NA.FindOutHowToInsert _ →
        nonInsertableClassNames
      NA.Drill _ _ actions | not (F.any NA.isInsert actions) →
        nonInsertableClassNames
      _ →
        insertableClassNames

    insertableClassNames =
      [ HH.className "sd-button" ]

    nonInsertableClassNames =
        [ HH.className "sd-button", HH.className "sd-button-warning" ]

  addCardGuideText = case _ of
    Port.Initial → "To get this deck started press one of these buttons to add a card."
    _            → "To do more with this deck press one of these buttons to add a card."

eval ∷ Query ~> DSL
eval = case _ of
  AddCard _ next →
    dismissAddCardGuide $> next
  PresentReason io card next →
    pure next
  UpdateFilter str next →
    H.modify (State._filterString .~ str) $> next
  DismissAddCardGuide next →
    dismissAddCardGuide $> next
  PresentAddCardGuide next → do
    dismissed ← getDismissedAddCardGuideBefore
    H.modify $ State._presentAddCardGuide .~ not dismissed
    pure next
  Selected action next → do
    st ← H.get
    case action of
      NA.Insert cardType →
        HU.raise $ H.action $ AddCard cardType
      NA.FindOutHowToInsert cardType →
        HU.raise $ H.action $ PresentReason st.input cardType
      NA.Drill _ _ actions →
        H.modify $ (State._actions .~ actions) ∘ (State._previousActions .~ st.actions)
      NA.GoBack →
        H.modify
          $ (State._actions .~ st.previousActions)
          ∘ (State._previousActions .~ [ ])
    pure next

takesInput ∷ Port.Port → CT.CardType → Boolean
takesInput input =
  ICT.takesInput (ICT.fromPort input) ∘ ICT.fromCardType

possibleToGetTo ∷ Port.Port → CT.CardType → Boolean
possibleToGetTo input =
  ICT.possibleToGetTo (ICT.fromPort input) ∘ ICT.fromCardType

dismissedAddCardGuideKey ∷ String
dismissedAddCardGuideKey = "dismissedAddCardGuide"

getDismissedAddCardGuideBefore ∷ DSL Boolean
getDismissedAddCardGuideBefore =
  H.liftH
    $ either (const false) id
    <$> LocalStorage.getLocalStorage dismissedAddCardGuideKey

storeDismissedAddCardGuide ∷ DSL Unit
storeDismissedAddCardGuide =
  H.liftH $ LocalStorage.setLocalStorage dismissedAddCardGuideKey true

dismissAddCardGuide ∷ DSL Unit
dismissAddCardGuide =
  H.modify (State._presentAddCardGuide .~ false)
    *> storeDismissedAddCardGuide
