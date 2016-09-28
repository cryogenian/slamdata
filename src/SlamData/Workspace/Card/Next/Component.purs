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

import Data.Foldable as F
import Data.Lens ((.~))
import Data.String as Str

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
import SlamData.Workspace.Card.Component as CC
import SlamData.Workspace.Card.Model as Card
import SlamData.Workspace.Card.Next.NextAction as NA
import SlamData.Workspace.Card.Next.Component.Query (QueryP, Query(..), _AddCardType, _PresentReason)
import SlamData.Workspace.Card.Next.Component.State (State)
import SlamData.Workspace.Card.Next.Component.State as State
import SlamData.Workspace.Card.Port as Port
import SlamData.Workspace.Card.InsertableCardType as ICT

import Utils.LocalStorage as LocalStorage

type NextHTML = H.ComponentHTML QueryP
type NextDSL = H.ComponentDSL State QueryP Slam

nextCardComponent ∷ CC.CardComponent
nextCardComponent = CC.makeCardComponent
  { cardType: CT.NextAction
  , component:
      H.lifecycleComponent
        { initializer: Just $ right $ H.action $ Init
        , finalizer: Nothing
        , render
        , eval
        }
  , initialState: State.initialState
  , _State: CC._NextState
  , _Query: CC.makeQueryPrism CC._NextQuery
  }

render ∷ State → NextHTML
render state =
  HH.div_
    $ (guard (not state.presentAddCardGuide) $>
      HH.form_
        [ HH.div_
            [ HH.input
                [ HP.value state.filterString
                , HE.onValueInput (HE.input (\s → right ∘ UpdateFilter s))
                , ARIA.label "Filter next actions"
                , HP.placeholder "Filter actions"
                ]
            , HH.button
                [ HP.buttonType HP.ButtonButton
                , HE.onClick (HE.input_ (right ∘ UpdateFilter ""))
                ]
                [ glyph B.glyphiconRemove ]
            ]
        ])
    ⊕ (guard state.presentAddCardGuide $>
        Guide.render
          Guide.DownArrow
          (HH.className "sd-add-card-guide")
          (right ∘ DismissAddCardGuide)
          (addCardGuideText state.input))
    ⊕ [ HH.ul_ $ map nextButton state.actions ]
  where

  filterString ∷ String
  filterString = Str.toLower state.filterString

  cardTitle ∷ NA.NextAction → String
  cardTitle (NA.Insert cty) = "Insert " ⊕ CT.cardName cty ⊕ " card"
  cardTitle (NA.Drill name _ _) = "Select " ⊕ name ⊕ " card category"
  cardTitle (NA.GoBack) = "Go Back"

  nextButton ∷ NA.NextAction → NextHTML
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
      _ → F.any (Str.contains filterString ∘ Str.toLower) $ NA.searchFilters action

    attrs =
      [ HP.title $ cardTitle action
      , HP.disabled $ not enabled
      , ARIA.label $ cardTitle action
      ] ⊕ (guard warned
             $> HP.classes [ HH.className "sd-button-warning" ])
        ⊕ (guard enabled
             $> HE.onClick (HE.input_ (right ∘ Selected action)))

    warned ∷ Boolean
    warned = case action of
      NA.GoBack → false
      _ → not $ F.any (takesInput state.input) $ NA.foldToArray action

  addCardGuideTextEmptyDeck = "To get this deck started press one of these buttons to add a card."
  addCardGuideTextNonEmptyDeck = "To do more with this deck press one of these buttons to add a card."
  addCardGuideText = maybe addCardGuideTextEmptyDeck (const addCardGuideTextNonEmptyDeck)

eval ∷ QueryP ~> NextDSL
eval = coproduct cardEval nextEval

cardEval ∷ CC.CardEvalQuery ~> NextDSL
cardEval = case _ of
  CC.EvalCard value output next →
    H.modify (State._input .~ value.input) $> next
  CC.Activate next →
    pure next
  CC.Deactivate next →
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


takesInput ∷ Maybe Port.Port → CT.CardType → Boolean
takesInput input =
  maybe false (ICT.takesInput $ ICT.fromMaybePort input) ∘ ICT.fromCardType

possibleToGetTo ∷ Maybe Port.Port → CT.CardType → Boolean
possibleToGetTo input =
  maybe false (ICT.possibleToGetTo $ ICT.fromMaybePort input) ∘ ICT.fromCardType


dismissedAddCardGuideKey ∷ String
dismissedAddCardGuideKey = "dismissedAddCardGuide"

getDismissedAddCardGuideBefore ∷ NextDSL Boolean
getDismissedAddCardGuideBefore =
  H.liftH
    $ either (const $ false) id
    <$> LocalStorage.getLocalStorage dismissedAddCardGuideKey

storeDismissedAddCardGuide ∷ NextDSL Unit
storeDismissedAddCardGuide =
  H.liftH $ LocalStorage.setLocalStorage dismissedAddCardGuideKey true

dismissAddCardGuide ∷ NextDSL Unit
dismissAddCardGuide =
  H.modify (State._presentAddCardGuide .~ false) *> storeDismissedAddCardGuide

nextEval ∷ Query ~> NextDSL
nextEval (AddCard _ next) = dismissAddCardGuide $> next
nextEval (PresentReason io card next) = pure next
nextEval (UpdateFilter str next) = do
  H.modify
    $ (State._filterString .~ str)
  pure next
nextEval (DismissAddCardGuide next) = dismissAddCardGuide $> next
nextEval (Init next) = do
  H.modify ∘ (State._presentAddCardGuide .~ _) ∘ not =<< getDismissedAddCardGuideBefore
  pure next
nextEval (Selected action next) = do
  st ← H.get
  case action of
    NA.Insert cardType → do
      HU.raise
        $ right
        $ H.action
          if takesInput st.input cardType
          then AddCard cardType
          else PresentReason st.input cardType
    NA.Drill _ _ actions → do
      H.modify
        $ (State._actions .~ actions)
        ∘ (State._previousActions .~ st.actions)
    NA.GoBack →
      H.modify
        $ (State._actions .~ st.previousActions)
        ∘ (State._previousActions .~ [ ])
  pure next
