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
module SlamData.Workspace.Deck.Slider
  ( startSliding
  , stopSlidingAndSnap
  , updateSliderPosition
  , render
  , containerProperties
  ) where

import SlamData.Prelude

import Data.Array ((..))
import Data.Array as Array
import Data.Int as Int
import Data.Lens ((.~), (?~))
import Data.Lens as Lens
import Data.String as String
import Data.Tuple as Tuple

import CSS (CSS)

import Halogen as H
import Halogen.HTML.CSS.Indexed (style)
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Events.Types (Event, MouseEvent)
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed (IProp(), I)
import Halogen.HTML.Properties.Indexed as HP
import Halogen.HTML.Properties.Indexed.ARIA as ARIA

import SlamData.Render.CSS as ClassNames
import SlamData.Guide as Guide
import SlamData.Workspace.AccessType as AT
import SlamData.Workspace.Card.CardId (CardId)
import SlamData.Workspace.Card.CardId as CardId
import SlamData.Workspace.Card.Component as CardC
import SlamData.Workspace.Card.InsertableCardType as ICT
import SlamData.Workspace.Card.Factory as Factory
import SlamData.Workspace.Card.Model as Card
import SlamData.Workspace.Deck.Common (DeckOptions, DeckHTML, DeckDSL)
import SlamData.Workspace.Deck.Component.ChildSlot as ChildSlot
import SlamData.Workspace.Deck.Component.Cycle (DeckComponent)
import SlamData.Workspace.Deck.Component.Query (Query)
import SlamData.Workspace.Deck.Component.Query as DCQ
import SlamData.Workspace.Deck.Component.State (State)
import SlamData.Workspace.Deck.Component.State as DCS
import SlamData.Workspace.Deck.DeckId (DeckId)
import SlamData.Workspace.Deck.DeckId as DeckId
import SlamData.Workspace.Deck.Gripper as Gripper
import SlamData.Workspace.Deck.Gripper.Def (GripperDef(..))

import Utils.CSS as CSSUtils

render ∷ DeckOptions → (DeckOptions → DeckComponent) → State → Boolean → DeckHTML
render opts deckComponent st visible =
  HH.div
    ([ HP.key "deck-cards"
     , HP.classes [ ClassNames.cardSlider ]
     , HE.onTransitionEnd $ HE.input_ DCQ.StopSliderTransition
     , style do
         cardSliderTransformCSS (fromMaybe 0 st.activeCardIndex) st.sliderTranslateX
         cardSliderTransitionCSS st.sliderTransition
     ]
     ⊕ (guard (not visible) $> (HP.class_ ClassNames.invisible)))
    $ map (Tuple.uncurry $ renderCard opts deckComponent st)
    $ Array.zip st.displayCards (0 .. Array.length st.displayCards)

startSliding ∷ Event MouseEvent → GripperDef → DeckDSL Unit
startSliding mouseEvent gDef = do
  cardWidth ← H.gets _.cardElementWidth
  H.modify
    $ (DCS._initialSliderX .~ Just mouseEvent.screenX)
    ∘ (DCS._initialSliderCardWidth .~ cardWidth)
    ∘ (DCS._sliderTransition .~ false)
    ∘ (DCS._fadeTransition .~ DCS.FadeIn)
    ∘ (DCS._displayMode .~ DCS.Normal)
    ∘ (DCS._slidingTo ?~ gDef)

stopSlidingAndSnap ∷ Event MouseEvent → DeckDSL Unit
stopSlidingAndSnap mEvent =
  H.modify
    $ stopSliding
    ∘ snap
    ∘ stateUpdateSliderPosition mEvent

stateUpdateSliderPosition ∷ Event MouseEvent → State → State
stateUpdateSliderPosition mouseEvent =
  maybe id (Lens.set DCS._sliderTranslateX ∘ translateXCalc mouseEvent.screenX)
    <$> _.initialSliderX
    <*> id

updateSliderPosition ∷ Event MouseEvent → DeckDSL Unit
updateSliderPosition = H.modify ∘ stateUpdateSliderPosition

translateXCalc ∷ Number → Number → Number
translateXCalc eventScreenX initialX =
  eventScreenX - initialX

stopSliding ∷ State → State
stopSliding =
  (DCS._initialSliderX .~ Nothing)
  ∘ (DCS._sliderTranslateX .~ 0.0)
  ∘ (DCS._slidingTo .~ Nothing)
  ∘ (DCS._fadeTransition .~ DCS.FadeOut)
  ∘ startTransition

clickBound ∷ Number
clickBound = 4.0

isClick ∷ Number -> Boolean
isClick n = n < clickBound ∧ n > -clickBound

snapActiveCardIndexByTranslationAndCardWidth
  ∷ State
  → Number
  → Int
  → Int
snapActiveCardIndexByTranslationAndCardWidth st cardWidth idx =
  let
    translateX = st.sliderTranslateX

    numberOfCards = Array.length st.displayCards

    halfOffset = (offsetCardSpacing cardWidth) / 2.0

    result | isClick translateX  = case st.slidingTo of
      Just (Previous true) → min (numberOfCards - 1) $ idx - one
      Just (Next true) → max 0 $ idx + one
      _ → idx
    result | translateX <= -1.0 * halfOffset =
      min (numberOfCards - 1)
      $ sub idx
      $ one
      + Int.floor ((translateX - halfOffset) / cardWidth)
    result | translateX >= halfOffset =
      max 0
      $ idx
      + one
      + Int.floor ((-translateX - halfOffset) / cardWidth)
    result | otherwise =
      idx
  in
    result

offsetCardSpacing ∷ Number → Number
offsetCardSpacing = add cardSpacingPx

snapActiveCardIndex ∷ State → Int
snapActiveCardIndex st = maybe id snap' st.initialSliderCardWidth $ fromMaybe 0 st.activeCardIndex
  where
  snap' = snapActiveCardIndexByTranslationAndCardWidth st

snap ∷ State → State
snap st = st { activeCardIndex = Just $ snapActiveCardIndex st }

startTransition ∷ State → State
startTransition = DCS._sliderTransition .~ true

willChangeActiveCardWhenDropped ∷ State → Boolean
willChangeActiveCardWhenDropped st =
  fromMaybe 0 st.activeCardIndex ≠ snapActiveCardIndex st

cardPositionCSS ∷ Int → CSS
cardPositionCSS index = do
  CSSUtils.left $ CSSUtils.calc $
    "(100% + " ⊕ show cardSpacingPx ⊕ "px) * " ⊕ show index

cardSliderTransformCSS ∷ Int → Number → CSS
cardSliderTransformCSS activeCardIndex translateX =
  CSSUtils.transform
    $ CSSUtils.translate3d ((show (-100 * activeCardIndex)) ⊕ "%") "0" "0"
    ⊕ CSSUtils.translate3d ((show (-cardSpacingPx * Int.toNumber activeCardIndex)) ⊕ "px") "0" "0"
    ⊕ CSSUtils.translate3d ((show translateX) ⊕ "px") "0" "0"

cardSliderTransitionCSS ∷ Boolean → CSS
cardSliderTransitionCSS false = CSSUtils.transition "none"
cardSliderTransitionCSS true = CSSUtils.transition "all 0.125s"

dropEffect ∷ Boolean → String
dropEffect true = "execute"
dropEffect false = "none"

containerProperties
  ∷ ∀ a
  . State
  → Array (IProp (onMouseUp ∷ I, onMouseLeave ∷ I, onMouseMove ∷ I | a) (Query Unit))
containerProperties st =
  [ ARIA.dropEffect $ dropEffect $ willChangeActiveCardWhenDropped st ]
    ⊕ (guard (isJust st.initialSliderX)
         $> (HE.onMouseUp \e →
                pure $ Just (H.action (DCQ.StopSlidingAndSnap e))))
    ⊕ (guard (isJust st.initialSliderX)
         $> (HE.onMouseLeave \e →
                pure $ Just (H.action (DCQ.StopSlidingAndSnap e))))
    ⊕ (guard (isJust st.initialSliderX)
         $> (HE.onMouseMove $ HE.input DCQ.UpdateSliderPosition))

cardSelected ∷ State → DeckId × CardId → Boolean
cardSelected state coord =
  Just coord ≡ DCS.activeCardCoord state

cardPending ∷ State → DeckId × CardId → Boolean
cardPending state coord =
  case state.pendingCard of
    Just pcoord →
      case DCS.compareCoordCards coord pcoord state.modelCards of
        Just GT → true
        _ → false
    _ → false

cardProperties ∷ ∀ a b. State → DeckId × CardId → Array (IProp a b)
cardProperties state coord =
  [ ARIA.disabled ∘ show ∘ not $ cardSelected state coord ]

cardSpacingGridSquares ∷ Number
cardSpacingGridSquares = 2.0

cardSpacingPx ∷ Number
cardSpacingPx = cardSpacingGridSquares * 24.0

renderCard ∷ DeckOptions → (DeckOptions → DeckComponent) → State → (DeckId × Card.Model) → Int → DeckHTML
renderCard opts deckComponent st (deckId × card) index =
  HH.div
    [ HP.key key
    , HP.classes classes
    , style $ cardPositionCSS index
    ]
    if opts.accessType == AT.ReadOnly
    then
      [ HH.div
          (cardProperties st coord)
          [ HH.slot' ChildSlot.cpCard slotId \_ → cardComponent ]
      , loadingPanel
      ]
    else
      Gripper.renderGrippers
        (cardSelected st (deckId × card.cardId))
        (isJust st.initialSliderX)
        (Gripper.gripperDefsForCard st.displayCards $ Just coord)
        ⊕ [ HH.div
              (cardProperties st coord)
              ([ HH.slot' ChildSlot.cpCard slotId \_ → cardComponent ]
                ⊕ (guard presentAccessNextActionCardGuide $> renderGuide))
          , loadingPanel
          ]
  where
  key = "card-" ⊕ DeckId.deckIdToString deckId ⊕ "-" ⊕ CardId.cardIdToString card.cardId
  isLastRealCard = Just (deckId × card.cardId) == DCS.findLastRealCard st
  presentAccessNextActionCardGuide = st.presentAccessNextActionCardGuide ∧ isLastRealCard
  renderGuide =
    Guide.render
      Guide.RightArrow
      (HH.className "sd-access-next-card-guide")
      DCQ.HideAccessNextActionCardGuide
      guideText
  outputs = maybe [] ICT.outputsFor $ ICT.fromCardType (Card.modelCardType card.model)
  guideText = guideText' ∘ String.joinWith " / " $ Array.catMaybes $ ICT.printIOType' <$> outputs
  guideText' outputTypesString =
    "To do more with "
      ⊕ outputTypesString
      ⊕ " click or drag this gripper to the left and add a new card to the deck."
  classes =
    [ ClassNames.card
    , HH.className case st.fadeTransition of
        DCS.FadeIn   → "sd-fade-in"
        DCS.FadeOut  → "sd-fade-out"
        DCS.FadeNone → "sd-fade-none"
    ]
      ⊕ (guard (not $ isClick st.sliderTranslateX) $> ClassNames.cardSliding)
      ⊕ (guard (cardSelected st coord) $> ClassNames.cardActive)
      ⊕ (guard (cardPending st coord) $> ClassNames.pending)
  coord = deckId × card.cardId
  slotId = ChildSlot.CardSlot coord
  cardOpts =
    { deck: opts
    , deckComponent
    , cardId: card.cardId
    , deckId: deckId
    }

  cardComponent =
    { component: Factory.cardComponent st.id card cardOpts
    , initialState: H.parentState CardC.initialCardState
    }

loadingPanel ∷ DeckHTML
loadingPanel =
  HH.div
    [ HP.classes [ HH.className "sd-pending-overlay" ] ]
    [ HH.div_
        [ HH.i_ []
        , HH.span_ [ HH.text "Please wait while this card is evaluated" ]
        ]
    ]
