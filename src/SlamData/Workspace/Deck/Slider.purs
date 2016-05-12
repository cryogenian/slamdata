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
  , updateSliderPositionAndSetSliderSelectedCardId
  , setLens
  , cardWidthCSS
  , render
  , containerProperties
  ) where

import CSS (CSS)
import SlamData.Workspace.AccessType as AccessType
import Control.Monad.Aff.Free (fromEff)
import DOM.HTML.Types (HTMLElement)
import Data.Int as Int
import Data.Lens (LensP)
import Data.Lens.Setter ((.~))
import Data.List as List
import Data.Ord (max)
import Halogen as H
import Halogen.HTML.CSS.Indexed (style)
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Events.Handler as HEH
import Halogen.Component.ChildPath (injSlot, injState)
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Events.Types (Event, MouseEvent)
import Halogen.HTML.Properties.Indexed (IProp(), I)
import Halogen.HTML.Properties.Indexed as HP
import Halogen.HTML.Properties.Indexed.ARIA as ARIA
import SlamData.Config as Config
import SlamData.Workspace.Card.CardId (CardId)
import SlamData.Workspace.Card.CardId as CardId
import SlamData.Workspace.Deck.Common (DeckHTML, DeckDSL)
import SlamData.Workspace.Card.Component as Card
import SlamData.Workspace.Deck.Gripper as Gripper
import SlamData.Workspace.Deck.Component.Query (Query)
import SlamData.Workspace.Deck.Component.Query as Query
import SlamData.Workspace.Deck.Component.ChildSlot as ChildSlot
import SlamData.Workspace.Card.Next.Component as Next
import SlamData.Workspace.Deck.Component.State (State, CardDef)
import SlamData.Workspace.Deck.Component.State as State
import SlamData.Prelude
import SlamData.Render.CSS as ClassNames
import Utils.CSS as CSSUtils
import Utils.DOM (getBoundingClientRect)

render :: State -> Boolean -> DeckHTML
render state visible =
  HH.div
    ([ HP.key "deck-cards"
     , HP.classes [ ClassNames.cardSlider ]
     , HE.onTransitionEnd $ HE.input_ Query.StopSliderTransition
     , style
         $ (cardSliderWidthCSS $ List.length state.cards + 1)
         *> (cardSliderTransformCSS (List.length state.cards + 1) (State.activeCardIndex state) state.sliderTranslateX)
         *> (cardSliderTransitionCSS state.sliderTransition)
     ]
       ⊕ (guard (not visible) $> (HP.class_ ClassNames.invisible)))
    ((List.fromList (map (renderCard state) state.cards)) ⊕ [ renderNextActionCard state ])

startSliding :: Event MouseEvent -> DeckDSL Unit
startSliding mouseEvent =
  setLens State._initialSliderX (Just mouseEvent.screenX)
    *> (setLens State._initialSliderCardWidth =<< getCardWidth)
    *> setLens State._sliderTransition false
    *> setLens State._backsided false

stopSlidingAndSnap :: Event MouseEvent -> DeckDSL Unit
stopSlidingAndSnap mouseEvent =
  updateSliderPositionAndSetSliderSelectedCardId mouseEvent
    *> startTransition
    *> snap
    *> stopSliding

updateSliderPositionAndSetSliderSelectedCardId :: Event MouseEvent -> DeckDSL Unit
updateSliderPositionAndSetSliderSelectedCardId mouseEvent =
  (updateSliderPosition mouseEvent.screenX =<< H.gets _.initialSliderX)
    *> (setLens State._sliderSelectedCardId =<< getSnappedActiveCardId)

updateSliderPosition :: Number -> Maybe Number -> DeckDSL Unit
updateSliderPosition screenX =
  maybe (pure unit) (setLens State._sliderTranslateX <<< translateXCalc screenX)

translateXCalc :: Number -> Number -> Number
translateXCalc eventScreenX initialX =
  eventScreenX - initialX

setLens :: forall b. LensP State b -> b -> DeckDSL Unit
setLens lens =
  H.modify <<< (lens .~ _)

stopSliding :: DeckDSL Unit
stopSliding =
  setLens State._initialSliderX Nothing *> setLens State._sliderTranslateX 0.0

getBoundingClientWidth :: HTMLElement -> DeckDSL Number
getBoundingClientWidth =
  fromEff <<< map _.width <<< getBoundingClientRect

getCardWidth :: DeckDSL (Maybe Number)
getCardWidth =
  traverse getBoundingClientWidth =<< H.gets _.nextActionCardElement

getCardIdByIndex :: List.List CardDef -> Int -> Maybe CardId
getCardIdByIndex cards =
  map _.id <<< List.index cards

getSnappedActiveCardId :: DeckDSL (Maybe CardId)
getSnappedActiveCardId =
  snapActiveCardId <$> H.get

snapActiveCardIndex :: Number -> Number -> Int -> Int
snapActiveCardIndex translateX cardWidth
  | translateX <= -(offsetCardSpacing cardWidth / 2.0) =
    flip sub (1 + Int.floor ((translateX - (offsetCardSpacing cardWidth / 2.0)) / cardWidth))
  | translateX >= (offsetCardSpacing cardWidth / 2.0) =
    max 0 <<< add (1 + Int.floor ((-translateX - (offsetCardSpacing cardWidth / 2.0)) / cardWidth))
  | otherwise =
    id

offsetCardSpacing :: Number -> Number
offsetCardSpacing = add $ cardSpacingGridSquares * Config.gridPx

snapActiveCardId :: State -> Maybe CardId
snapActiveCardId st =
  getCardIdByIndex st.cards
    $ maybe' (const id) (snapActiveCardIndex st.sliderTranslateX) st.initialSliderCardWidth
    $ State.activeCardIndex st

snap :: DeckDSL Unit
snap =
  setLens State._activeCardId =<< getSnappedActiveCardId

startTransition :: DeckDSL Unit
startTransition =
  setLens State._sliderTransition true

willChangeActiveCardWhenDropped :: State -> Boolean
willChangeActiveCardWhenDropped st =
  st.activeCardId ≠ st.sliderSelectedCardId

cardWidthPct :: Int -> Number
cardWidthPct cardsCount =
  100.0 / Int.toNumber cardsCount

cardWidthCSS :: Int -> CSS
cardWidthCSS cardsCount =
  CSSUtils.width $ CSSUtils.calc
    $ (show $ cardWidthPct cardsCount) ++ "%"
    ++ " - " ++ show cardSpacingPx ++ "px"

cardSliderWidthCSS :: Int -> CSS
cardSliderWidthCSS cardsCount =
  CSSUtils.width $ CSSUtils.calc
    $ (show $ 100.0 * Int.toNumber cardsCount) ++ "%"
    ++ " + " ++ (show $ cardSpacingPx * Int.toNumber cardsCount) ++ "px"

cardSliderTransformCSS :: Int -> Int -> Number -> CSS
cardSliderTransformCSS cardCount activeCardIndex translateX =
  CSSUtils.transform
    $ CSSUtils.translate3d (cardSliderTranslateX cardCount activeCardIndex translateX) "0" "0"

cardSliderTransitionCSS :: Boolean -> CSS
cardSliderTransitionCSS false = CSSUtils.transition "none"
cardSliderTransitionCSS true = CSSUtils.transition "all 0.33s"

cardSliderTranslateX :: Int -> Int -> Number -> String
cardSliderTranslateX cardCount activeCardIndex translateX =
  CSSUtils.calc
    $ "((((-100% / " ++ show cardCount ++ ")))"
    ++ " * " ++ show activeCardIndex ++ ")"
    ++ " + (" ++ show translateX ++ "px)"

dropEffect :: Boolean -> String
dropEffect true = "execute"
dropEffect false = "none"

nextActionCardActive :: State -> Boolean
nextActionCardActive state = isNothing state.activeCardId

containerProperties :: forall a. State -> Array (IProp (onMouseUp :: I, onMouseLeave :: I, onMouseMove :: I | a) (Query Unit))
containerProperties state =
  [ ARIA.dropEffect $ dropEffect $ willChangeActiveCardWhenDropped state ]
    ⊕ (guard (isJust state.initialSliderX)
         $> (HE.onMouseUp \e -> HEH.preventDefault $> H.action (Query.StopSlidingAndSnap e)))
    ⊕ (guard (isJust state.initialSliderX)
         $> (HE.onMouseLeave \e -> HEH.stopPropagation $> HEH.preventDefault $> H.action (Query.StopSlidingAndSnap e)))
    ⊕ (guard (isJust state.initialSliderX)
         $> (HE.onMouseMove $ HE.input Query.UpdateSliderPosition))

cardSelected :: State -> CardId -> Boolean
cardSelected state cardId =
  Just cardId == state.activeCardId

cardPresented :: State -> CardId -> Boolean
cardPresented state cardId =
  cardSelected state cardId || isJust state.initialSliderX

cardProperties :: forall a b. State -> CardId -> Array (IProp a b)
cardProperties state cardId =
  [ ARIA.disabled $ show $ not $ cardSelected state cardId ]

cardSpacingGridSquares :: Number
cardSpacingGridSquares = 2.0

cardSpacingPx :: Number
cardSpacingPx = cardSpacingGridSquares * Config.gridPx

renderCard :: State -> CardDef -> DeckHTML
renderCard state cardDef =
  HH.div
  ([ HP.key ("card" ⊕ CardId.cardIdToString cardDef.id)
   , HP.classes [ ClassNames.card ]
   , style $ cardWidthCSS (List.length state.cards + 1)
   ]
   ⊕ foldMap (viewingStyle cardDef) state.viewingCard)
  (Gripper.renderGrippers
      (cardSelected state cardDef.id)
      (isJust state.initialSliderX)
      (Gripper.gripperDefsForCardId state.cards $ Just cardDef.id)
      ⊕ [ HH.div
            (cardProperties state cardDef.id)
            [ HH.Slot $ transformCardConstructor cardDef.ctor ]
        ]
  )
  where
  viewingStyle cardDef cid =
    guard (not (cardDef.id ≡ cid))
    $> (HP.class_ ClassNames.invisible)
  transformCardConstructor (H.SlotConstructor p l) =
    H.SlotConstructor
      (injSlot ChildSlot.cpCard p)
      (l <#> \def →
        { component: H.transformChild ChildSlot.cpCard def.component
        , initialState: injState ChildSlot.cpCard def.initialState
        }
      )

renderNextActionCard :: State -> DeckHTML
renderNextActionCard state =
  HH.div
    ([ HP.key ("next-action-card")
     , HP.classes [ ClassNames.card ]
     , HP.ref (H.action <<< Query.SetNextActionCardElement)
     , style $ cardWidthCSS (List.length state.cards + 1)
     ]
       ⊕ (guard (shouldHideNextActionCard state) $> (HP.class_ ClassNames.invisible))
    )
    (Gripper.renderGrippers
       (isNothing state.activeCardId)
       (isJust state.initialSliderX)
       (Gripper.gripperDefsForCardId state.cards Nothing)
       ⊕ [ HH.slot' ChildSlot.cpCard (ChildSlot.CardSlot top) \_ →
             { component: Next.nextCardComponent
             , initialState: H.parentState Card.initialCardState
             }
         ]
    )

shouldHideNextActionCard :: State -> Boolean
shouldHideNextActionCard state =
  isJust state.viewingCard ∨ state.accessType ≡ AccessType.ReadOnly
