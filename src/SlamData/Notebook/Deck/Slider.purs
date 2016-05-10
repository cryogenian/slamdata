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

module SlamData.Notebook.Deck.Slider
  ( startSliding
  , stopSlidingAndSnap
  , updateSliderPositionAndSetSliderSelectedCardId
  , setNextActionCardElement
  , setSliderTransition
  , cardWidthCSS
  , render
  , containerProperties
  ) where

import CSS (CSS)
import SlamData.Notebook.AccessType as AccessType
import Control.Monad.Aff.Free (fromEff)
import DOM.HTML.Types (HTMLElement)
import Data.Int as Int
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
import SlamData.Notebook.Card.CardId (CardId)
import SlamData.Notebook.Card.CardId as CardId
import SlamData.Notebook.Deck.Common (NotebookHTML, NotebookDSL)
import SlamData.Notebook.Card.Component as Card
import SlamData.Notebook.Deck.Gripper as Gripper
import SlamData.Notebook.Deck.Component.Query (Query)
import SlamData.Notebook.Deck.Component.Query as Query
import SlamData.Notebook.Deck.Component.ChildSlot as ChildSlot
import SlamData.Notebook.Card.Next.Component as Next
import SlamData.Notebook.Deck.Component.State (State, CardDef)
import SlamData.Notebook.Deck.Component.State as State
import SlamData.Prelude
import SlamData.Render.CSS as ClassNames
import Utils.CSS as CSSUtils
import Utils.DOM (getBoundingClientRect)

render :: State -> Boolean -> NotebookHTML
render state visible =
  HH.div
    ([ HP.key "notebook-cards"
     , HP.classes [ ClassNames.cardSlider ]
     , HE.onTransitionEnd $ HE.input_ Query.StopSliderTransition
     , style
         $ (cardSliderWidthCSS $ List.length state.cards + 1)
         *> (cardSliderTransformCSS (List.length state.cards + 1) (State.activeCardIndex state) state.sliderTranslateX)
         *> (cardSliderTransitionCSS state.sliderTransition)
     ]
       ⊕ (guard (not visible) $> (HP.class_ ClassNames.invisible)))
    ((List.fromList (map (renderCard state) state.cards)) ⊕ [ renderNextActionCard state ])

startSliding :: Event MouseEvent -> NotebookDSL Unit
startSliding mouseEvent =
  setInitialSliderX (Just mouseEvent.screenX)
    *> (setInitialSliderCardWidth =<< getCardWidth)
    *> setSliderTransition false
    *> setBacksided false

stopSlidingAndSnap :: NotebookDSL Unit
stopSlidingAndSnap =
  startTransition *> snap *> stopSliding

updateSliderPositionAndSetSliderSelectedCardId :: Event MouseEvent -> NotebookDSL Unit
updateSliderPositionAndSetSliderSelectedCardId mouseEvent =
  (updateSliderPosition mouseEvent.screenX =<< getInitialX)
    *> (setSliderSelectedCardId =<< getSnappedActiveCardId)

setNextActionCardElement :: Maybe HTMLElement -> NotebookDSL Unit
setNextActionCardElement =
  H.modify <<< (State._nextActionCardElement .~ _)

setSliderTransition :: Boolean -> NotebookDSL Unit
setSliderTransition =
  H.modify <<< (State._sliderTransition .~ _)

updateSliderPosition :: Number -> Maybe Number -> NotebookDSL Unit
updateSliderPosition screenX =
  maybe (pure unit) (setTranslateX <<< translateXCalc screenX)

getInitialX :: NotebookDSL (Maybe Number)
getInitialX =
  H.gets _.initialSliderX

translateXCalc :: Number -> Number -> Number
translateXCalc eventScreenX initialX =
  eventScreenX - initialX

setTranslateX :: Number -> NotebookDSL Unit
setTranslateX =
  H.modify <<< (State._sliderTranslateX .~ _)

setInitialSliderX :: Maybe Number -> NotebookDSL Unit
setInitialSliderX =
  H.modify <<< (State._initialSliderX .~ _)

setInitialSliderCardWidth :: Maybe Number -> NotebookDSL Unit
setInitialSliderCardWidth =
  H.modify <<< (State._initialSliderCardWidth .~ _)

setBacksided :: Boolean -> NotebookDSL Unit
setBacksided =
  H.modify <<< (State._backsided .~ _)

setActiveCardId :: Maybe CardId -> NotebookDSL Unit
setActiveCardId =
  H.modify <<< (State._activeCardId .~ _)

setInitialX :: Maybe Number -> NotebookDSL Unit
setInitialX =
  H.modify <<< (State._initialSliderX .~ _)

setSliderSelectedCardId :: Maybe CardId -> NotebookDSL Unit
setSliderSelectedCardId =
  H.modify <<< (State._sliderSelectedCardId .~ _)

stopSliding :: NotebookDSL Unit
stopSliding =
  setInitialX Nothing *> setTranslateX 0.0

getBoundingClientWidth :: HTMLElement -> NotebookDSL Number
getBoundingClientWidth =
  fromEff <<< map _.width <<< getBoundingClientRect

getNextActionCardElement :: NotebookDSL (Maybe HTMLElement)
getNextActionCardElement =
  H.gets _.nextActionCardElement

getCardWidth :: NotebookDSL (Maybe Number)
getCardWidth =
  traverse getBoundingClientWidth =<< getNextActionCardElement

getCardIdByIndex :: List.List CardDef -> Int -> Maybe CardId
getCardIdByIndex cards =
  map _.id <<< List.index cards

getSnappedActiveCardId :: NotebookDSL (Maybe CardId)
getSnappedActiveCardId =
  snapActiveCardId <$> H.get

getInitialSliderCardWidth :: NotebookDSL (Maybe Number)
getInitialSliderCardWidth = H.gets _.initialSliderCardWidth

snapActiveCardIndex :: Number -> Number -> Int -> Int
snapActiveCardIndex translateX cardWidth
  | translateX <= -(f cardWidth / 2.0) =
    flip sub (1 + Int.floor ((translateX - (f cardWidth / 2.0)) / cardWidth))
  | translateX >= (f cardWidth / 2.0) =
    max 0 <<< add (1 + Int.floor ((-translateX - (f cardWidth / 2.0)) / cardWidth))
  | otherwise =
    id

f = add $ cardSpacingGridSquares * Config.gridPx

snapActiveCardId :: State -> Maybe CardId
snapActiveCardId st =
  Debug.Trace.traceAny st.initialSliderCardWidth \_ ->
    getCardIdByIndex st.cards
      $ maybe' (const id) (snapActiveCardIndex st.sliderTranslateX) st.initialSliderCardWidth
      $ State.activeCardIndex st

snap :: NotebookDSL Unit
snap =
  setActiveCardId =<< getSnappedActiveCardId

startTransition :: NotebookDSL Unit
startTransition =
  setSliderTransition true

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
  [ ARIA.disabled
      $ show $ not $ cardSelected state cardId
  , ARIA.hidden
      $ show $ not $ cardPresented state cardId
  ]

cardSpacingGridSquares :: Number
cardSpacingGridSquares = 2.0

cardSpacingPx :: Number
cardSpacingPx = cardSpacingGridSquares * Config.gridPx

renderCard :: State -> CardDef -> NotebookHTML
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

renderNextActionCard :: State -> NotebookHTML
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
