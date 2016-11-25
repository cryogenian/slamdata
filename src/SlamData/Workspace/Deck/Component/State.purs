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

module SlamData.Workspace.Deck.Component.State
  ( StateP
  , State
  , DisplayMode(..)
  , ResponsiveSize(..)
  , Fade(..)
  , MetaCard(..)
  , DisplayCard
  , CardDef
  , initialDeck
  , _id
  , _name
  , _parent
  , _displayCards
  , _activeCardIndex
  , _presentAccessNextActionCardGuideCanceler
  , _presentAccessNextActionCardGuide
  , _stateMode
  , _displayMode
  , _initialSliderX
  , _initialSliderCardWidth
  , _sliderTransition
  , _sliderTranslateX
  , _cardElementWidth
  , _slidingTo
  , _breakers
  , _focused
  , _responsiveSize
  , _fadeTransition
  , _PendingCard
  , _NextActionCard
  , _ErrorCard
  , addMetaCard
  , findLastCardIndex
  , findLastCard
  , fromModel
  , cardIndexFromCoord
  , cardCoordFromIndex
  , activeCard
  , prevCardCoord
  , eqCoordModel
  , eqDisplayCard
  , compareCoordCards
  , coordModelToCoord
  , defaultActiveIndex
  ) where

import SlamData.Prelude

import Control.Monad.Aff.EventLoop (Breaker)
import Control.Monad.Aff (Canceler)

import DOM.HTML.Types (HTMLElement)

import Data.Array as A
import Data.Lens (Lens', lens, Prism', prism')

import Halogen.Component.Opaque.Unsafe (OpaqueState)

import SlamData.Effects (SlamDataEffects)

import SlamData.Workspace.Card.CardId (CardId)
import SlamData.Workspace.Card.CardType (CardType)
import SlamData.Workspace.Card.Model as Card
import SlamData.Workspace.Card.Port as Port
import SlamData.Workspace.StateMode (StateMode(..))

import SlamData.Workspace.Deck.DeckId (DeckId)
import SlamData.Workspace.Deck.Gripper.Def (GripperDef)

import Utils (censor)

type StateP = OpaqueState State

data DisplayMode
  = Normal
  | Backside
  | Dialog

data ResponsiveSize
  = XSmall
  | Small
  | Medium
  | Large
  | XLarge
  | XXLarge

data MetaCard
  = PendingCard
  | ErrorCard String
  | NextActionCard Port.Port

data Fade
  = FadeNone
  | FadeIn
  | FadeOut

derive instance eqDisplayMode ∷ Eq DisplayMode

derive instance eqResponsiveSize ∷ Eq ResponsiveSize

derive instance eqFade ∷ Eq Fade

type CardDef =
  { coord ∷ DeckId × CardId
  , cardType ∷ CardType
  }

type DisplayCard = Either MetaCard CardDef

type State =
  { id ∷ DeckId
  , name ∷ String
  , parent ∷ Maybe (DeckId × CardId)
  , stateMode ∷ StateMode
  , displayMode ∷ DisplayMode
  , displayCards ∷ Array DisplayCard
  , activeCardIndex ∷ Maybe Int
  , presentAccessNextActionCardGuideCanceler ∷ Maybe (Canceler SlamDataEffects)
  , presentAccessNextActionCardGuide ∷ Boolean
  , initialSliderX ∷ Maybe Number
  , initialSliderCardWidth ∷ Maybe Number
  , sliderTransition ∷ Boolean
  , sliderTranslateX ∷ Number
  , cardElementWidth ∷ Maybe Number
  , slidingTo ∷ Maybe GripperDef
  , focused ∷ Boolean
  , finalized ∷ Boolean
  , deckElement ∷ Maybe HTMLElement
  , responsiveSize ∷ ResponsiveSize
  , fadeTransition ∷ Fade
  , breakers ∷ Array (Breaker Unit)
  }

-- | Constructs a default `State` value.
initialDeck ∷ DeckId → State
initialDeck deckId =
  { id: deckId
  , name: ""
  , parent: Nothing
  , stateMode: Loading
  , displayMode: Normal
  , displayCards: mempty
  , activeCardIndex: Nothing
  , presentAccessNextActionCardGuideCanceler: Nothing
  , presentAccessNextActionCardGuide: false
  , initialSliderX: Nothing
  , initialSliderCardWidth: Nothing
  , sliderTransition: false
  , sliderTranslateX: 0.0
  , cardElementWidth: Nothing
  , slidingTo: Nothing
  , focused: false
  , finalized: false
  , deckElement: Nothing
  , responsiveSize: XLarge
  , fadeTransition: FadeNone
  , breakers: mempty
  }

-- | The unique identifier of the deck.
_id ∷ ∀ a r. Lens' {id ∷ a|r} a
_id = lens _.id _{id = _}

-- | The name of the deck. Initially Nothing.
_name ∷ ∀ a r. Lens' {name ∷ a|r} a
_name = lens _.name _{name = _}

-- | A pointer to the parent deck/card. If `Nothing`, the deck is assumed to be
-- | the root deck.
_parent ∷ ∀ a r. Lens' {parent ∷ a|r} a
_parent = lens _.parent _{parent = _}

-- | The list of cards to be displayed in the deck
_displayCards ∷ ∀ a r. Lens' {displayCards ∷ a |r} a
_displayCards = lens _.displayCards _{displayCards = _}

-- | The `CardId` for the currently focused card. `Nothing` indicates the next
-- | action card.
_activeCardIndex ∷ ∀ a r. Lens' {activeCardIndex ∷ a |r} a
_activeCardIndex = lens _.activeCardIndex _{activeCardIndex = _}

-- | An optional canceler for the delayed guiding of the user to add a card. Can
-- | be used to reset the delay of this guiding.
_presentAccessNextActionCardGuideCanceler ∷ ∀ a r. Lens' {presentAccessNextActionCardGuideCanceler ∷ a |r} a
_presentAccessNextActionCardGuideCanceler = lens _.presentAccessNextActionCardGuideCanceler _{presentAccessNextActionCardGuideCanceler = _}

-- | Whether the add card guide should be presented or not.
_presentAccessNextActionCardGuide ∷ ∀ a r. Lens' {presentAccessNextActionCardGuide ∷ a |r} a
_presentAccessNextActionCardGuide = lens _.presentAccessNextActionCardGuide _{presentAccessNextActionCardGuide = _}

-- | The "state mode" used to track whether the deck is ready, loading, or
-- | if an error has occurred while loading.
_stateMode ∷ ∀ a r. Lens' {stateMode ∷ a|r} a
_stateMode = lens _.stateMode _{stateMode = _}

-- | backsided, dialog or normal (card)
_displayMode ∷ ∀ a r. Lens' {displayMode ∷ a |r} a
_displayMode = lens (_.displayMode) (_{displayMode = _})

-- | The x position of the card slider at the start of the slide interaction in
-- | pixels. If `Nothing` slide interaction is not in progress.
_initialSliderX ∷ ∀ a r. Lens' {initialSliderX ∷ a|r} a
_initialSliderX = lens _.initialSliderX _{initialSliderX = _}

-- | The width of the next action card at the start of the slide interaction in
-- | pixels. If `Nothing` either the slide interaction is not in progress or the
-- | next action card element reference is broken.
_initialSliderCardWidth ∷ ∀ a r. Lens' {initialSliderCardWidth ∷ a|r} a
_initialSliderCardWidth = lens _.initialSliderCardWidth _{initialSliderCardWidth = _}

-- | Whether the translation of the card slider should be animated or not.
-- | Should be true between the end of the slide interaction and the end of the
-- | transition.
_sliderTransition ∷ ∀ a r. Lens' {sliderTransition ∷ a |r} a
_sliderTransition = lens _.sliderTransition _{sliderTransition = _}

-- | The current x translation of the card slider during the slide interaction.
_sliderTranslateX ∷ ∀ a r. Lens' {sliderTranslateX ∷ a |r} a
_sliderTranslateX = lens _.sliderTranslateX _{sliderTranslateX = _}

-- | The width of card
_cardElementWidth ∷ ∀ a r. Lens' {cardElementWidth ∷ a|r} a
_cardElementWidth = lens _.cardElementWidth _{cardElementWidth = _}

_slidingTo ∷ ∀ a r. Lens' {slidingTo ∷ a|r} a
_slidingTo = lens _.slidingTo _{slidingTo = _}

_breakers ∷ ∀ a r. Lens' {breakers ∷ a|r} a
_breakers = lens _.breakers _{breakers = _}

_focused ∷ ∀ a r. Lens' {focused ∷ a|r} a
_focused = lens _.focused _{focused = _}

_responsiveSize ∷ ∀ a r. Lens' {responsiveSize ∷ a|r} a
_responsiveSize = lens _.responsiveSize _{responsiveSize = _}

_fadeTransition ∷ ∀ a r. Lens' {fadeTransition ∷ a|r} a
_fadeTransition = lens _.fadeTransition _{fadeTransition = _}

_NextActionCard ∷ Prism' MetaCard Port.Port
_NextActionCard = prism' NextActionCard case _ of
  NextActionCard a → Just a
  _ → Nothing

_ErrorCard ∷ Prism' MetaCard String
_ErrorCard = prism' ErrorCard case _ of
  ErrorCard a → Just a
  _ → Nothing

_PendingCard ∷ Prism' MetaCard Unit
_PendingCard = prism' (const PendingCard) case _ of
  PendingCard → Just unit
  _ → Nothing

addMetaCard ∷ MetaCard → State → State
addMetaCard card state =
  state { displayCards = A.snoc init (Left card) }
  where
  init = A.filter isRight state.displayCards

findLastCardIndex ∷ State → Maybe Int
findLastCardIndex st =
  const (A.length st.displayCards - 1) <$> A.last st.displayCards

findLastCard ∷ State → Maybe DisplayCard
findLastCard state =
  A.last state.displayCards

-- | Reconstructs a deck state from a deck model.
fromModel
  ∷ { name ∷ String
    , parent ∷ Maybe (DeckId × CardId)
    , displayCards ∷ Array DisplayCard
    }
  → State
  → State
fromModel { name, parent, displayCards } state =
  state
    { name = name
    , parent = parent
    , displayCards = displayCards
    , displayMode = Normal
    , activeCardIndex = Nothing
    , initialSliderX = Nothing
    }

cardIndexFromCoord ∷ DeckId × CardId → State → Maybe Int
cardIndexFromCoord coord =
  A.findIndex (eq (Just coord) ∘ map _.coord ∘ censor) ∘ _.displayCards

cardCoordFromIndex ∷ Int → State → Maybe (DeckId × CardId)
cardCoordFromIndex i st =
  A.index st.displayCards i >>= either (const Nothing) (Just ∘ _.coord)

activeCard ∷ State → Maybe DisplayCard
activeCard st = A.index st.displayCards (fromMaybe 0 st.activeCardIndex)

prevCardCoord ∷ DeckId × CardId → State → Maybe (DeckId × CardId)
prevCardCoord coord st = do
  i ← cardIndexFromCoord coord st
  cardCoordFromIndex (i - 1) st

eqCoordModel ∷ DeckId × CardId → DeckId × Card.Model → Boolean
eqCoordModel (deckId × cardId) (deckId' × model) =
  deckId ≡ deckId' && cardId ≡ model.cardId

eqDisplayCard ∷ DisplayCard → DisplayCard → Boolean
eqDisplayCard (Right r1) (Right r2) = r1.coord ≡ r2.coord && r1.cardType ≡ r2.cardType
eqDisplayCard (Left l1) (Left l2) = eqMetaCard l1 l2
eqDisplayCard _ _ = false

eqMetaCard ∷ MetaCard → MetaCard → Boolean
eqMetaCard = case _, _ of
  PendingCard, PendingCard → true
  ErrorCard _, ErrorCard _ → true
  NextActionCard _, NextActionCard _ → true
  _, _ → false

compareCoordCards
  ∷ DeckId × CardId
  → DeckId × CardId
  → Array (DeckId × Card.Model)
  → Maybe Ordering
compareCoordCards coordA coordB cards =
  compare
    <$> A.findIndex (eqCoordModel coordA) cards
    <*> A.findIndex (eqCoordModel coordB) cards

coordModelToCoord ∷ DeckId × Card.Model → DeckId × CardId
coordModelToCoord = map _.cardId

defaultActiveIndex ∷ State → Int
defaultActiveIndex st =
  fromMaybe lastCardIndex lastRealCardIndex
  where
  lastCardIndex = max 0 $ A.length st.displayCards - 1
  lastRealCardIndex = findLastCardIndex st
