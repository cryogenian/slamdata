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
  , CardDef
  , initialDeck
  , _id
  , _name
  , _parent
  , _modelCards
  , _displayCards
  , _activeCardIndex
  , _presentAccessNextActionCardGuideCanceler
  , _presentAccessNextActionCardGuide
  , _saveTrigger
  , _runTrigger
  , _pendingCard
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
  , _additionalSources
  , _responsiveSize
  , _fadeTransition
  , addCard
  , addCard'
  , removeCard
  , findLastCardType
  , findLastCardIndex
  , findLastCard
  , addPendingCard
  , removePendingCard
  , variablesCards
  , fromModel
  , cardIndexFromCoord
  , cardCoordFromIndex
  , activeCardCoord
  , activeCardType
  , prevCard
  , eqCoordModel
  , compareCoordCards
  , coordModelToCoord
  , defaultActiveIndex
  ) where

import SlamData.Prelude

import Control.Monad.Aff.EventLoop (Breaker)
import Control.Monad.Aff (Canceler)

import DOM.HTML.Types (HTMLElement)

import Data.Array as A
import Data.Foldable (maximum)
import Data.Lens (LensP, lens)
import Data.Lens as Lens
import Data.Map as Map
import Data.Path.Pathy ((</>))
import Data.Set as Set

import Halogen.Component.Opaque.Unsafe (OpaqueState)
import Halogen.Component.Utils.Debounced (DebounceTrigger)

import SlamData.Effects (SlamDataEffects)
import SlamData.Monad (Slam)

import SlamData.Workspace.Card.CardId (CardId(..))
import SlamData.Workspace.Card.CardId as CID
import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.Model as Card
import SlamData.Workspace.StateMode (StateMode(..))

import SlamData.Workspace.Deck.AdditionalSource (AdditionalSource)
import SlamData.Workspace.Deck.Component.Query (Query)
import SlamData.Workspace.Deck.DeckId (DeckId)
import SlamData.Workspace.Deck.Gripper.Def (GripperDef)

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

derive instance eqDisplayMode ∷ Eq DisplayMode

data Fade
  = FadeNone
  | FadeIn
  | FadeOut

-- | The deck state. See the corresponding lenses for descriptions of
-- | the fields.
type State =
  { id ∷ DeckId
  , name ∷ String
  , parent ∷ Maybe (DeckId × CardId)
  , fresh ∷ Int
  , modelCards ∷ Array (DeckId × Card.Model)
  , displayCards ∷ Array (DeckId × Card.Model)
  , cardsToLoad ∷ Set.Set (DeckId × CardId)
  , activeCardIndex ∷ Maybe Int
  , presentAccessNextActionCardGuideCanceler ∷ Maybe (Canceler SlamDataEffects)
  , presentAccessNextActionCardGuide ∷ Boolean
  , saveTrigger ∷ Maybe (DebounceTrigger Query Slam)
  , runTrigger ∷ Maybe (DebounceTrigger Query Slam)
  , pendingCard ∷ Maybe (DeckId × CardId)
  , stateMode ∷ StateMode
  , displayMode ∷ DisplayMode
  , initialSliderX ∷ Maybe Number
  , initialSliderCardWidth ∷ Maybe Number
  , sliderTransition ∷ Boolean
  , sliderTranslateX ∷ Number
  , cardElementWidth ∷ Maybe Number
  , slidingTo ∷ Maybe GripperDef
  , breakers ∷ Array (Breaker Unit)
  , focused ∷ Boolean
  , finalized ∷ Boolean
  , deckElement ∷ Maybe HTMLElement
  , additionalSources ∷ Map.Map (DeckId × CardId) (Set.Set AdditionalSource)
  , responsiveSize ∷ ResponsiveSize
  , fadeTransition ∷ Fade
  }

-- | A record used to represent card definitions in the deck.
type CardDef = { id ∷ CardId, ty ∷ CT.CardType }

-- | Constructs a default `State` value.
initialDeck ∷ DeckId → State
initialDeck deckId =
  { id: deckId
  , name: ""
  , parent: Nothing
  , fresh: 0
  , modelCards: mempty
  , displayCards: mempty
  , cardsToLoad: mempty
  , activeCardIndex: Nothing
  , presentAccessNextActionCardGuideCanceler: Nothing
  , presentAccessNextActionCardGuide: false
  , saveTrigger: Nothing
  , runTrigger: Nothing
  , pendingCard: Nothing
  , stateMode: Loading
  , displayMode: Normal
  , initialSliderX: Nothing
  , initialSliderCardWidth: Nothing
  , sliderTransition: false
  , sliderTranslateX: 0.0
  , cardElementWidth: Nothing
  , slidingTo: Nothing
  , breakers: mempty
  , focused: false
  , finalized: false
  , deckElement: Nothing
  , additionalSources: mempty
  , responsiveSize: XLarge
  , fadeTransition: FadeNone
  }

-- | The unique identifier of the deck.
_id ∷ ∀ a r. LensP {id ∷ a|r} a
_id = lens _.id _{id = _}

-- | The name of the deck. Initially Nothing.
_name ∷ ∀ a r. LensP {name ∷ a|r} a
_name = lens _.name _{name = _}

-- | A pointer to the parent deck/card. If `Nothing`, the deck is assumed to be
-- | the root deck.
_parent ∷ ∀ a r. LensP {parent ∷ a|r} a
_parent = lens _.parent _{parent = _}

-- | A counter used to generate `CardId` values. This should be a monotonically increasing value
_fresh ∷ ∀ a r. LensP {fresh ∷ a|r} a
_fresh = lens _.fresh _{fresh = _}

-- | The list of cards currently in the deck.
_modelCards ∷ ∀ a r. LensP {modelCards ∷ a |r} a
_modelCards = lens _.modelCards _{modelCards = _}

-- | The list of cards to be displayed in the deck
_displayCards ∷ ∀ a r. LensP {displayCards ∷ a |r} a
_displayCards = lens _.displayCards _{displayCards = _}

-- | The `CardId` for the currently focused card. `Nothing` indicates the next
-- | action card.
_activeCardIndex ∷ ∀ a r. LensP {activeCardIndex ∷ a |r} a
_activeCardIndex = lens _.activeCardIndex _{activeCardIndex = _}

-- | An optional canceler for the delayed guiding of the user to add a card. Can
-- | be used to reset the delay of this guiding.
_presentAccessNextActionCardGuideCanceler ∷ ∀ a r. LensP {presentAccessNextActionCardGuideCanceler ∷ a |r} a
_presentAccessNextActionCardGuideCanceler = lens _.presentAccessNextActionCardGuideCanceler _{presentAccessNextActionCardGuideCanceler = _}

-- | Whether the add card guide should be presented or not.
_presentAccessNextActionCardGuide ∷ ∀ a r. LensP {presentAccessNextActionCardGuide ∷ a |r} a
_presentAccessNextActionCardGuide = lens _.presentAccessNextActionCardGuide _{presentAccessNextActionCardGuide = _}

-- | The debounced trigger for deck save actions.
_saveTrigger ∷ ∀ a r. LensP {saveTrigger ∷ a|r} a
_saveTrigger = lens _.saveTrigger _{saveTrigger = _}

-- | The debounced trigger for running all cards that are pending.
_runTrigger ∷ ∀ a r. LensP {runTrigger ∷ a|r} a
_runTrigger = lens _.runTrigger _{runTrigger = _}

-- | The earliest card in the deck that needs to evaluate.
_pendingCard ∷ ∀ a r. LensP {pendingCard ∷ a|r} a
_pendingCard = lens _.pendingCard _{pendingCard = _}

-- | The "state mode" used to track whether the deck is ready, loading, or
-- | if an error has occurred while loading.
_stateMode ∷ ∀ a r. LensP {stateMode ∷ a|r} a
_stateMode = lens _.stateMode _{stateMode = _}

-- | backsided, dialog or normal (card)
_displayMode ∷ ∀ a r. LensP {displayMode ∷ a |r} a
_displayMode = lens (_.displayMode) (_{displayMode = _})

-- | The x position of the card slider at the start of the slide interaction in
-- | pixels. If `Nothing` slide interaction is not in progress.
_initialSliderX ∷ ∀ a r. LensP {initialSliderX ∷ a|r} a
_initialSliderX = lens _.initialSliderX _{initialSliderX = _}

-- | The width of the next action card at the start of the slide interaction in
-- | pixels. If `Nothing` either the slide interaction is not in progress or the
-- | next action card element reference is broken.
_initialSliderCardWidth ∷ ∀ a r. LensP {initialSliderCardWidth ∷ a|r} a
_initialSliderCardWidth = lens _.initialSliderCardWidth _{initialSliderCardWidth = _}

-- | Whether the translation of the card slider should be animated or not.
-- | Should be true between the end of the slide interaction and the end of the
-- | transition.
_sliderTransition ∷ ∀ a r. LensP {sliderTransition ∷ a |r} a
_sliderTransition = lens _.sliderTransition _{sliderTransition = _}

-- | The current x translation of the card slider during the slide interaction.
_sliderTranslateX ∷ ∀ a r. LensP {sliderTranslateX ∷ a |r} a
_sliderTranslateX = lens _.sliderTranslateX _{sliderTranslateX = _}

-- | The width of card
_cardElementWidth ∷ ∀ a r. LensP {cardElementWidth ∷ a|r} a
_cardElementWidth = lens _.cardElementWidth _{cardElementWidth = _}

_slidingTo ∷ ∀ a r. LensP {slidingTo ∷ a|r} a
_slidingTo = lens _.slidingTo _{slidingTo = _}

_breakers ∷ ∀ a r. LensP {breakers ∷ a|r} a
_breakers = lens _.breakers _{breakers = _}

_additionalSources ∷ ∀ a r. LensP {additionalSources ∷ a|r} a
_additionalSources = lens _.additionalSources _{additionalSources = _}

_focused ∷ ∀ a r. LensP {focused ∷ a|r} a
_focused = lens _.focused _{focused = _}

_responsiveSize ∷ ∀ a r. LensP {responsiveSize ∷ a|r} a
_responsiveSize = lens _.responsiveSize _{responsiveSize = _}

_fadeTransition ∷ ∀ a r. LensP {fadeTransition ∷ a|r} a
_fadeTransition = lens _.fadeTransition _{fadeTransition = _}

addCard ∷ Card.AnyCardModel → State → State
addCard card st = fst $ addCard' card st

addCard' ∷ Card.AnyCardModel → State → State × CardId
addCard' model st =
  let
    cardId = CardId st.fresh
    newState = st
      { fresh = st.fresh + one
      , modelCards =
          A.snoc st.modelCards (st.id × { cardId, model })
      }
  in newState × cardId

removeCard ∷ DeckId × CardId → State → (Array (DeckId × Card.Model)) × State
removeCard coord st =
  Tuple displayCards.rest
    $ removePendingCard coord
    $ st
      { modelCards = modelCards.init
      , displayCards = displayCards.init
      , activeCardIndex = Just $ max zero $ A.length displayCards.init - 1
      }
  where
  modelCards = A.span (not ∘ eqCoordModel coord) st.modelCards
  displayCards = A.span (not ∘ eqCoordModel coord) st.displayCards

findLastCardIndex ∷ State → Maybe Int
findLastCardIndex st =
  const (A.length st.displayCards - 1) <$> A.last st.displayCards

findLastCard ∷ State → Maybe (DeckId × CardId)
findLastCard state =
  coordModelToCoord <$> A.last state.displayCards

-- | Finds the type of the last card.
findLastCardType ∷ State → Maybe CT.CardType
findLastCardType { displayCards } = Card.modelCardType ∘ _.model ∘ snd <$> A.last displayCards

variablesCards ∷ State → Array (DeckId × CardId)
variablesCards = A.mapMaybe cardTypeMatches ∘ _.modelCards
  where
  cardTypeMatches (deckId × { cardId, model }) =
    case Card.modelCardType model of
      CT.Variables → Just (deckId × cardId)
      _ → Nothing

-- | Updates the stored card that is pending to run. This handles the logic of
-- | changing the pending card when a provided card appears earlier in the deck
-- | than the currently enqueued card, and if the provided card appears after
-- | the currently enqueued card the function is a no-op.
addPendingCard ∷ (DeckId × CardId) → State → State
addPendingCard coord st =
  case st.pendingCard of
    Nothing → st { pendingCard = Just coord }
    Just oldCoord → fromMaybe st $
      compareCoordCards coord oldCoord st.modelCards <#> eq LT <#>
        if _ then st { pendingCard = Just coord } else st

removePendingCard ∷ DeckId × CardId → State → State
removePendingCard coord st =
  fromMaybe st do
    oldCoord ← st.pendingCard
    comp ← (eq LT || eq EQ) <$> compareCoordCards coord oldCoord st.modelCards
    pure $ st { pendingCard = Nothing }

-- | Reconstructs a deck state from a deck model.
fromModel
  ∷ { id ∷ DeckId
    , parent ∷ Maybe (DeckId × CardId)
    , modelCards ∷ Array (DeckId × Card.Model)
    , name ∷ String
    }
  → State
  → State
fromModel { id: deckId, parent, modelCards, name } state =
  state
    { id = deckId
    , parent = parent
    , modelCards = modelCards
    , name = name
    , activeCardIndex = Nothing
    , displayMode = Normal
    , displayCards = mempty
    , fresh = fresh
    , initialSliderX = Nothing
    , runTrigger = Nothing
    , pendingCard = Nothing
    }
  where
  fresh =
    modelCards
      # A.filter (eq deckId ∘ fst)
      # A.mapMaybe (Lens.preview CID._CardId ∘ _.cardId ∘ snd)
      # maximum
      # maybe 0 (add 1)

cardIndexFromCoord ∷ DeckId × CardId → State → Maybe Int
cardIndexFromCoord coord = A.findIndex (eqCoordModel coord) ∘ _.displayCards

cardFromIndex ∷ Int → State → Maybe (DeckId × Card.Model)
cardFromIndex i st = A.index st.displayCards i

cardCoordFromIndex ∷ Int → State → Maybe (DeckId × CardId)
cardCoordFromIndex vi = map (map _.cardId) ∘ cardFromIndex vi

activeCardCoord ∷ State → Maybe (DeckId × CardId)
activeCardCoord st = cardCoordFromIndex (fromMaybe 0 st.activeCardIndex) st

prevCard ∷ DeckId × CardId → State → Maybe (DeckId × Card.Model)
prevCard coord st = do
  i ← cardIndexFromCoord coord st
  cardFromIndex (i - 1) st

activeCardType ∷ State → Maybe CT.CardType
activeCardType st =
  Card.modelCardType ∘ _.model ∘ snd <$>
    cardFromIndex (fromMaybe 0 st.activeCardIndex) st

eqCoordModel ∷ DeckId × CardId → DeckId × Card.Model → Boolean
eqCoordModel (deckId × cardId) (deckId' × model) =
  deckId ≡ deckId' && cardId ≡ model.cardId

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
