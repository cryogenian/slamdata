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
  , CardDef
  , initialDeck
  , _id
  , _parent
  , _accessType
  , _modelCards
  , _displayCards
  , _cardsToLoad
  , _activeCardIndex
  , _name
  , _path
  , _saveTrigger
  , _runTrigger
  , _globalVarMap
  , _pendingCard
  , _stateMode
  , _displayMode
  , _initialSliderX
  , _initialSliderCardWidth
  , _sliderTransition
  , _sliderTranslateX
  , _cardElementWidth
  , _level
  , addCard
  , addCard'
  , removeCard
  , findLastCardType
  , findLastRealCardIndex
  , findLastRealCard
  , addPendingCard
  , removePendingCard
  , cardsOfType
  , fromModel
  , deckPath
  , deckPath'
  , cardIndexFromId
  , cardIdFromIndex
  , activeCardId
  , activeCardType
  ) where

import SlamData.Prelude

import Data.Array as A
import Data.Foldable (maximum)
import Data.Lens (LensP, lens)
import Data.Lens as Lens
import Data.Ord (max)
import Data.Path.Pathy ((</>))
import Data.Path.Pathy as P
import Data.StrMap as SM
import Data.Set as Set

import Halogen.Component.Opaque.Unsafe (OpaqueState)
import Halogen.Component.Utils.Debounced (DebounceTrigger)

import SlamData.Effects (Slam)
import SlamData.Workspace.AccessType (AccessType(..))

import SlamData.Workspace.Card.CardId (CardId(..))
import SlamData.Workspace.Card.CardId as CID
import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.Model as Card
import SlamData.Workspace.Card.Port.VarMap as Port

import SlamData.Workspace.Deck.Component.Query (Query)
import SlamData.Workspace.Deck.DeckId (DeckId, deckIdToString)
import SlamData.Workspace.Deck.DeckLevel as DL
import SlamData.Workspace.Deck.Model as Model
import SlamData.Workspace.StateMode (StateMode(..))

import Utils.Path (DirPath)

type StateP = OpaqueState State

data DisplayMode
  = Normal
  | Backside
  | Dialog

derive instance eqDisplayMode ∷ Eq DisplayMode

-- | The deck state. See the corresponding lenses for descriptions of
-- | the fields.
type State =
  { id ∷ Maybe DeckId
  , parent ∷ Maybe (Tuple DeckId CardId)
  , fresh ∷ Int
  , accessType ∷ AccessType
  , modelCards ∷ Array Card.Model
  , displayCards ∷ Array Card.Model
  , cardsToLoad ∷ Set.Set CardId
  , activeCardIndex ∷ Maybe Int
  , name ∷ Maybe String
  , path ∷ Maybe DirPath
  , saveTrigger ∷ Maybe (DebounceTrigger Query Slam)
  , runTrigger ∷ Maybe (DebounceTrigger Query Slam)
  , pendingCard ∷ Maybe CardId
  , globalVarMap ∷ Port.VarMap
  , stateMode ∷ StateMode
  , displayMode ∷ DisplayMode
  , initialSliderX ∷ Maybe Number
  , initialSliderCardWidth ∷ Maybe Number
  , sliderTransition ∷ Boolean
  , sliderTranslateX ∷ Number
  , cardElementWidth ∷ Maybe Number
  , level ∷ DL.DeckLevel
  }

-- | A record used to represent card definitions in the deck.
type CardDef = { id ∷ CardId, ty ∷ CT.CardType }

-- | Constructs a default `State` value.
initialDeck ∷ State
initialDeck =
  { id: Nothing
  , parent: Nothing
  , fresh: 0
  , accessType: Editable
  , modelCards: mempty
  , displayCards: mempty
  , cardsToLoad: mempty
  , activeCardIndex: Nothing
  , name: Nothing
  , path: Nothing
  , saveTrigger: Nothing
  , globalVarMap: SM.empty
  , runTrigger: Nothing
  , pendingCard: Nothing
  , stateMode: Loading
  , displayMode: Normal
  , initialSliderX: Nothing
  , initialSliderCardWidth: Nothing
  , sliderTransition: false
  , sliderTranslateX: 0.0
  , cardElementWidth: Nothing
  , level: DL.root
  }

-- | The unique identifier of the deck. If it's a fresh, unsaved deck, the id
-- | will be Nothing.
_id ∷ ∀ a r. LensP {id ∷ a|r} a
_id = lens _.id _{id = _}

-- | A pointer to the parent deck/card. If `Nothing`, the deck is assumed to be
-- | the root deck.
_parent ∷ ∀ a r. LensP {parent ∷ a|r} a
_parent = lens _.parent _{parent = _}

-- | A counter used to generate `CardId` values. This should be a monotonically increasing value
_fresh ∷ ∀ a r. LensP {fresh ∷ a|r} a
_fresh = lens _.fresh _{fresh = _}

-- | Determines whether the deck is editable.
_accessType ∷ ∀ a r. LensP {accessType ∷ a|r} a
_accessType = lens _.accessType _{accessType = _}

-- | The list of cards currently in the deck.
_modelCards ∷ ∀ a r. LensP {modelCards ∷ a |r} a
_modelCards = lens _.modelCards _{modelCards = _}

-- | The list of cards to be displayed in the deck
_displayCards ∷ ∀ a r. LensP {displayCards ∷ a |r} a
_displayCards = lens _.displayCards _{displayCards = _}

_cardsToLoad ∷ ∀ a r. LensP {cardsToLoad ∷ a |r} a
_cardsToLoad = lens _.cardsToLoad _{cardsToLoad = _}

-- | The `CardId` for the currently focused card. `Nothing` indicates the next
-- | action card.
_activeCardIndex ∷ ∀ a r. LensP {activeCardIndex ∷ a |r} a
_activeCardIndex = lens _.activeCardIndex _{activeCardIndex = _}

-- | The display name of the deck.
_name ∷ ∀ a r. LensP {name ∷ a |r} a
_name = lens _.name _{name = _}

-- | The path to the deck in the filesystem
_path ∷ ∀ a r. LensP {path ∷ a |r} a
_path = lens _.path _{path = _}

-- | The debounced trigger for deck save actions.
_saveTrigger ∷ ∀ a r. LensP {saveTrigger ∷ a|r} a
_saveTrigger = lens _.saveTrigger _{saveTrigger = _}

-- | The debounced trigger for running all cards that are pending.
_runTrigger ∷ ∀ a r. LensP {runTrigger ∷ a|r} a
_runTrigger = lens _.runTrigger _{runTrigger = _}

-- | The global `VarMap`, passed through to the deck via the URL.
_globalVarMap ∷ ∀ a r. LensP {globalVarMap ∷ a |r} a
_globalVarMap = lens _.globalVarMap _{globalVarMap = _}

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

-- | Whether the deck is at the top-level of the deck component hierarchy
_level ∷ ∀ a r. LensP {level ∷ a|r} a
_level = lens _.level _{level = _}

addCard ∷ Card.AnyCardModel → State → State
addCard card st = fst $ addCard' card st

addCard' ∷ Card.AnyCardModel → State → State × CardId
addCard' card st =
  let
    cardId = CardId st.fresh
    newState = st
      { fresh = st.fresh + one
      , modelCards = A.snoc st.modelCards { cardId, model: card}
      }
  in newState × cardId

removeCard ∷ CardId → State → Tuple (Array Card.Model) State
removeCard cardId st =
  Tuple oldModelCards
    $ removePendingCard cardId
    $ st
      { modelCards = newModelCards
      , displayCards = newDisplayCards
      , activeCardIndex = Just $ max zero $ A.length newDisplayCards - 1
      }
  where
  newDisplayCards ∷ Array Card.Model
  newDisplayCards = A.filter (\c → c.cardId < cardId) st.displayCards

  newModelCards ∷ Array Card.Model
  newModelCards = A.filter (\c → c.cardId < cardId) st.modelCards

  oldModelCards ∷ Array Card.Model
  oldModelCards = A.filter (\c → c.cardId >= cardId) st.modelCards

findLastRealCardIndex ∷ State → Maybe Int
findLastRealCardIndex =
  A.findLastIndex (Lens.has CID._CardId ∘ _.cardId)
    ∘ _.displayCards

findLastRealCard ∷ State → Maybe CardId
findLastRealCard state =
  findLastRealCardIndex state
    >>= A.index state.displayCards
    <#> _.cardId

-- | Finds the type of the last card.
findLastCardType ∷ State → Maybe CT.CardType
findLastCardType { displayCards } = Card.modelCardType ∘ _.model <$> A.last displayCards

-- TODO: should this be displayCards? - js
cardsOfType ∷ CT.CardType → State → Array CardId
cardsOfType cardType =
  _.modelCards ⋙ A.mapMaybe cardTypeMatches ⋙ foldMap pure
  where
  cardTypeMatches { cardId: cid, model } =
    if Card.modelCardType model ≡ cardType
       then Just cid
       else Nothing

-- | Updates the stored card that is pending to run. This handles the logic of
-- | changing the pending card when a provided card appears earlier in the deck
-- | than the currently enqueued card, and if the provided card appears after
-- | the currently enqueued card the function is a no-op.
addPendingCard ∷ CardId → State → State
addPendingCard cardId st@{ pendingCard } =
  case pendingCard of
    Nothing → st { pendingCard = Just cardId }
    Just oldCardId | cardId < oldCardId → st { pendingCard = Just cardId }
    _ -> st

removePendingCard ∷ CardId → State → State
removePendingCard cardId st@{ pendingCard } =
  case pendingCard of
    Just oldCardId | cardId <= oldCardId → st { pendingCard = Nothing }
    _ → st

-- | Finds the current deck path
deckPath ∷ State → Maybe DirPath
deckPath state = deckPath' <$> state.path <*> state.id

deckPath' ∷ DirPath → DeckId → DirPath
deckPath' path deckId = path </> P.dir (deckIdToString deckId)

-- | Reconstructs a deck state from a deck model.
fromModel
  ∷ Maybe DirPath
  → Maybe DeckId
  → Model.Deck
  → State
  → State
fromModel path deckId { cards, name, parent } state =
  state
    { activeCardIndex = Nothing
    , displayMode = Normal
    , modelCards = cards
    , displayCards = mempty
    , fresh = fresh
    , globalVarMap = SM.empty
    , id = deckId
    , parent = parent
    , initialSliderX = Nothing
    , name = name
    , path = path
    , runTrigger = Nothing
    , pendingCard = Nothing
    }

  where
    fresh ∷ Int
    fresh = maybe 0 (_ + 1) $ maximum $ A.mapMaybe (Lens.preview CID._CardId ∘ _.cardId) cards

cardIndexFromId ∷ CardId → State → Maybe Int
cardIndexFromId cid = A.findIndex (\c → c.cardId ≡ cid) ∘ _.displayCards

cardFromIndex ∷ Int → State → Maybe Card.Model
cardFromIndex i st = A.index st.displayCards i

cardIdFromIndex ∷ Int → State → Maybe CardId
cardIdFromIndex vi = map _.cardId ∘ cardFromIndex vi

activeCardId ∷ State → Maybe CardId
activeCardId st = cardIdFromIndex (fromMaybe 0 st.activeCardIndex) st

activeCardType ∷ State → Maybe CT.CardType
activeCardType st =
  Card.modelCardType ∘ _.model <$>
    cardFromIndex (fromMaybe 0 st.activeCardIndex) st
