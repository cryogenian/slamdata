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
  , StateMode(..)
  , CardDef
  , CardConstructor
  , initialDeck
  , _id
  , _fresh
  , _accessType
  , _cards
  , _dependencies
  , _activeCardId
  , _name
  , _browserFeatures
  , _viewingCard
  , _path
  , _saveTrigger
  , _runTrigger
  , _globalVarMap
  , _pendingCards
  , _failingCards
  , _stateMode
  , _backsided
  , _initialSliderX
  , _initialSliderCardWidth
  , _sliderTransition
  , _sliderTranslateX
  , _nextActionCardElement
  , addCard
  , addCard'
  , removeCards
  , findRoot
  , findParent
  , findChildren
  , findDescendants
  , findLast
  , findLastCardType
  , addPendingCard
  , getCardType
  , cardsOfType
  , fromModel
  , deckPath
  , cardIndexFromId
  , activeCardIndex

  , VirtualState()
  , runVirtualState
  , virtualState
  ) where

import SlamData.Prelude

import Data.BrowserFeatures (BrowserFeatures)
import Data.Foldable (maximum, any)
import Data.Lens (LensP, lens)
import Data.List (List)
import Data.List as L
import Data.Map as M
import Data.Path.Pathy ((</>))
import Data.Path.Pathy as P
import Data.Set as S
import Data.StrMap as SM

import DOM.HTML.Types (HTMLElement)

import Halogen as H
import Halogen.Component.Utils.Debounced (DebounceTrigger)

import SlamData.Effects (Slam)
import SlamData.Workspace.AccessType (AccessType(..))
import SlamData.Workspace.Card.Ace.Component (AceEvaluator, AceSetup, aceComponent)
import SlamData.Workspace.Card.API.Component (apiComponent)
import SlamData.Workspace.Card.APIResults.Component (apiResultsComponent)
import SlamData.Workspace.Card.CardId (CardId(..), runCardId)
import SlamData.Workspace.Card.CardType (CardType(..), AceMode(..))
import SlamData.Workspace.Card.Chart.Component (chartComponent)
import SlamData.Workspace.Card.Component (CardComponent, CardStateP, CardQueryP, initialCardState)
import SlamData.Workspace.Card.Download.Component (downloadComponent)
import SlamData.Workspace.Card.JTable.Component (jtableComponent)
import SlamData.Workspace.Card.Markdown.Component (markdownComponent)
import SlamData.Workspace.Card.Next.Component (nextCardComponent)
import SlamData.Workspace.Card.Save.Component (saveCardComponent)
import SlamData.Workspace.Card.DownloadOptions.Component as DOpts
import SlamData.Workspace.Card.Error.Component as Error
import SlamData.Workspace.Card.Markdown.Eval (markdownEval, markdownSetup)
import SlamData.Workspace.Card.Model as Card
import SlamData.Workspace.Card.Port.VarMap as Port
import SlamData.Workspace.Card.Query.Eval (queryEval, querySetup)
import SlamData.Workspace.Card.Search.Component (searchComponent)
import SlamData.Workspace.Card.Viz.Component (vizComponent)
import SlamData.Workspace.Card.OpenResource.Component (openResourceComponent)
import SlamData.Workspace.Deck.Component.ChildSlot (CardSlot(..), ChildSlot, ChildState, ChildQuery)
import SlamData.Workspace.Deck.Component.Query (Query)
import SlamData.Workspace.Deck.DeckId (DeckId, deckIdToString)
import SlamData.Workspace.Deck.Model as Model

import Utils.Path (DirPath)

type StateP = H.ParentState State ChildState Query ChildQuery Slam ChildSlot

data StateMode
  = Loading
  | Ready
  | Error String

-- | The deck state. See the corresponding lenses for descriptions of
-- | the fields.
type State =
  { id ∷ Maybe DeckId
  , fresh ∷ Int
  , accessType ∷ AccessType
  , cards ∷ List CardDef
  , dependencies ∷ M.Map CardId CardId
  , cardTypes ∷ M.Map CardId CardType
  , activeCardId ∷ Maybe CardId
  , name ∷ Maybe String
  , path ∷ Maybe DirPath
  , browserFeatures ∷ BrowserFeatures
  , viewingCard ∷ Maybe CardId
  , saveTrigger ∷ Maybe (DebounceTrigger Query Slam)
  , runTrigger ∷ Maybe (DebounceTrigger Query Slam)
  , pendingCards ∷ S.Set CardId
  , failingCards ∷ S.Set CardId
  , globalVarMap ∷ Port.VarMap
  , stateMode ∷ StateMode
  , backsided ∷ Boolean
  , initialSliderX :: Maybe Number
  , initialSliderCardWidth :: Maybe Number
  , sliderTransition :: Boolean
  , sliderTranslateX :: Number
  , nextActionCardElement :: Maybe HTMLElement
  }

-- | A record used to represent card definitions in the deck.
type CardDef = { id ∷ CardId, ty ∷ CardType, ctor ∷ CardConstructor }

-- | The specific `SlotConstructor` type for cards in the deck.
type CardConstructor = H.SlotConstructor CardStateP CardQueryP Slam CardSlot

-- | Constructs a default `State` value.
initialDeck ∷ BrowserFeatures → State
initialDeck browserFeatures =
  { id: Nothing
  , fresh: 0
  , accessType: Editable
  , cards: mempty
  , cardTypes: M.empty
  , dependencies: M.empty
  , activeCardId: Nothing
  , name: Nothing
  , browserFeatures
  , viewingCard: Nothing
  , path: Nothing
  , saveTrigger: Nothing
  , globalVarMap: SM.empty
  , runTrigger: Nothing
  , pendingCards: S.empty
  , failingCards: S.empty
  , stateMode: Ready
  , backsided: false
  , initialSliderX: Nothing
  , initialSliderCardWidth: Nothing
  , sliderTransition: false
  , sliderTranslateX: 0.0
  , nextActionCardElement: Nothing
  }

-- | The unique identifier of the deck. If it's a fresh, unsaved deck, the id
-- | will be Nothing.
_id ∷ LensP State (Maybe DeckId)
_id = lens _.id _{id = _}

-- | A counter used to generate `CardId` values.
_fresh ∷ LensP State Int
_fresh = lens _.fresh _{fresh = _}

-- | Determines whether the deck is editable.
_accessType ∷ LensP State AccessType
_accessType = lens _.accessType _{accessType = _}

-- | The list of cards currently in the deck.
_cards ∷ LensP State (List CardDef)
_cards = lens _.cards _{cards = _}

-- | A map of the edges in the dependency tree, where each key/value pair
-- | represents a child/parent relation.
_dependencies ∷ LensP State (M.Map CardId CardId)
_dependencies = lens _.dependencies _{dependencies = _}

-- | The `CardId` for the currently focused card. `Nothing` indicates the next
-- | action card.
_activeCardId ∷ LensP State (Maybe CardId)
_activeCardId = lens _.activeCardId _{activeCardId = _}

-- | The display name of the deck.
_name ∷ LensP State (Maybe String)
_name = lens _.name _{name = _}

-- | The path to the deck in the filesystem
_path ∷ LensP State (Maybe DirPath)
_path = lens _.path _{path = _}

-- | The available browser features - passed through to markdown results cards
-- | as they need this information to render the output HTML.
_browserFeatures ∷ LensP State BrowserFeatures
_browserFeatures = lens _.browserFeatures _{browserFeatures = _}

-- | The currently focused card when viewing an individual card within a
-- | deck.
_viewingCard ∷ LensP State (Maybe CardId)
_viewingCard = lens _.viewingCard _{viewingCard = _}

-- | The debounced trigger for deck save actions.
_saveTrigger ∷ LensP State (Maybe (DebounceTrigger Query Slam))
_saveTrigger = lens _.saveTrigger _{saveTrigger = _}

-- | The debounced trigger for running all cards that are pending.
_runTrigger ∷ LensP State (Maybe (DebounceTrigger Query Slam))
_runTrigger = lens _.runTrigger _{runTrigger = _}

-- | The global `VarMap`, passed through to the deck via the URL.
_globalVarMap ∷ LensP State Port.VarMap
_globalVarMap = lens _.globalVarMap _{globalVarMap = _}

-- | The cards that have been enqueued to run.
_pendingCards ∷ LensP State (S.Set CardId)
_pendingCards = lens _.pendingCards _{pendingCards = _}

-- | The cards which currently have errors.
_failingCards ∷ LensP State (S.Set CardId)
_failingCards = lens _.failingCards _{failingCards = _}

-- | The "state mode" used to track whether the deck is ready, loading, or
-- | if an error has occurred while loading.
_stateMode ∷ LensP State StateMode
_stateMode = lens _.stateMode _{stateMode = _}

-- | Is `true` if backside of deck is displayed
_backsided ∷ ∀ a r. LensP {backsided ∷ a |r} a
_backsided = lens _.backsided _{backsided = _}

-- | The x position of the card slider at the start of the slide interaction in
-- | pixels. If `Nothing` slide interaction is not in progress.
_initialSliderX :: LensP State (Maybe Number)
_initialSliderX = lens _.initialSliderX _{initialSliderX = _}

-- | The width of the next action card at the start of the slide interaction in
-- | pixels. If `Nothing` either the slide interaction is not in progress or the
-- | next action card element reference is broken.
_initialSliderCardWidth :: LensP State (Maybe Number)
_initialSliderCardWidth = lens _.initialSliderCardWidth _{initialSliderCardWidth = _}

-- | Whether the translation of the card slider should be animated or not.
-- | Should be true between the end of the slide interaction and the end of the
-- | transition.
_sliderTransition :: LensP State Boolean
_sliderTransition = lens _.sliderTransition _{sliderTransition = _}

-- | The current x translation of the card slider during the slide interaction.
_sliderTranslateX :: LensP State Number
_sliderTranslateX = lens _.sliderTranslateX _{sliderTranslateX = _}

-- | The next action card HTML element
_nextActionCardElement :: LensP State (Maybe HTMLElement)
_nextActionCardElement = lens _.nextActionCardElement _{nextActionCardElement = _}

-- | Adds a new card to the deck.
-- |
-- | Takes the current deck state, the type of card to add, and an optional
-- | parent card ID.
addCard ∷ CardType → Maybe CardId → State → State
addCard cardType parent st = fst $ addCard' cardType parent st

-- | Adds a new card to the deck.
-- |
-- | Takes the current deck state, the type of card to add, and an optional
-- | parent card ID and returns the modified deck state and the new card ID.
addCard' ∷ CardType → Maybe CardId → State → Tuple State CardId
addCard' cardType parent st =
  let
    cardId = CardId st.fresh
    newState = st
      { fresh = st.fresh + 1
      , cards = st.cards `L.snoc` mkCardDef cardType cardId st
      , activeCardId = Just cardId
      , cardTypes = M.insert cardId cardType st.cardTypes
      , dependencies =
          maybe st.dependencies (flip (M.insert cardId) st.dependencies) parent
      }
  in
    Tuple newState cardId

-- | Insert an error card as a child to a specified card, and reassociate all
-- | its children as children of the error card.
insertErrorCard ∷ CardId → State → State
insertErrorCard parentId st =
  st
    { cards =
        fromMaybe st.cards do
          parentAddr ← L.findIndex (\c → c.id ≡ parentId) st.cards
          let errorCard = mkCardDef ErrorCard cardId st
          L.insertAt (parentAddr + 1) errorCard st.cards
    , cardTypes = M.insert cardId ErrorCard st.cardTypes
    , dependencies =
        let
          children = S.toList $ findChildren parentId st
          updates = children <#> \childId → M.insert childId cardId
        in foldr (∘) id updates $ M.insert cardId parentId st.dependencies
    }
  where
    -- The -1 index is reserved for the error card.
    cardId = CardId (-1)

mkCardDef ∷ CardType → CardId → State → CardDef
mkCardDef cardType cardId st =
  { id: cardId
  , ty: cardType
  , ctor: H.SlotConstructor (CardSlot cardId) \_ → { component, initialState }
  }
  where
  component = cardTypeComponent cardType cardId st.browserFeatures
  initialState =
    H.parentState initialCardState
      { accessType = st.accessType }

cardTypeComponent ∷ CardType → CardId → BrowserFeatures → CardComponent
cardTypeComponent (Ace mode) _ _ = aceComponent { mode, evaluator, setup }
  where
  evaluator = aceEvalMode mode
  setup = aceSetupMode mode
cardTypeComponent Search _ _ = searchComponent
cardTypeComponent Viz _ _ = vizComponent
cardTypeComponent Chart _ _ = chartComponent
cardTypeComponent Markdown cardId bf = markdownComponent cardId bf
cardTypeComponent JTable _ _ = jtableComponent
cardTypeComponent Download _ _ = downloadComponent
cardTypeComponent API _ _ = apiComponent
cardTypeComponent APIResults _ _ = apiResultsComponent
cardTypeComponent NextAction _ _ = nextCardComponent
cardTypeComponent Save _ _ = saveCardComponent
cardTypeComponent OpenResource _ _ = openResourceComponent
cardTypeComponent DownloadOptions _ _ = DOpts.comp
cardTypeComponent ErrorCard _ _ = Error.comp

aceEvalMode ∷ AceMode → AceEvaluator
aceEvalMode MarkdownMode = markdownEval
aceEvalMode SQLMode = queryEval

aceSetupMode ∷ AceMode → AceSetup
aceSetupMode MarkdownMode = markdownSetup
aceSetupMode SQLMode = querySetup

-- | Removes a set of cards from the deck. Any cards that depend on a card
-- | in the set of provided cards will also be removed.
-- |
-- | Takes the set of IDs for the cards to remove and the current deck
-- | state.
removeCards ∷ S.Set CardId → State → State
removeCards cardIds st = st
    { cards = L.filter f st.cards
    , cardTypes = foldl (flip M.delete) st.cardTypes cardIds'
    , dependencies = M.fromList $ L.filter g $ M.toList st.dependencies
    , pendingCards = S.difference st.pendingCards cardIds
    }
  where
  cardIds' ∷ S.Set CardId
  cardIds' = cardIds ⊕ foldMap (flip findDescendants st) cardIds

  f ∷ CardDef → Boolean
  f = not ∘ flip S.member cardIds' ∘ _.id

  g ∷ Tuple CardId CardId → Boolean
  g (Tuple kId vId) = not $ S.member kId cardIds' ∨ S.member vId cardIds'


-- | Finds the last card/card
findLast ∷ State → Maybe CardId
findLast state =
  maximum $ M.keys state.cardTypes

findLastCardType ∷ State → Maybe CardType
findLastCardType state =
  join $ flip M.lookup state.cardTypes <$> findLast state

-- | Finds the root in a chain of dependencies starting at the specified card.
-- | A card can be its own root if it depends on no other cards.
-- |
-- | Takes the ID of the card to start searching from and the current deck
-- | state.
findRoot ∷ CardId → State → CardId
findRoot cardId st = case findParent cardId st of
  Nothing → cardId
  Just parentId → findRoot parentId st

-- | Finds the parent of a card. If the card is a root it has no parent, and
-- | the result will be `Nothing`.
-- |
-- | Takes the ID of the card to find the parent of and the current deck
-- | state.
findParent ∷ CardId → State → Maybe CardId
findParent cardId st = M.lookup cardId st.dependencies

-- | Finds the immediate dependencies of a card.
-- |
-- | Takes the ID of the card to find the children of and the current deck
-- | state.
findChildren ∷ CardId → State → S.Set CardId
findChildren parentId st =
  S.fromList $ map fst $ L.filter f $ M.toList st.dependencies
  where
  f ∷ Tuple CardId CardId → Boolean
  f (Tuple _ cardId) = cardId ≡ parentId

-- | Finds all the dependencies of a card: the children, children's children,
-- | and so on until the leaves of the tree are reached.
-- |
-- | Takes the ID of the card to find the descendants of and the current
-- | deck state.
findDescendants ∷ CardId → State → S.Set CardId
findDescendants cardId st =
  let children = findChildren cardId st
  in children ⊕ foldMap (flip findDescendants st) children

-- | Determine's the `CardType` of a card; returns `Just` if the card is
-- | in the deck, and `Nothing` if it is not.
getCardType ∷ CardId → State → Maybe CardType
getCardType cardId st = M.lookup cardId st.cardTypes

cardsOfType ∷ CardType → State → List CardId
cardsOfType cardType =
  _.cardTypes ⋙ M.toList ⋙ L.mapMaybe \(Tuple cid ty) →
    if ty ≡ cardType
       then Just cid
       else Nothing

newtype VirtualState = VirtualState State

runVirtualState ∷ VirtualState → State
runVirtualState (VirtualState st) = st

-- | Equip the state for presentation by inserting Error cards
-- | in the appropriate places.
virtualState ∷ State → VirtualState
virtualState st =
  VirtualState
    case findFirst hasError st.cards of
      Just c → insertErrorCard c.id st
      Nothing → st
  where

  -- in case you're wondering, Data.Foldable.find does not find the *first*
  -- satisfying element in the list! This took me a long time to figure out.
  findFirst ∷ ∀ a. (a → Boolean) → List a → Maybe a
  findFirst p xs =
    L.findIndex p xs
      >>= L.index xs

  hasError ∷ CardDef → Boolean
  hasError c = S.member c.id st.failingCards

-- | Adds a card to the set of cards that are enqueued to run.
-- |
-- | If the card is a descendant of an card that has already been enqueued this
-- | will have no effect, as in this case the card is already pending by
-- | implication: all cards under the queued ancestor will be evaluated as
-- | changes propagate through the subgraph.
-- |
-- | If the card is an ancestor of cards that have already been enqueued they
-- | will be removed when this card is added, for the same reasoning as above.
addPendingCard ∷ CardId → State → State
addPendingCard cardId st@{ pendingCards } =
  if cardId `S.member` pendingCards ∨ any isAncestor pendingCards
  then st
  else st { pendingCards = S.insert cardId (removeDescendants pendingCards) }
  where
  isAncestor ∷ CardId → Boolean
  isAncestor otherId = cardId `S.member` findDescendants otherId st
  removeDescendants ∷ S.Set CardId → S.Set CardId
  removeDescendants = flip S.difference (findDescendants cardId st)

-- | Finds the current deck path
deckPath ∷ State → Maybe DirPath
deckPath state = do
  path ← state.path
  deckId ← deckIdToString <$> state.id
  pure $ path </> P.dir deckId

-- | Reconstructs a deck state from a deck model.
fromModel
  ∷ BrowserFeatures
  → Maybe DirPath
  → Maybe DeckId
  → Model.Deck
  → State
  → Tuple (Array Card.Model) State
fromModel browserFeatures path deckId { cards, dependencies, name } state =
  Tuple
    cards
    ((state
        { accessType = ReadOnly
        , activeCardId = _.id <$> L.last cardDefs
        , backsided = false
        , browserFeatures = browserFeatures
        , cardTypes = foldl addCardIdTypePair M.empty cards
        , cards = cardDefs
        , dependencies = dependencies
        , failingCards = S.empty
        , fresh = maybe 0 (_ + 1) $ maximum $ map (runCardId ∘ _.cardId) cards
        , globalVarMap = SM.empty
        , id = deckId
        , initialSliderX = Nothing
        , name = name
        , path = path
        , runTrigger = Nothing
        , pendingCards = S.empty
        }) :: State)
  where
  cardDefs = foldMap cardDefFromModel cards

  activeCardId = _.id <$> L.last cardDefs

  addCardIdTypePair mp {cardId, cardType} = M.insert cardId cardType mp

  cardDefFromModel ∷ Card.Model → List CardDef
  cardDefFromModel { cardId, cardType} =
    let component = cardTypeComponent cardType cardId browserFeatures
        initialState = H.parentState initialCardState
    in
      pure
        { id: cardId
        , ty: cardType
        , ctor: H.SlotConstructor (CardSlot cardId) \_ → { component, initialState }
        }

activeCardIndex :: VirtualState -> Int
activeCardIndex vstate =
  fromMaybe (L.length state.cards) (L.findIndex isActiveCard state.cards)
  where
  state = runVirtualState vstate
  isActiveCard = eq state.activeCardId <<< Just <<< _.id

cardIndexFromId :: VirtualState -> Maybe CardId -> Maybe Int
cardIndexFromId vstate =
  maybe (Just $ L.length state.cards) (flip L.elemIndex (_.id <$> state.cards))
  where
  state = runVirtualState vstate

