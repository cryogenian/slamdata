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

module SlamData.Workspace.Deck.Component
  ( comp
  , initialState
  , module SlamData.Workspace.Deck.Component.Query
  , module DCS
  ) where

import SlamData.Prelude

import Control.Monad.Aff.Console (log)
import Control.Monad.Eff.Exception as Exn
import Control.Monad.Except.Trans (ExceptT(..), runExceptT)
import Control.UI.Browser (newTab, locationObject)

import Data.Argonaut (Json)
import Data.Array (catMaybes, nub)
import Data.BrowserFeatures (BrowserFeatures)
import Data.Lens as Lens
import Data.Lens ((.~), (%~), (^?))
import Data.Lens.Prism.Coproduct (_Right)
import Data.List as List
import Data.Map as Map
import Data.Path.Pathy ((</>))
import Data.Path.Pathy as Pathy
import Data.Set as S
import Data.Time (Milliseconds(..))

import Ace.Halogen.Component as Ace

import DOM.HTML.Location as Location

import Halogen as H
import Halogen.Component.Utils (forceRerender')
import Halogen.Component.Utils.Debounced (fireDebouncedQuery')
import Halogen.HTML.CSS.Indexed (style)
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP
import Halogen.Themes.Bootstrap3 as B
import Halogen.HTML.Properties.Indexed.ARIA as ARIA

import SlamData.Effects (Slam)
import SlamData.FileSystem.Resource as R
import SlamData.Workspace.AccessType (AccessType(..), isEditable)
import SlamData.Workspace.Action as NA
import SlamData.Workspace.Card.CardId (CardId())
import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.Common.EvalQuery as CEQ
import SlamData.Workspace.Card.Component (CardQueryP, CardQuery(..), InnerCardQuery, AnyCardQuery(..), _NextQuery)
import SlamData.Workspace.Card.Next.Component as Next
import SlamData.Workspace.Card.OpenResource.Component as Open
import SlamData.Workspace.Card.Port (Port(..))
import SlamData.Workspace.Deck.BackSide.Component as Back
import SlamData.Workspace.Deck.Common (DeckHTML, DeckDSL)
import SlamData.Workspace.Deck.Component.ChildSlot (cpBackSide, cpCard, ChildQuery, ChildSlot, CardSlot(..))
import SlamData.Workspace.Deck.Component.Query (QueryP, Query(..))

import SlamData.Workspace.Deck.Component.State as DCS
import SlamData.Workspace.Deck.DeckId (DeckId(..), deckIdToString)
import SlamData.Workspace.Deck.Gripper as Gripper
import SlamData.Workspace.Deck.Model as Model
import SlamData.Workspace.Model as NB
import SlamData.Workspace.Deck.Slider as Slider
import SlamData.Workspace.Routing (mkWorkspaceHash, mkWorkspaceCardHash, mkWorkspaceURL)
import SlamData.Quasar.Data (save, load) as Quasar
import SlamData.Render.CSS as CSS

import Utils.Path (DirPath, FilePath)

initialState ∷ BrowserFeatures → DCS.StateP
initialState fs = H.parentState $ DCS.initialDeck fs

comp ∷ H.Component DCS.StateP QueryP Slam
comp =
  H.parentComponent
    { render: render ∘ DCS.virtualState
    , eval
    , peek: Just peek
    }

render ∷ DCS.State → DeckHTML
render state =
  case state.stateMode of
    DCS.Loading →
      HH.div
        [ HP.classes [ B.alert, B.alertInfo ]
        , HP.key "board"
        ]
        [ HH.h1
          [ HP.class_ B.textCenter ]
          [ HH.text "Loading..." ]
          -- We need to render the cards but have them invisible during loading
          -- otherwise the various nested components won't initialise correctly.
          -- This div is required, along with the key, so that structurally it
          -- is in the same place in both `Loading` and `Ready` states.
        , HH.div
            [ HP.key "deck-container" ]
            [ Slider.render state $ not state.backsided ]
        ]
    DCS.Ready →
      -- WARNING: Very strange things happen when this is not in a div; see SD-1326.
      HH.div
        ([ HP.class_ CSS.board
         , HP.key "board"
         ] ++ Slider.containerProperties state)
        [ HH.div
            [ HP.class_ CSS.deck
            , HP.key "deck-container"
            ]
            [ HH.button
                [ HP.classes [ CSS.flipDeck ]
                , HE.onClick (HE.input_ FlipDeck)
                , ARIA.label "Flip deck"
                , HP.title "Flip deck"
                ]
                [ HH.text "" ]
            , Slider.render state $ not state.backsided
            , renderBackside state.backsided
            ]
        ]

    DCS.Error err →
      HH.div
        [ HP.classes [ B.alert, B.alertDanger ] ]
        [ HH.h1
            [ HP.class_ B.textCenter ]
            [ HH.text err ]
        ]

  where
  renderBackside visible =
    HH.div
      ([ HP.classes [ CSS.cardSlider ]
       , ARIA.hidden $ show $ not visible
       ]
         ⊕ ((guard $ not visible) $> (HP.class_ CSS.invisible)))
      [ HH.div
          [ HP.classes [ CSS.card ]
          , style $ Slider.cardWidthCSS 1
          ]
          (Gripper.renderGrippers
             visible
             (isJust state.initialSliderX)
             (Gripper.gripperDefsForCardId state.cards state.activeCardId)
             ⊕ [ HH.slot' cpBackSide unit \_ →
                  { component: Back.comp
                  , initialState: Back.initialState
                  }
               ]
          )
      ]

eval ∷ Natural Query DeckDSL
eval (AddCard cardType next) = createCard cardType $> next
eval (RunActiveCard next) = do
  (maybe (pure unit) runCard =<< H.gets (_.activeCardId)) $> next
eval (Load fs dir deckId next) = do
  state ← H.get
  H.modify (DCS._stateMode .~ DCS.Loading)
  json ← Quasar.load $ deckIndex dir deckId
  case Model.decode =<< json of
    Left err → do
      H.fromAff $ log err
      H.modify $ DCS._stateMode .~ DCS.Error "There was a problem decoding the saved deck"
    Right model →
      case DCS.fromModel fs (Just dir) (Just deckId) model state of
        Tuple cards st → do
          setDeckState st
          forceRerender'
          ranCards ← catMaybes <$> for cards \card → do
            H.query' cpCard  (CardSlot card.cardId)
              $ left
              $ H.action
              $ LoadCard card
            pure if card.hasRun then Just card.cardId else Nothing
          -- We only need to run the root node in each subgraph, as doing so
          -- will result in all child nodes being run also as the outputs
          -- propagate down each subgraph.
          traverse_ runCard $ nub $ flip DCS.findRoot st <$> ranCards
          H.modify $ DCS._stateMode .~ DCS.Ready
  updateNextActionCard
  pure next

eval (ExploreFile fs res next) = do
  setDeckState $ DCS.initialDeck fs
  H.modify
    $ (DCS._path .~ Pathy.parentDir res)
    ∘ (DCS.addCard CT.OpenResource Nothing)
  forceRerender'
  H.query' cpCard (CardSlot zero)
    $ right
    $ H.ChildF unit
    $ right
    $ OpenResourceQuery
    $ right
    $ H.action
    $ Open.ResourceSelected
    $ R.File res
  forceRerender'
  runCard zero
  -- Flush the eval queue
  saveDeck
  updateNextActionCard
  pure next
eval (Publish next) = do
  H.gets DCS.deckPath >>= \mpath →
    for_ mpath $ H.fromEff ∘ newTab ∘ flip mkWorkspaceURL (NA.Load ReadOnly)
  pure next
eval (Reset fs dir deckId next) = do
  let deck = DCS.initialDeck fs
  setDeckState $ deck { id = deckId, path = Just dir }
  pure next
eval (SetName name next) =
  H.modify (DCS._name .~ Just name) $> next
eval (SetAccessType aType next) = do
  cids ← map Map.keys $ H.gets _.cardTypes
  for_ cids \cardId →
    void
      $ H.query' cpCard (CardSlot cardId)
      $ left
      $ H.action
      $ SetCardAccessType aType
  H.modify $ DCS._accessType .~ aType
  unless (isEditable aType)
    $ H.modify (DCS._backsided .~ false)
  pure next
eval (GetPath k) = k <$> H.gets DCS.deckPath
eval (SetViewingCard mbcid next) = H.modify (DCS._viewingCard .~ mbcid) $> next
eval (Save next) = saveDeck $> next
eval (RunPendingCards next) = do
  -- Only run pending cards if we have a deckPath. Some cards run with the
  -- assumption that the deck is saved to disk.
  H.gets DCS.deckPath >>= traverse_ \_ → runPendingCards
  pure next
eval (GetGlobalVarMap k) = k <$> H.gets _.globalVarMap
eval (SetGlobalVarMap m next) = do
  st ← H.get
  when (m ≠ st.globalVarMap) do
    H.modify $ DCS._globalVarMap .~ m
    traverse_ runCard $ DCS.cardsOfType CT.API st
  pure next
eval (FindCardParent cid k) = k <$> H.gets (DCS.findParent cid)
eval (GetCardType cid k) = k <$> H.gets (DCS.getCardType cid)
eval (FlipDeck next) = H.modify (DCS._backsided %~ not) $> next
eval (GetActiveCardId k) = map k $ H.gets DCS.findLast
eval (StartSliding mouseEvent next) =
  Slider.startSliding mouseEvent $> next
eval (StopSlidingAndSnap mouseEvent next) =
  Slider.stopSlidingAndSnap mouseEvent $> next
eval (UpdateSliderPosition mouseEvent next) =
  Slider.updateSliderPositionAndSetSliderSelectedCardId mouseEvent $> next
eval (SetNextActionCardElement element next) =
  Slider.setLens DCS._nextActionCardElement element $> next
eval (StopSliderTransition next) =
  Slider.setLens DCS._sliderTransition false $> next

peek ∷ ∀ a. H.ChildF ChildSlot ChildQuery a → DeckDSL Unit
peek (H.ChildF s q) =
  coproduct
    (either peekCards (\_ _ → pure unit) s)
    peekBackSide
    q

peekBackSide ∷ ∀ a. Back.Query a → DeckDSL Unit
peekBackSide (Back.UpdateFilter _ _) = pure unit
peekBackSide (Back.DoAction action _) = case action of
  Back.Trash → do
    activeId ← H.gets _.activeCardId
    lastId ← H.gets DCS.findLast
    for_ (activeId <|> lastId) \trashId → do
      descendants ← H.gets $ DCS.findDescendants trashId
      H.modify ∘ DCS.removeCards $ S.insert trashId descendants
      triggerSave
      updateNextActionCard
      H.modify $ DCS._backsided .~ false
  Back.Share → pure unit
  Back.Embed → pure unit
  Back.Publish →
    H.gets DCS.deckPath >>= \mpath →
      for_ mpath $ H.fromEff ∘ newTab ∘ flip mkWorkspaceURL (NA.Load ReadOnly)
  Back.Mirror → pure unit
  Back.Wrap → pure unit

peekCards ∷ ∀ a. CardSlot → CardQueryP a → DeckDSL Unit
peekCards (CardSlot cardId) q =
  coproduct (peekCard cardId) (peekCardInner cardId) q


-- | Peek on the card component to observe actions from the card control
-- | buttons.
peekCard ∷ ∀ a. CardId → CardQuery a → DeckDSL Unit
peekCard cardId q = case q of
  RunCard _ → runCard cardId
  RefreshCard _ → runCard ∘ DCS.findRoot cardId =<< H.get
  TrashCard _ → do
    descendants ← H.gets $ DCS.findDescendants cardId
    H.modify ∘ DCS.removeCards $ S.insert cardId descendants
    triggerSave
    updateNextActionCard
  ToggleCaching _ →
    triggerSave
  ShareCard _ → pure unit
  StopCard _ → do
    H.modify $ DCS._runTrigger .~ Nothing
    H.modify $ DCS._pendingCards %~ S.delete cardId
    runPendingCards
  _ → pure unit


updateNextActionCard ∷ DeckDSL Unit
updateNextActionCard = do
  cid ← H.gets DCS.findLast
  mbMessage ← case cid of
    Just cardId → do
      out ←
        map join
          $ H.query' cpCard (CardSlot cardId)
          $ left (H.request GetOutput)
      pure $ case out of
        Nothing →
          Just "Next actions will be made available once the last card has been run"
        Just Blocked →
          Just "There are no available next actions"
        Just (CardError _) →
          Just "There are no available next actions (parent cards have errors)"
        _ → Nothing
    Nothing → pure Nothing
  queryNextActionCard
    $ H.action
    $ Next.SetMessage mbMessage

  lastCardType ← H.gets DCS.findLastCardType
  queryNextActionCard
    $ H.action
    $ Next.SetAvailableTypes
    $ CT.nextCardTypes lastCardType
  pure unit
  where
  queryNextActionCard q =
    H.query' cpCard (CardSlot top)
      $ right
      $ H.ChildF unit
      $ right
      $ NextQuery
      $ right q


createCard ∷ CT.CardType → DeckDSL Unit
createCard cardType = do
  cid ← H.gets DCS.findLast
  s ← H.get
  case cid of
    Nothing →
      H.modify $ DCS.addCard cardType Nothing
    Just cardId → do
      Tuple st newCardId ← H.gets $ DCS.addCard' cardType (Just cardId)
      setDeckState st
      forceRerender'
      input ←
        map join $ H.query' cpCard (CardSlot cardId) $ left (H.request GetOutput)

      for_ input \input' → do
        path ← H.gets DCS.deckPath
        let setupInfo = { path, inputPort: input', cardId: newCardId }
        void
          $ H.query' cpCard  (CardSlot newCardId)
          $ right
          $ H.ChildF unit
          $ left
          $ H.action (CEQ.SetupCard setupInfo)
      runCard newCardId
  updateNextActionCard
  triggerSave

-- | Peek on the inner card components to observe `NotifyRunCard`, which is
-- | raised by actions within a card that should cause the card to run.
peekCardInner
  ∷ ∀ a. CardId → H.ChildF Unit InnerCardQuery a → DeckDSL Unit
peekCardInner cardId (H.ChildF _ q) =
  coproduct (peekEvalCard cardId) (peekAnyCard cardId) q

peekEvalCard ∷ ∀ a. CardId → CEQ.CardEvalQuery a → DeckDSL Unit
peekEvalCard cardId (CEQ.NotifyRunCard _) = runCard cardId
peekEvalCard _ _ = pure unit

peekAnyCard ∷ ∀ a. CardId → AnyCardQuery a → DeckDSL Unit
peekAnyCard cardId q = do
  for_ (q ^? _NextQuery ∘ _Right ∘ Next._AddCardType) createCard
  when (queryShouldRun q) $ runCard cardId
  when (queryShouldSave q) triggerSave
  pure unit

queryShouldRun ∷ ∀ a. AnyCardQuery a → Boolean
queryShouldRun (SaveQuery q) = false
queryShouldRun _ = true

queryShouldSave  ∷ ∀ a. AnyCardQuery a → Boolean
queryShouldSave (AceQuery q) =
  coproduct evalQueryShouldSave aceQueryShouldSave q
queryShouldSave _ = true

evalQueryShouldSave ∷ ∀ a. CEQ.CardEvalQuery a → Boolean
evalQueryShouldSave _ = true

aceQueryShouldSave
  ∷ ∀ p a. H.ChildF p Ace.AceQuery a → Boolean
aceQueryShouldSave (H.ChildF _ q) =
  case q of
    Ace.TextChanged _ → true
    _ → false


-- | Runs all card that are present in the set of pending cards.
runPendingCards ∷ DeckDSL Unit
runPendingCards = do
  cards ← H.gets _.pendingCards
  traverse_ runCard' cards
  updateNextActionCard
  where
  runCard' ∷ CardId → DeckDSL Unit
  runCard' cardId = do
    mbParentId ← H.gets $ DCS.findParent cardId
    case mbParentId of
      -- if there's no parent there's no input port value to pass through
      Nothing → updateCard Nothing cardId
      Just parentId → do
        value ←
          map join $ H.query' cpCard (CardSlot parentId) $ left (H.request GetOutput)
        case value of
          -- if there's a parent but no output the parent card hasn't been evaluated
          -- yet, so we can't run this card either
          Nothing → pure unit
          -- if there's a parent and an output, pass it on as this card's input
          Just p → updateCard (Just p) cardId
    H.modify $ DCS._pendingCards %~ S.delete cardId
    triggerSave

-- | Enqueues the card with the specified ID in the set of cards that are
-- | pending to run and enqueues a debounced H.query to trigger the cards to
-- | actually run.
runCard ∷ CardId → DeckDSL Unit
runCard cardId = do
  H.modify (DCS.addPendingCard cardId)
  fireDebouncedQuery' (Milliseconds 500.0) DCS._runTrigger RunPendingCards

-- | Updates the evaluated value for a card by running it with the specified
-- | input and then runs any cards that depend on the card's output with the
-- | new result.
updateCard ∷ Maybe Port → CardId → DeckDSL Unit
updateCard inputPort cardId = do
  path ← H.gets DCS.deckPath
  globalVarMap ← H.gets _.globalVarMap
  let input = { path, inputPort, cardId, globalVarMap }
  result ←
    map join
      $ H.query' cpCard (CardSlot cardId)
      $ left
      $ H.request (UpdateCard input)

  H.modify ∘ Lens.over DCS._failingCards $
    case result of
      Just (CardError msg) → S.insert cardId
      _ → S.delete cardId
  forceRerender'

  runCardDescendants cardId (fromMaybe Blocked result)
  where
  runCardDescendants ∷ CardId → Port → DeckDSL Unit
  runCardDescendants parentId value = do
    -- Crucially, we run the card descendents according to the virtual graph;
    -- this enables the correct behavior of virtual cards, including the Error Card.
    children ← H.gets $ DCS.findChildren parentId ∘ DCS.virtualState
    traverse_ (updateCard (Just value)) children

-- | Triggers the H.query for autosave. This does not immediate perform the save
-- | H.action, but instead enqueues a debounced H.query to trigger the actual save.
triggerSave ∷ DeckDSL Unit
triggerSave =
  fireDebouncedQuery' (Milliseconds 500.0) DCS._saveTrigger Save

-- | Saves the deck as JSON, using the current values present in the state.
saveDeck ∷ DeckDSL Unit
saveDeck = H.get >>= \st → do
  if isUnsaved st ∧ isNewExploreDeck st
    -- If its an unsaved Explore deck, it is safe to go ahead and run it.
    then runPendingCards
    else do
      cards ← catMaybes <$> for (List.fromList st.cards) \card →
        H.query' cpCard (CardSlot card.id)
          $ left
          $ H.request (SaveCard card.id card.ty)

      let json = Model.encode { name: st.name, cards, dependencies: st.dependencies }

      for_ st.path \path → do
        deckId ← runExceptT do
          i ← ExceptT $ genId path st.id
          ExceptT $ save path i json
          pure i

        case deckId of
          Left err → do
            -- TODO: do something to notify the user saving failed
            pure unit
          Right deckId' → do
            H.modify $ DCS._id .~ Just deckId'

            -- runPendingCards would be deffered if there had previously been
            -- no `deckPath`. We need to flush the queue.
            when (isNothing $ DCS.deckPath st) runPendingCards

            -- We need to get the modified version of the deck state.
            H.gets DCS.deckPath >>= traverse_ \path' →
              let deckHash =
                    case st.viewingCard of
                      Nothing →
                        mkWorkspaceHash path' (NA.Load st.accessType) st.globalVarMap
                      Just cid →
                        mkWorkspaceCardHash path' cid st.accessType st.globalVarMap
              in H.fromEff $ locationObject >>= Location.setHash deckHash

  where

  isUnsaved ∷ DCS.State → Boolean
  isUnsaved = isNothing ∘ DCS.deckPath

  isNewExploreDeck ∷ DCS.State → Boolean
  isNewExploreDeck { cards } =
    let
      cardArrays = List.toUnfoldable (map _.ty cards)
    in
      cardArrays ≡ [ CT.OpenResource ] ∨ cardArrays ≡ [ CT.OpenResource, CT.JTable ]

  genId ∷ DirPath → Maybe DeckId → DeckDSL (Either Exn.Error DeckId)
  genId path deckId = case deckId of
    Just id' → pure $ Right id'
    Nothing → map DeckId <$> NB.fresh (path </> Pathy.file "index")

  -- Saves a workspace and returns the name it was saved as.
  save ∷ DirPath → DeckId → Json → DeckDSL (Either Exn.Error Unit)
  save dir deckId json = Quasar.save (deckIndex dir deckId) json

deckIndex ∷ DirPath → DeckId → FilePath
deckIndex path deckId = path </> Pathy.dir (deckIdToString deckId) </> Pathy.file "index"

setDeckState :: DCS.State -> DeckDSL Unit
setDeckState newState = do
  nextActionCardElement <- H.gets _.nextActionCardElement
  H.set $ newState { nextActionCardElement = nextActionCardElement }
