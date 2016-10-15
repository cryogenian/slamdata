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
  ( initialState
  , render
  , eval
  , peek
  , module SlamData.Workspace.Deck.Component.Query
  , module DCS
  , module SlamData.Workspace.Deck.DeckPath
  ) where

import SlamData.Prelude

import Control.Monad.Aff as Aff
import Control.Monad.Eff.Exception as Exception
import Control.Monad.Aff.Bus as Bus
import Control.Monad.Aff.EventLoop as EventLoop
import Control.Monad.Aff.Promise as Pr
import Control.UI.Browser (locationObject, setHref, newTab)

import Data.Array as Array
import Data.Foldable (find, any)
import Data.Lens ((.~), (%~), (^?), (?~))
import Data.Lens as Lens
import Data.Lens.Prism.Coproduct (_Right)
import Data.List ((:))
import Data.List as L
import Data.Map as Map
import Data.Path.Pathy ((</>))
import Data.Path.Pathy as Pathy
import Data.Set as Set
import Data.Time.Duration (Milliseconds(..))

import DOM.HTML.Location as Location
import DOM.HTML.HTMLElement (getBoundingClientRect)

import Halogen as H
import Halogen.Component.Opaque.Unsafe (opaqueState)
import Halogen.Component.Utils (raise', sendAfter', subscribeToBus')
import Halogen.Component.Utils.Debounced (fireDebouncedQuery')
import Halogen.HTML.Indexed as HH

import SlamData.Analytics as SA
import SlamData.Config as Config
import SlamData.Effects (SlamDataEffects)
import SlamData.FileSystem.Resource as R
import SlamData.FileSystem.Routing (parentURL)
import SlamData.GlobalError as GE
import SlamData.Guide as Guide
import SlamData.Quasar.Error as QE
import SlamData.Wiring (Wiring(..), CardEval, Cache, DeckMessage(..), putCardEval, putCache, getCache, makeCache)
import SlamData.Wiring as W
import SlamData.Workspace.AccessType as AT
import SlamData.Workspace.Action as WA
import SlamData.Workspace.Card.CardId (CardId(..), _CardId)
import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.Common.EvalQuery as CEQ
import SlamData.Workspace.Card.Component (CardQueryP, CardQuery(..), InnerCardQuery, AnyCardQuery, _NextQuery)
import SlamData.Workspace.Card.Component.Query as CQ
import SlamData.Workspace.Card.Draftboard.Common as DBC
import SlamData.Workspace.Card.Draftboard.Component.Query as DBQ
import SlamData.Workspace.Card.Draftboard.Pane as Pane
import SlamData.Workspace.Card.Eval as Eval
import SlamData.Workspace.Card.InsertableCardType as ICT
import SlamData.Workspace.Card.Model as Card
import SlamData.Workspace.Card.Next.Component as Next
import SlamData.Workspace.Card.Port (Port)
import SlamData.Workspace.Card.Port as Port
import SlamData.Workspace.Card.Variables.Eval as Variables
import SlamData.Workspace.Class (getURLVarMaps)
import SlamData.Workspace.Deck.AdditionalSource (AdditionalSource(..))
import SlamData.Workspace.Deck.BackSide.Component as Back
import SlamData.Workspace.Deck.Common (DeckOptions, DeckHTML, DeckDSL)
import SlamData.Workspace.Deck.Component.ChildSlot (cpBackSide, cpCard, cpIndicator, ChildQuery, ChildSlot, CardSlot(..), cpDialog)
import SlamData.Workspace.Deck.Component.Cycle (DeckComponent)
import SlamData.Workspace.Deck.Component.Query (QueryP, Query(..), DeckAction(..))
import SlamData.Workspace.Deck.Component.Render as DCR
import SlamData.Workspace.Deck.Component.State as DCS
import SlamData.Workspace.Deck.DeckPath (deckPath, deckPath')
import SlamData.Workspace.Deck.DeckId (DeckId, deckIdToString)
import SlamData.Workspace.Deck.Dialog.Component as Dialog
import SlamData.Workspace.Deck.Dialog.Share.Model (SharingInput)
import SlamData.Workspace.Deck.Indicator.Component as Indicator
import SlamData.Workspace.Deck.Model as Model
import SlamData.Workspace.Deck.Slider as Slider
import SlamData.Workspace.Model as WM
import SlamData.Workspace.Notification as Notify
import SlamData.Workspace.Routing (mkWorkspaceHash, mkWorkspaceURL)
import SlamData.Workspace.StateMode (StateMode(..))

import Utils.DOM (elementEq)
import Utils.LocalStorage as LocalStorage
import Utils.Path (DirPath)

initialState ∷ DirPath → DeckId → DCS.StateP
initialState path = opaqueState ∘ DCS.initialDeck path

render ∷ DeckOptions → (DeckOptions → DeckComponent) → DCS.State → DeckHTML
render opts deckComponent st =
  -- HACK: required so that nested finalizers get run. Since this is run inside
  -- of a separate runUI instance with Deck.Component.Nested, they will not
  -- get invoked by normal machinery. -nf
  if st.finalized
  then HH.div_ []
  else case st.stateMode of
    Error err → DCR.renderError err
    _ → DCR.renderDeck opts deckComponent st

eval ∷ DeckOptions → Query ~> DeckDSL
eval opts = case _ of
  Init next → do
    Wiring wiring ← H.liftH $ H.liftH ask
    pb ← subscribeToBus' (H.action ∘ RunPendingCards) wiring.pending
    mb ← subscribeToBus' (H.action ∘ HandleMessage) wiring.messaging
    H.modify $ DCS._breakers .~ [pb, mb]
    when (L.null opts.cursor) do
      eb ← subscribeToBus' (H.action ∘ HandleError) wiring.globalError
      H.modify $ DCS._breakers %~ (Array.cons eb)
    updateCardSize
    pure next
  PresentAccessNextActionCardGuide next → do
    H.modify (DCS._presentAccessNextActionCardGuide .~ true) $> next
  HideAccessNextActionCardGuide next →
    dismissAccessNextActionCardGuide $> next
  Finish next → do
    H.modify _ { finalized = true }
    H.gets _.breakers >>= traverse_ (H.fromAff ∘ EventLoop.break')
    pure next
  Load dir deckId next → do
    loadDeck opts dir deckId
    pure next
  SetModel deckId model next → do
    state ← H.get
    setModel opts
      { path: state.path
      , id: deckId
      , parent: model.parent
      , modelCards: Tuple deckId <$> model.cards
      , name: state.name
      }
    pure next
  ExploreFile res next → do
    SA.track SA.Explore
    H.modify
      $ (DCS.addCard $ Card.cardModelOfType CT.Table)
      ∘ (DCS.addCard ∘ Card.Open ∘ Just $ R.File res)
      ∘ (DCS._stateMode .~ Preparing)
    initialCard ← H.gets (map DCS.coordModelToCoord ∘ Array.head ∘ _.modelCards)
    for_ initialCard queuePendingCard
    saveDeck opts Nothing
    updateIndicator
    pure next
  Publish next → do
    path ← H.gets deckPath
    H.fromEff ∘ newTab $ mkWorkspaceURL path (WA.Load AT.ReadOnly)
    pure next
  -- TODO: How can we get rid of this? What is it's purpose? It smells.
  Reset path next → do
    H.modify \st →
      (DCS.initialDeck path st.id)
        { stateMode = Ready
        , displayCards = [ st.id × nextActionCard ]
        , deckElement = st.deckElement
        , responsiveSize = st.responsiveSize
        , breakers = st.breakers
        }
    pure next
  SetParent parent next →
    H.modify (DCS._parent .~ Just parent) $> next
  GetModelCards k →
    k <$> getModelCards
  SetModelCards modelCards next → do
    st ← H.get
    setModel opts
      { path: st.path
      , id: st.id
      , parent: st.parent
      , modelCards
      , name: st.name
      }
    saveDeck opts Nothing
    pure next
  GetId k →
    k <$> H.gets _.id
  GetParent k →
    k <$> H.gets _.parent
  Save coord next →
    saveDeck opts coord $> next
  RunPendingCards { source, pendingCard, cards } next → do
    st ← H.get
    let pendingCoord = DCS.coordModelToCoord pendingCard
    when (any (DCS.eqCoordModel pendingCoord) st.modelCards) do
      H.modify $ DCS.addPendingCard pendingCoord
      runPendingCards opts source pendingCard cards
    pure next
  QueuePendingCard next → do
    H.gets _.pendingCard >>= traverse_ \pending → do
      modelCards ← getModelCards
      H.modify
        $ (DCS._modelCards .~ modelCards)
        ∘ (DCS._pendingCard .~ Nothing)
      queuePendingCard pending
    pure next
  GetVarMaps k → do
    deckPath ← H.gets deckPath
    k <$> getVarMaps deckPath
  FlipDeck next → do
    updateBackSide opts
    H.modify
      $ DCS._displayMode %~ case _ of
        DCS.Normal → DCS.Backside
        _ → DCS.Normal
    presentFlipGuideFirstTime
    pure next
  GrabDeck _ next →
    pure next
  UpdateCardSize next → do
    updateCardSize
    pure next
  ZoomIn next → do
    st ← H.get
    varMaps ← getURLVarMaps
    let deckHash = mkWorkspaceHash (deckPath st) (WA.Load opts.accessType) varMaps
    H.fromEff $ locationObject >>= Location.setHash deckHash
    pure next
  ZoomOut next → do
    st ← H.get
    case st.parent of
      Just (Tuple deckId _) → do
        varMaps ← getURLVarMaps
        let deckHash = mkWorkspaceHash (deckPath' st.path deckId) (WA.Load opts.accessType) varMaps
        H.fromEff $ locationObject >>= Location.setHash deckHash
      Nothing →
        void $ H.fromEff $ setHref $ parentURL $ Left st.path
    pure next
  StartSliding mouseEvent gDef next → do
    H.gets _.deckElement >>= traverse_ \el → do
      width ← getBoundingClientWidth el
      H.modify (DCS._cardElementWidth ?~ width)
      Slider.startSliding mouseEvent gDef
    pure next
  StopSlidingAndSnap mouseEvent next → do
    st ← H.get
    for_ st.activeCardIndex \oldIndex →
      for_ (DCS.cardCoordFromIndex oldIndex st) \coord →
        void $ queryCardEval coord $ H.action CQ.DeactivateCard
    Slider.stopSlidingAndSnap mouseEvent
    updateIndicator
    updateActiveState
    eq (Just CT.NextAction) ∘ DCS.activeCardType <$> H.get >>=
      flip when dismissAccessNextActionCardGuide

    pure next
  UpdateSliderPosition mouseEvent next →
    Slider.updateSliderPosition mouseEvent $> next
  SetCardElement element next → do
    H.modify _ { deckElement = element }
    pure next
  StopSliderTransition next → do
    sliderTransition ← H.gets _.sliderTransition
    when sliderTransition $
      H.modify $ DCS._sliderTransition .~ false
    pure next
  DoAction _ next → pure next
  Focus next → do
    st ← H.get
    when (not st.focused) do
      H.modify (DCS._focused .~ true)
      Wiring wiring ← H.liftH $ H.liftH ask
      H.fromAff $ Bus.write (DeckFocused st.id) wiring.messaging
    pure next
  Defocus ev next → do
    st ← H.get
    isFrame ← H.fromEff $ elementEq ev.target ev.currentTarget
    when (st.focused && isFrame) $
      for_ (L.last opts.cursor) \rootId → do
        Wiring wiring ← H.liftH $ H.liftH ask
        H.fromAff $ Bus.write (DeckFocused rootId) wiring.messaging
    pure next
  HandleMessage msg next → do
    st ← H.get
    case msg of
      DeckFocused focusedDeckId → do
        when (st.id ≡ focusedDeckId && not st.focused) $
          H.modify (DCS._focused .~ true)
        when (st.id ≠ focusedDeckId && st.focused) $
          H.modify (DCS._focused .~ false)
      URLVarMapsUpdated →
        traverse_ runCard $ DCS.variablesCards st
    pure next
  GetModel k →
    k <$> getDeckModel
  GetSharingInput k →
    k <$> getSharingInput
  HandleError ge next → do
    showDialog $ Dialog.Error $ GE.print ge
    pure next
  Run next → do
    H.modify _{ stateMode = Preparing }
    initialCard ← H.gets (map DCS.coordModelToCoord ∘ Array.head ∘ _.modelCards)
    for_ initialCard queuePendingCard
    pure next

  where
  getBoundingClientWidth =
    H.fromEff ∘ map _.width ∘ getBoundingClientRect

-- | Accumulates all `VarMap`s within the deck, including `VarMaps` from any
-- | child decks within board cards.
getVarMaps ∷ DirPath → DeckDSL (Map.Map DeckId Port.VarMap)
getVarMaps path =
  Map.fromFoldable <$> (Array.foldM goCard L.Nil =<< H.gets _.modelCards)
  where
  goCard
    ∷ L.List (DeckId × Port.VarMap)
    → DeckId × Card.Model
    → DeckDSL (L.List (DeckId × Port.VarMap))
  goCard acc (deckId × model) =
    case model.model of
      Card.Variables vm →
        pure $ (deckId × Variables.eval deckId Map.empty vm) : acc
      Card.Draftboard dbm →
        L.foldM goDeck acc (L.catMaybes $ Pane.toList dbm.layout)
      _ →
        pure acc
  goDeck
    ∷ L.List (DeckId × Port.VarMap)
    → DeckId
    → DeckDSL (L.List (DeckId × Port.VarMap))
  goDeck acc deckId = do
    res ← runExceptT do
      deck ← ExceptT $ getDeck path deckId
      mirroredCards ← ExceptT $ loadMirroredCards path deck.mirror
      pure $ mirroredCards <> (Tuple deckId <$> deck.cards)
    case res of
      Left err → do
        Notify.loadDeckFail err
        pure acc
      Right cards →
        Array.foldM goCard acc cards

peek ∷ ∀ a. DeckOptions → H.ChildF ChildSlot ChildQuery a → DeckDSL Unit
peek opts (H.ChildF s q) =
  (peekCards ⊹ (\_ _ → pure unit) $ s)
   ⨁ peekBackSide opts
   ⨁ (const $ pure unit)
   ⨁ (peekDialog opts ⨁ (const $ pure unit))
   $ q

peekDialog ∷ ∀ a. DeckOptions → Dialog.Query a → DeckDSL Unit
peekDialog _ (Dialog.Show _ _) = do
  H.modify (DCS._displayMode .~ DCS.Dialog)
peekDialog _ (Dialog.Dismiss _) =
  H.modify (DCS._displayMode .~ DCS.Backside)
peekDialog _ (Dialog.FlipToFront _) =
  H.modify (DCS._displayMode .~ DCS.Normal)
peekDialog opts (Dialog.SetDeckName name _) =
  H.modify ((DCS._displayMode .~ DCS.Normal) ∘ (DCS._name .~ name))
    *> saveDeck opts Nothing
peekDialog _ (Dialog.Confirm d b _) = do
  H.modify (DCS._displayMode .~ DCS.Backside)
  case d of
    Dialog.DeleteDeck | b → raise' $ H.action $ DoAction DeleteDeck
    _ → pure unit

peekBackSide ∷ ∀ a. DeckOptions → Back.Query a → DeckDSL Unit
peekBackSide opts (Back.DoAction action _) =
  case action of
    Back.Trash → do
      state ← H.get
      lastId ← H.gets DCS.findLastRealCard
      for_ (DCS.activeCardCoord state <|> lastId)  \trashId →
        case snd trashId of
          ErrorCardId → do
            showDialog
              $ Dialog.Error "You cannot delete the error card. Please, fix errors or slide to previous card."
          NextActionCardId → do
            showDialog
              $ Dialog.Error "You cannot delete the next action card. Please, slide to previous card."
          PendingCardId → do
            showDialog
              $ Dialog.Error "You cannot delete the pending card. Please, wait till card evaluation is finished."
          CardId _ → do
            let rem = DCS.removeCard trashId state
            H.liftH $ H.liftH $
              parTraverse_ (DBC.deleteGraph state.path) $
                DBC.childDeckIds (snd <$> fst rem)
            H.set $ snd rem
            triggerSave Nothing
            updateActiveCardAndIndicator
            H.modify $ (DCS._displayMode .~ DCS.Normal) ∘ (DCS._presentAccessNextActionCardGuide .~ false)
            DCS.activeCardCoord (snd rem)
              # maybe (runCardUpdates opts state.id L.Nil) queuePendingCard
      void $ H.queryAll' cpCard $ left $ H.action UpdateDimensions
    Back.Rename → do
      name ← H.gets _.name
      showDialog $ Dialog.Rename name
    Back.Share → do
      sharingInput ← getSharingInput
      showDialog $ Dialog.Share sharingInput
    Back.Unshare → do
      sharingInput ← getSharingInput
      showDialog $ Dialog.Unshare sharingInput
    Back.Embed → do
      st ← H.get
      SA.track (SA.Embed st.id)
      sharingInput ← getSharingInput
      varMaps ← getVarMaps (deckPath st)
      showDialog $ Dialog.Embed sharingInput varMaps
    Back.Publish → do
      st ← H.get
      SA.track (SA.Publish st.id)
      sharingInput ← getSharingInput
      varMaps ← getVarMaps (deckPath st)
      showDialog $ Dialog.Publish sharingInput varMaps
    Back.DeleteDeck → do
      cards ← H.gets _.modelCards
      if Array.null cards
        then raise' $ H.action $ DoAction DeleteDeck
        else showDialog Dialog.DeleteDeck
    Back.Mirror → do
      H.modify $ DCS._displayMode .~ DCS.Normal
      raise' $ H.action $ DoAction Mirror
    Back.Wrap →
      raise' $ H.action $ DoAction Wrap
    Back.Unwrap decks →
      raise' $ H.action $ DoAction $ Unwrap decks
peekBackSide _ _ = pure unit

peekCards ∷ ∀ a. CardSlot → CardQueryP a → DeckDSL Unit
peekCards (CardSlot cardId) = const (pure unit) ⨁ peekCardInner cardId

showDialog ∷ Dialog.Dialog → DeckDSL Unit
showDialog dlg = do
  queryDialog $ H.action $ Dialog.Show dlg
  H.modify (DCS._displayMode .~ DCS.Dialog)

queryDialog ∷ Dialog.Query Unit → DeckDSL Unit
queryDialog q = H.query' cpDialog unit (left q) *> pure unit

queryCard ∷ ∀ a. DeckId × CardId → CQ.AnyCardQuery a → DeckDSL (Maybe a)
queryCard cid =
  H.query' cpCard (CardSlot cid)
    ∘ right
    ∘ H.ChildF unit
    ∘ right

queryCardEval ∷ ∀ a. DeckId × CardId → CQ.CardQuery a → DeckDSL (Maybe a)
queryCardEval cid =
  H.query' cpCard (CardSlot cid) ∘ left

updateActiveCardAndIndicator ∷ DeckDSL Unit
updateActiveCardAndIndicator = do
  st ← H.get
  case st.activeCardIndex of
    Nothing → H.modify $ DCS._activeCardIndex .~ Just (DCS.defaultActiveIndex st)
    Just _ → pure unit
  updateIndicator
  updateActiveState

updateIndicator ∷ DeckDSL Unit
updateIndicator = do
  cards ← H.gets _.displayCards
  H.query' cpIndicator unit
    $ H.action
    $ Indicator.UpdatePortList
    $ map (Card.modelCardType ∘ _.model ∘ snd) cards
  vid ← H.gets $ fromMaybe 0 ∘ _.activeCardIndex
  void $ H.query' cpIndicator unit $ H.action $ Indicator.UpdateActiveId vid

updateActiveState ∷ DeckDSL Unit
updateActiveState = do
  st ← H.get
  Wiring wiring ← H.liftH $ H.liftH ask
  for_ st.activeCardIndex \cardIndex → do
    putCache st.id { cardIndex } wiring.activeState
    for_ (DCS.cardCoordFromIndex cardIndex st) \coord →
      void $ queryCardEval coord $ H.action CQ.ActivateCard

updateBackSide ∷ DeckOptions → DeckDSL Unit
updateBackSide { cursor } = do
  state ← H.get
  let ty = DCS.activeCardType state
  let tys = Card.modelCardType ∘ _.model ∘ snd <$> state.displayCards
  void $ H.query' cpBackSide unit $ H.action $ Back.UpdateCardType ty tys

  -- For an unwrap to be at all possible the current deck must only have one
  -- card, which is a board card
  when (ty == Just CT.Draftboard && Array.length state.modelCards == 1) $
    void $ runMaybeT do
      coord ← MaybeT $ pure $ DCS.activeCardCoord state

      -- TODO: 😱 never do this... if the query fails because the prism
      -- fails, a runtime error will occur. It should be safe here, because
      -- we're guarded by the card type, but even still it would be better for
      -- this to arise through the card itself somehow. -gb
      decks ← MaybeT $
        queryCard coord $ Lens.review CQ._DraftboardQuery $ left $ right $
          H.request DBQ.GetDecks

      -- Further to the prior predicate, it is only possible to unwrap when one
      -- of the following hold:
      --   - a board is a child of another board
      --   - there is only one deck inside a root board
      when (not (L.null cursor) || L.length (L.catMaybes (Pane.toList decks)) == 1) $
        void $ lift $
          H.query' cpBackSide unit $ H.action $ Back.SetUnwrappable (Just decks)

      pure unit

createCard ∷ CT.CardType → DeckDSL Unit
createCard cardType = do
  presentAccessNextActionCardGuideAfterDelay
  SA.track (SA.AddCard cardType)
  deckId ← H.gets _.id
  (st × newCardId) ← H.gets ∘ DCS.addCard' $ Card.cardModelOfType cardType
  H.set st
  when (isNothing st.pendingCard) do
    queuePendingCard (deckId × newCardId)
  triggerSave $ Just (deckId × newCardId)

dismissedAccessNextActionCardGuideKey ∷ String
dismissedAccessNextActionCardGuideKey = "dismissedAccessNextActionCardGuide"

getDismissedAccessNextActionCardGuideBefore ∷ DeckDSL Boolean
getDismissedAccessNextActionCardGuideBefore =
  H.liftH $ H.liftH
    $ either (const $ false) id
    <$> LocalStorage.getLocalStorage dismissedAccessNextActionCardGuideKey

storeDismissedAccessNextActionCardGuide ∷ DeckDSL Unit
storeDismissedAccessNextActionCardGuide =
  H.liftH $ H.liftH $ LocalStorage.setLocalStorage dismissedAccessNextActionCardGuideKey true

presentAccessNextActionCardGuideAfterDelay ∷ DeckDSL Unit
presentAccessNextActionCardGuideAfterDelay = do
  dismissedBefore ← getDismissedAccessNextActionCardGuideBefore
  if dismissedBefore
    then pure unit
    else do
      cancelPresentAccessNextActionCardGuide
      H.modify
        ∘ (DCS._presentAccessNextActionCardGuideCanceler .~ _)
        ∘ Just
        =<< (sendAfter' Config.addCardGuideDelay $ PresentAccessNextActionCardGuide unit)

cancelPresentAccessNextActionCardGuide ∷ DeckDSL Boolean
cancelPresentAccessNextActionCardGuide =
  H.fromAff ∘ maybe (pure false) (flip Aff.cancel $ Exception.error "Cancelled")
    =<< H.gets _.presentAccessNextActionCardGuideCanceler

dismissAccessNextActionCardGuide ∷ DeckDSL Unit
dismissAccessNextActionCardGuide =
  H.gets _.presentAccessNextActionCardGuide >>=
    flip when do
      H.modify (DCS._presentAccessNextActionCardGuide .~ false)
      storeDismissedAccessNextActionCardGuide

resetAccessNextActionCardGuideDelay ∷ DeckDSL Unit
resetAccessNextActionCardGuideDelay =
  cancelPresentAccessNextActionCardGuide
  >>= if _ then presentAccessNextActionCardGuideAfterDelay else pure unit

deckDSLLater ∷ Int → DeckDSL Unit → DeckDSL Unit
deckDSLLater ms action =
  (H.fromAff $ Aff.later' ms noOp) *> action
  where
  noOp ∷ Aff.Aff SlamDataEffects Unit
  noOp = pure unit

peekCardInner
  ∷ ∀ a
  . DeckId × CardId
  → H.ChildF Unit InnerCardQuery a
  → DeckDSL Unit
peekCardInner cardCoord = H.runChildF ⋙
  (peekCardEvalQuery cardCoord ⨁ peekAnyCard cardCoord)

peekCardEvalQuery ∷ ∀ a. DeckId × CardId → CEQ.CardEvalQuery a → DeckDSL Unit
peekCardEvalQuery cardCoord = case _ of
  CEQ.ModelUpdated CEQ.StateOnlyUpdate _ → triggerSave (Just cardCoord)
  CEQ.ModelUpdated CEQ.EvalModelUpdate _ → runCard cardCoord *> triggerSave (Just cardCoord)
  CEQ.ZoomIn _ → raise' $ H.action ZoomIn
  _ → pure unit

peekAnyCard ∷ ∀ a. DeckId × CardId → AnyCardQuery a → DeckDSL Unit
peekAnyCard cardCoord q = do
  resetAccessNextActionCardGuideDelay
  for_ (q ^? _NextQuery ∘ _Right ∘ Next._AddCardType) createCard
  for_ (q ^? _NextQuery ∘ _Right ∘ Next._PresentReason) $ uncurry presentReason

presentReason ∷ (Maybe Port.Port) → CT.CardType → DeckDSL Unit
presentReason input cardType =
  traverse_ showDialog dialog
  where
  insertableCardType = ICT.fromCardType cardType
  ioType = ICT.fromMaybePort input
  reason = ICT.reason ioType cardType
  cardPaths = map (ICT.cardPathsBetween ioType) insertableCardType
  dialog = Dialog.Reason cardType <$> reason <*> cardPaths

nextActionCard ∷ Card.Model
nextActionCard =
  { cardId: NextActionCardId
  , model: Card.NextAction
  }

errorCard ∷ Card.Model
errorCard =
  { cardId: ErrorCardId
  , model: Card.ErrorCard
  }

pendingEvalCard ∷ Card.Model
pendingEvalCard =
  { cardId: PendingCardId
  , model: Card.PendingCard
  }

type UpdateAccum =
  { cards ∷ L.List (DeckId × Card.Model)
  , steps ∷ L.List CardEval
  , updates ∷ L.List CardEval
  }

type UpdateResult =
  { displayCards ∷ Array (DeckId × Card.Model)
  , updates ∷ L.List CardEval
  }

queuePendingCard
  ∷ DeckId × CardId
  → DeckDSL Unit
queuePendingCard pendingCoord = do
  st ← H.get
  Wiring wiring ← H.liftH $ H.liftH ask
  for_ (find (DCS.eqCoordModel pendingCoord) st.modelCards) \pendingCard →
    H.fromAff do
      cards ← makeCache
      Bus.write { source: st.id, pendingCard, cards } wiring.pending

runPendingCards
  ∷ DeckOptions
  → DeckId
  → DeckId × Card.Model
  → Cache (DeckId × CardId) CardEval
  → DeckDSL Unit
runPendingCards opts source pendingCard pendingCards = do
  st ← H.get
  Wiring wiring ← H.liftH $ H.liftH ask
  let
    pendingCoord = DCS.coordModelToCoord pendingCard
    splitCards = L.span (not ∘ DCS.eqCoordModel pendingCoord) $ L.fromFoldable st.modelCards
    prevCard = DCS.coordModelToCoord <$> L.last splitCards.init
    pendingCards = L.Cons pendingCard <$> L.tail splitCards.rest

  for_ pendingCards \cards → do
    input ← join <$> for prevCard (flip getCache wiring.cards)
    steps ← resume wiring st (input >>= _.output <#> map fst) cards
    runCardUpdates opts source steps
    H.modify $ DCS.removePendingCard $ DCS.coordModelToCoord pendingCard

  where
  resume wiring st = go L.Nil where
    go steps input L.Nil = pure $ L.reverse steps
    go steps input (L.Cons c cs) = do
      step ←
        getCache (DCS.coordModelToCoord c) pendingCards >>= case _ of
          Just ev → pure ev
          Nothing → do
            urlVarMaps ← getURLVarMaps
            ev ← evalCard st.path urlVarMaps input c
            putCardEval ev pendingCards
            putCardEval ev wiring.cards
            pure ev
      go (L.Cons step steps) (map (map fst) step.output) cs

-- | When we initially eval a deck we want to use whatever is in the main cache.
-- | If we were to just queuePendingCard then everything would get freshly
-- | evaluated, which is unnecessary.
runInitialEval ∷ DeckDSL Unit
runInitialEval = do
  st ← H.get
  Wiring wiring ← H.liftH $ H.liftH ask
  cards ← makeCache
  let
    cardCoords = DCS.coordModelToCoord <$> L.fromFoldable st.modelCards
    source = st.id

  for_ cardCoords \coord →
    getCache coord wiring.cards >>= traverse_ \ev → do
      putCardEval ev cards

  for_ (Array.head st.modelCards) \pendingCard → do
    H.fromAff $ Bus.write { source, pendingCard, cards } wiring.pending

-- | Evaluates a card given an input and model.
evalCard
  ∷ DirPath
  → Map.Map DeckId Port.URLVarMap
  → Maybe (Pr.Promise Port)
  → DeckId × Card.Model
  → DeckDSL CardEval
evalCard path urlVarMaps input card = do
  Wiring wiring ← H.liftH $ H.liftH ask
  output ← H.liftH $ H.liftH $ Pr.defer do
    input' ← for input Pr.wait

    let
      model = (snd card).model
    case Card.modelToEval model of
      Left err → do
        SA.track (SA.ErrorInCardEval $ Card.modelCardType model)
        pure $ (Port.CardError $ "Could not evaluate card: " <> err) × mempty
      Right cmd →
        let
          args =
            { path
            , urlVarMaps
            , cardCoord: DCS.coordModelToCoord card
            , input: input'
            }
        in
          Eval.runEvalCard args cmd >>= case _ of
            Left ge → do
              GE.raiseGlobalError ge
              pure $ (Port.CardError $ "Could not evaluate card") × mempty
            Right out@(Tuple p _) → do
              case p of
                Port.CardError _ →
                  SA.track (SA.ErrorInCardEval $ Card.modelCardType model)
                _ → pure unit
              pure out
  pure { input, card, output: Just output }

-- | Interprets a list of pending card evaluations into updates for the
-- | display cards. Takes care of the pending card, next action card and
-- | error card.
runCardUpdates ∷ DeckOptions → DeckId → L.List CardEval → DeckDSL Unit
runCardUpdates opts source steps = do
  st ← H.get
  let
    realCards = Array.filter (Lens.has _CardId ∘ _.cardId ∘ snd) st.displayCards
    loadedCards = foldr (Set.insert ∘ DCS.coordModelToCoord) Set.empty realCards
    pendingCm = st.id × pendingEvalCard
    nextActionStep =
      { card: st.id × nextActionCard
      , input: map (map fst) <$> _.output =<< L.last steps
      , output: Nothing
      }
    updateSteps =
      if AT.isEditable opts.accessType
        then L.snoc steps nextActionStep
        else steps

  H.modify $
    DCS._displayCards .~ Array.snoc realCards pendingCm

  updateResult ←
    H.liftH $ H.liftH $ Pr.wait $
      updateCards st { steps: updateSteps, cards: mempty, updates: mempty }

  -- Cleanup initial presentation. Note, we do this here to eliminate jank.
  -- Doing it at the end results in cards jumping around as renders are flushed
  -- when we apply the child updates.
  when (st.stateMode == Preparing) do
    Wiring wiring ← H.liftH $ H.liftH ask
    activeIndex ← map _.cardIndex <$> getCache st.id wiring.activeState
    lastIndex ← H.gets DCS.findLastRealCardIndex
    -- When a deck is deeply nested, we should treat it as "published", such
    -- that it always show the last available card.
    let
      activeCardIndex =
        if L.length opts.cursor > 1
          then lastIndex
          else activeIndex <|> lastIndex
    H.modify
      $ (DCS._stateMode .~ Ready)
      ∘ (DCS._activeCardIndex .~ activeCardIndex)

  -- Splice in the new display cards
  for_ (Array.head updateResult.displayCards) \card → do
    let
      pendingId = DCS.coordModelToCoord card
      oldCards = Array.takeWhile (not ∘ DCS.eqCoordModel pendingId) realCards
      displayCards = oldCards <> updateResult.displayCards
    H.modify $ DCS._displayCards .~ displayCards

  updateActiveCardAndIndicator
  for_ updateResult.updates $ updateCard st source loadedCards
  updateCardSize

  where
  updateCards ∷ DCS.State → UpdateAccum → Pr.Promise UpdateResult
  updateCards st = case _ of
    { steps: L.Nil, cards, updates } →
      pure
        { displayCards: Array.fromFoldable $ L.reverse cards
        , updates: L.reverse updates
        }
    { steps: L.Cons x xs, cards, updates } → do
      output ← sequence x.output
      let cards' = L.Cons x.card cards
          updates' = L.Cons x updates
      updateCards st $ case output of
        Just ((Port.CardError err) × _) →
          let errorCard' = st.id × errorCard
              errorStep  = { input: map (map fst) $ x.output, output: Nothing, card: errorCard' }
          in
              { steps: L.Nil
              , cards: L.Cons errorCard' cards'
              , updates: L.Cons errorStep updates'
              }
        _ →
          { steps: xs
          , cards: cards'
          , updates: updates'
          }

  updateCard
    ∷ DCS.State
    → DeckId
    → Set.Set (DeckId × CardId)
    → CardEval
    → DeckDSL Unit
  updateCard st source loadedCards step = void do
    input ← for step.input (H.liftH ∘ H.liftH ∘ Pr.wait)
    output ← for step.output (H.liftH ∘ H.liftH ∘ Pr.wait)
    urlVarMaps ← getURLVarMaps
    let
      cardCoord = DCS.coordModelToCoord step.card
      evalInput = { path: st.path, urlVarMaps, input, cardCoord }
      newSet = foldMap snd output

    H.modify $ DCS._additionalSources %~ Map.insert cardCoord newSet

    -- We always load the card when we are updating a mirrored deck. This is
    -- so mirrors will always have the most recent model.
    when (not (Set.member cardCoord loadedCards) || st.id ≠ source) $ void do
      queryCardEval cardCoord $ H.action $ LoadCard (snd step.card)

    queryCardEval cardCoord $ H.action $ UpdateCard evalInput (map fst output)

-- | Enqueues the card with the specified ID in the set of cards that are
-- | pending to run and enqueues a debounced query to trigger the cards to
-- | actually run.
runCard ∷ DeckId × CardId → DeckDSL Unit
runCard coord = do
  H.modify (DCS.addPendingCard coord)
  fireDebouncedQuery' (Milliseconds 500.0) DCS._runTrigger QueuePendingCard

-- | Triggers the H.query for autosave. This does not immediate perform the save
-- | H.action, but instead enqueues a debounced query to trigger the actual save.
triggerSave ∷ Maybe (DeckId × CardId) → DeckDSL Unit
triggerSave coord =
  fireDebouncedQuery' (Milliseconds 500.0) DCS._saveTrigger $ Save coord

getDeckModel ∷ DeckDSL Model.Deck
getDeckModel = do
  st ← H.get
  modelCards ← Array.span (not ∘ eq st.id ∘ fst) <$> getModelCards
  pure
    { parent: st.parent
    , mirror: map _.cardId <$> modelCards.init
    , cards: snd <$> modelCards.rest
    , name: st.name
    }

-- | Saves the deck as JSON, using the current values present in the state.
saveDeck ∷ DeckOptions → Maybe (DeckId × CardId) → DeckDSL Unit
saveDeck { accessType, cursor } coord = do
  st ← H.get
  when (AT.isEditable accessType) do
    modelCards ← Array.span (not ∘ eq st.id ∘ fst) <$> getModelCards
    coord >>= DCS.eqCoordModel >>> flip find modelCards.init
      # maybe' (\_ → saveMainDeck st modelCards) (saveMirroredCard st)

  where
  saveMainDeck st modelCards = do
    let model =
          { parent: st.parent
          , mirror: map _.cardId <$> modelCards.init
          , cards: snd <$> modelCards.rest
          , name: st.name
          }

    when (isNothing st.parent) do
      let index = st.path </> Pathy.file "index"
      WM.getRoot index >>= case _ of
        Left _ → void $ WM.setRoot index st.id
        Right _ → pure unit

    putDeck st.path st.id model >>= case _ of
      Left err →
        Notify.saveDeckFail err
      Right _ → do
        when (L.null cursor) $ do
          path' ← H.gets deckPath
          varMaps ← getURLVarMaps
          let deckHash = mkWorkspaceHash path' (WA.Load accessType) varMaps
          H.fromEff $ locationObject >>= Location.setHash deckHash

  saveMirroredCard st (deckId × card) =
    getDeck st.path deckId >>= case _ of
      Left err →
        Notify.saveMirrorFail err
      Right deck → do
        let cards = deck.cards <#> \c → if c.cardId == card.cardId then card else c
            model = deck { cards = cards }
        void $ putDeck st.path deckId model

loadDeck ∷ DeckOptions → DirPath → DeckId → DeckDSL Unit
loadDeck opts path deckId = do
  H.modify
    $ (DCS._stateMode .~ Loading)
    ∘ (DCS._displayCards .~ [ deckId × pendingEvalCard ])

  res ← runExceptT do
    deck ← ExceptT $ getDeck path deckId
    mirroredCards ← ExceptT $ loadMirroredCards path deck.mirror
    pure $ deck × (mirroredCards <> (Tuple deckId <$> deck.cards))
  case res of
    Left err →
      H.modify $ DCS._stateMode .~ Error "There was a problem decoding the saved deck"
    Right (deck × modelCards) →
      setModel opts
        { path
        , id: deckId
        , parent: deck.parent
        , modelCards
        , name: deck.name
        }

loadMirroredCards
  :: DirPath
  → Array (DeckId × CardId)
  → DeckDSL (Either QE.QError (Array (DeckId × Card.Model)))
loadMirroredCards path coords = do
  let deckIds = Array.nub (fst <$> coords)
  res ← H.liftH $ H.liftH $ sequence <$> parTraverse (W.getDeck path) deckIds
  pure $ hydrateCards coords =<< map (Array.zip deckIds) res
  where
  hydrateCards coords decks =
    for coords \(deckId × cardId) →
      case find (eq deckId ∘ fst) decks of
        Nothing → Left (QE.msgToQError "Deck not found")
        Just (_ × deck) →
          case find (eq cardId ∘ _.cardId) deck.cards of
            Nothing → Left (QE.msgToQError "Card not found")
            Just card → Right (deckId × card)

setModel
  ∷ DeckOptions
  → { path ∷ DirPath
    , id ∷ DeckId
    , parent ∷ Maybe (DeckId × CardId)
    , modelCards ∷ Array (DeckId × Card.Model)
    , name ∷ String
    }
  → DeckDSL Unit
setModel opts model = do
  updateCardSize
  H.modify
    $ (DCS._stateMode .~ Preparing)
    ∘ DCS.fromModel model
  presentAccessNextActionCardGuideAfterDelay
  case Array.head model.modelCards of
    Just _ →
      runInitialEval
    Nothing →
      runCardUpdates opts model.id L.Nil

getModelCards ∷ DeckDSL (Array (DeckId × Card.Model))
getModelCards = do
  st ← H.get
  for st.modelCards \(deckId × card) → do
    currentState ←
      queryCardEval (deckId × card.cardId)
        $ H.request (SaveCard card.cardId $ Card.modelCardType card.model)
    pure $ deckId × (fromMaybe card currentState)

getSharingInput ∷ DeckDSL SharingInput
getSharingInput = do
  st ← H.get
  childrenInput ← for st.modelCards \(deckId × card) → do
    case card.model of
      Card.Draftboard _ → do
        mbInputs ←
          queryCard (deckId × card.cardId)
            $ Lens.review CQ._DraftboardQuery
            $ left
            $ right
            $ H.request DBQ.GetDecksSharingInput
        pure $ maybe [] (foldMap pure) mbInputs
      _ →
        pure []

  workspacePath ← H.gets _.path
  deckId ← H.gets _.id
  mirrorIds ← Array.nub ∘ map fst ∘ _.mirror <$> getDeckModel

  additionalSources ← H.gets _.additionalSources
  let
    mirroredIndices =
      L.fromFoldable $ mirrorIds <#> \mid →
        workspacePath
          </> Pathy.dir (deckIdToString mid)
          </> Pathy.file "index"
    thisDeckSharingInput =
      foldl
      (\accum → case _ of
          Cache cfp → accum { caches = cfp:accum.caches }
          Source sfp → accum { sources = sfp:accum.sources })
      ({ workspacePath, caches: mirroredIndices, sources: L.Nil, deckId })
      (fold additionalSources)

    innerFoldFn accum child =
      { workspacePath: accum.workspacePath
      , deckId: accum.deckId
      , caches:
          L.nubBy (\a b → Pathy.printPath a ≡ Pathy.printPath b)
            $ accum.caches ⊕ child.caches
      , sources:
          L.nubBy (\a b → Pathy.printPath a ≡ Pathy.printPath b)
            $ (child.workspacePath
                </> Pathy.dir (deckIdToString child.deckId)
                </> Pathy.file "index"
              )
            : accum.sources
            ⊕ child.sources

      }

    foldChildren = foldl innerFoldFn

  pure
    $ foldl foldChildren thisDeckSharingInput childrenInput

updateCardSize ∷ DeckDSL Unit
updateCardSize = do
  H.queryAll' cpCard $ left $ H.action UpdateDimensions
  H.gets _.deckElement >>= traverse_ \el → do
    { width } ← H.fromEff $ getBoundingClientRect el
    H.modify $ DCS._responsiveSize .~ breakpoint width
  where
  breakpoint w
    | w < 240.0 = DCS.XSmall
    | w < 320.0 = DCS.Small
    | w < 420.0 = DCS.Medium
    | w < 540.0 = DCS.Large
    | w < 720.0 = DCS.XLarge
    | otherwise = DCS.XXLarge

putDeck
  ∷ DirPath
  → DeckId
  → Model.Deck
  → DeckDSL (Either QE.QError Unit)
putDeck path deckId deck =
  H.liftH $ H.liftH $ W.putDeck path deckId deck

getDeck
  ∷ DirPath
  → DeckId
  → DeckDSL (Either QE.QError Model.Deck)
getDeck path deckId =
  H.liftH $ H.liftH $ W.getDeck path deckId

presentFlipGuideFirstTime ∷ DeckDSL Unit
presentFlipGuideFirstTime = do
  H.gets _.displayMode >>=
    case _ of
      DCS.Backside → do
        W.Wiring wiring ← H.liftH $ H.liftH ask
        shouldPresentFlipGuide >>=
          if _
          then H.fromAff $ Bus.write W.FlipGuide wiring.presentStepByStepGuide
          else pure unit
      _ → pure unit

shouldPresentFlipGuide ∷ DeckDSL Boolean
shouldPresentFlipGuide =
  H.liftH
    $ H.liftH
    $ either (const true) not <$> LocalStorage.getLocalStorage Guide.dismissedFlipGuideKey
