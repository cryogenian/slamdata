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
import Control.Monad.Aff.AVar as AVar
import Control.Monad.Aff.Future as Future
import Control.Monad.Eff.Exception as Exception
import Control.Monad.Aff.Bus as Bus
import Control.Monad.Aff.EventLoop as EventLoop
import Control.UI.Browser as Browser

import Data.Array as Array
import Data.Lens ((.~), (%~), (^?), (?~), _Left, _Just, is)
import Data.Lens.Prism.Coproduct as CoP
import Data.List as L

import DOM.HTML.HTMLElement (getBoundingClientRect)

import Halogen as H
import Halogen.Component.Opaque.Unsafe (opaqueState)
import Halogen.Component.Utils (liftH', raise', sendAfter', subscribeToBus')
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP

import SlamData.AuthenticationMode as AuthenticationMode
import SlamData.Config as Config
import SlamData.FileSystem.Routing (parentURL)
import SlamData.GlobalError as GE
import SlamData.GlobalMenu.Bus (SignInMessage(..))
import SlamData.Guide as Guide
import SlamData.Quasar as Quasar
import SlamData.Quasar.Auth.Authentication as Authentication
import SlamData.Quasar.Error as QE
import SlamData.Wiring (DeckMessage(..))
import SlamData.Wiring as Wiring
import SlamData.Wiring.Cache as Cache
import SlamData.Workspace.AccessType as AT
import SlamData.Workspace.Action as WA
import SlamData.Workspace.Card.CardId (CardId)
import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.Common.EvalQuery as CEQ
import SlamData.Workspace.Card.Component (CardQueryP, CardQuery(..), InnerCardQuery, AnyCardQuery)
import SlamData.Workspace.Card.Component.Query as CQ
import SlamData.Workspace.Card.InsertableCardType as ICT
import SlamData.Workspace.Card.Model as Card
import SlamData.Workspace.Card.Port as Port
import SlamData.Workspace.Card.Next.Component.Query as Next
import SlamData.Workspace.Class (navigate, Routes(..))
import SlamData.Workspace.Deck.BackSide.Component as Back
import SlamData.Workspace.Deck.Common (DeckOptions, DeckHTML, DeckDSL)
import SlamData.Workspace.Deck.Component.ChildSlot (cpCard, ChildQuery, ChildSlot, cpDialog, cpBackSide, cpNext)
import SlamData.Workspace.Deck.Component.Cycle (DeckComponent)
import SlamData.Workspace.Deck.Component.Query (QueryP, Query(..))
import SlamData.Workspace.Deck.Component.Render as DCR
import SlamData.Workspace.Deck.Component.State as DCS
import SlamData.Workspace.Deck.DeckPath (deckPath, deckPath')
import SlamData.Workspace.Deck.DeckId (DeckId)
import SlamData.Workspace.Deck.Dialog.Component as Dialog
import SlamData.Workspace.Deck.Model as Model
import SlamData.Workspace.Deck.Slider as Slider
import SlamData.Workspace.Eval.Card as EC
import SlamData.Workspace.Eval.Deck as ED
import SlamData.Workspace.Eval.Persistence as P
import SlamData.Workspace.Eval.Traverse as ET
import SlamData.Workspace.Routing (mkWorkspaceURL)
import SlamData.Workspace.StateMode (StateMode(..))

import Utils (censor)
import Utils.DOM (elementEq)
import Utils.LocalStorage as LocalStorage

initialState ∷ DeckId → DCS.StateP
initialState = opaqueState ∘ DCS.initialDeck

render ∷ DeckOptions → (DeckOptions → DeckComponent) → DCS.State → DeckHTML
render opts deckComponent st =
  -- HACK: required so that nested finalizers get run. Since this is run inside
  -- of a separate runUI instance with Deck.Component.Nested, they will not
  -- get invoked by normal machinery. -nf
  if st.finalized
  then HH.div_ []
  else case st.stateMode of
    Error error →
      HH.div
        [ HP.class_ $ HH.className "sd-workspace-error" ]
        [ DCR.renderError error
        , if (QE.isUnauthorized error)
            then HH.p_ (DCR.renderSignInButton <$> st.providers)
            else HH.text ""
        ]
    _ → DCR.renderDeck opts deckComponent st

eval ∷ DeckOptions → Query ~> DeckDSL
eval opts = case _ of
  Init next → do
    { bus } ← liftH' Wiring.expose
    mb ← subscribeToBus' (H.action ∘ HandleMessage) bus.decks
    H.modify $ DCS._breakers .~ [mb]
    when (L.null opts.cursor) do
      eb ← subscribeToBus' (H.action ∘ HandleError) bus.globalError
      H.modify $ DCS._breakers %~ (Array.cons eb)
    updateCardSize
    loadDeck opts
    pure next
  PresentAccessNextActionCardGuide next → do
    H.modify (DCS._presentAccessNextActionCardGuide .~ true) $> next
  HideAccessNextActionCardGuide next →
    dismissAccessNextActionCardGuide $> next
  Finish next → do
    H.modify _ { finalized = true }
    H.gets _.breakers >>= traverse_ (H.fromAff ∘ EventLoop.break')
    pure next
  Publish next → do
    { path } ← liftH' Wiring.expose
    deckPath ← deckPath' path <$> H.gets _.id
    H.fromEff ∘ Browser.newTab $ mkWorkspaceURL deckPath (WA.Load AT.ReadOnly)
    pure next
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
    { path, accessType, varMaps } ← liftH' Wiring.expose
    st ← H.get
    navigateToDeck st.id
    pure next
  ZoomOut next → do
    { path, accessType, varMaps } ← liftH' Wiring.expose
    st ← H.get
    case st.parent of
      Just (Tuple deckId _) →
        navigateToDeck deckId
      Nothing →
        void $ H.fromEff $ Browser.setHref $ parentURL $ Left path
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
    updateActiveState
    when (DCS.activeCard st # is (_Just ∘ _Left ∘ DCS._NextActionCard)) do
      dismissAccessNextActionCardGuide
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
  Focus next → do
    st ← H.get
    when (not st.focused) do
      H.modify (DCS._focused .~ true)
      { bus } ← liftH' Wiring.expose
      H.fromAff $ Bus.write (DeckFocused st.id) bus.decks
      presentAccessNextActionCardGuideAfterDelay
    pure next
  Defocus ev next → do
    st ← H.get
    isFrame ← H.fromEff $ elementEq ev.target ev.currentTarget
    when (st.focused && isFrame) $
      for_ (L.last opts.cursor) \rootId → do
        { bus } ← liftH' Wiring.expose
        H.fromAff $ Bus.write (DeckFocused rootId) bus.decks
    H.modify (DCS._presentAccessNextActionCardGuide .~ false)
    pure next
  HandleEval msg next →
    handleEval msg $> next
  HandleMessage msg next → do
    st ← H.get
    case msg of
      DeckFocused focusedDeckId → do
        when (st.id ≡ focusedDeckId && not st.focused) $
          H.modify (DCS._focused .~ true)
        when (st.id ≠ focusedDeckId && st.focused) $
          H.modify (DCS._focused .~ false)
    pure next
  HandleError ge next → do
    showDialog $ Dialog.Error $ GE.print ge
    pure next
  DismissedCardGuide next → do
    when (L.null opts.cursor) $ void do
      queryNextAction (Next.PresentAddCardGuide unit)
    pure next
  GetActiveCoord k → do
    active ← H.gets DCS.activeCard
    pure (k (censor ∘ map _.coord =<< active))
  SignIn providerR next → do
    { auth } ← liftH' Wiring.expose
    idToken ← H.fromAff AVar.makeVar
    H.fromAff $ Bus.write { providerR, idToken, prompt: true, keySuffix } auth.requestToken
    either signInFailure (const $ signInSuccess) =<< (H.fromAff $ AVar.takeVar idToken)
    pure next
  where
  keySuffix = AuthenticationMode.toKeySuffix AuthenticationMode.ChosenProvider

  signInSuccess = do
    { auth } ← liftH' Wiring.expose
    H.fromAff $ Bus.write SignInSuccess $ auth.signIn
    H.fromEff Browser.reload

  signInFailure error = do
    { auth, bus } ← liftH' Wiring.expose
    H.fromAff do
      for_ (Authentication.toNotificationOptions error) $
        flip Bus.write bus.notify
      Bus.write SignInFailure auth.signIn

  getBoundingClientWidth =
    H.fromEff ∘ map _.width ∘ getBoundingClientRect

peek ∷ ∀ a. DeckOptions → H.ChildF ChildSlot ChildQuery a → DeckDSL Unit
peek opts (H.ChildF s q) =
  (peekCards ⊹ (\_ _ → pure unit) $ s)
   ⨁ peekBackSide opts
   ⨁ (peekDialog opts ⨁ (const $ pure unit))
   ⨁ peekNextAction
   ⨁ (const $ pure unit)
   ⨁ (const $ pure unit)
   $ q

peekDialog ∷ ∀ a. DeckOptions → Dialog.Query a → DeckDSL Unit
peekDialog opts = case _ of
  Dialog.Show _ _ → do
    H.modify (DCS._displayMode .~ DCS.Dialog)
  Dialog.Dismiss _ →
    H.modify (DCS._displayMode .~ DCS.Backside)
  Dialog.FlipToFront _ →
    H.modify (DCS._displayMode .~ DCS.Normal)
  Dialog.SetDeckName name _ → do
    deckId ← H.gets _.id
    H.modify (DCS._displayMode .~ DCS.Normal)
    void $ liftH' $ P.renameDeck deckId name
  Dialog.Confirm d b _ → do
    H.modify (DCS._displayMode .~ DCS.Backside)
    case d of
      Dialog.DeleteDeck | b → deleteDeck opts
      _ → pure unit

peekBackSide ∷ ∀ a. DeckOptions → Back.Query a → DeckDSL Unit
peekBackSide opts (Back.DoAction action _) = do
  { path } ← liftH' Wiring.expose
  st ← H.get
  let
    isRoot = L.null opts.cursor
  case action of
    Back.Trash → do
      let
        active = DCS.activeCard st
      for_ (join $ censor <$> active) \{ coord } → do
        liftH' $ P.removeCard st.id coord
        H.modify
          $ (DCS._displayMode .~ DCS.Normal)
          ∘ (DCS._presentAccessNextActionCardGuide .~ false)
      void $ H.queryAll' cpCard $ left $ H.action UpdateDimensions
    Back.Rename → do
      showDialog $ Dialog.Rename st.name
    Back.Share → do
      getDeckTree st.id >>= traverse_
        (showDialog ∘ Dialog.Share ∘ ET.getSharingInput path)
    Back.Unshare → do
      getDeckTree st.id >>= traverse_
        (showDialog ∘ Dialog.Unshare ∘ ET.getSharingInput path)
    Back.Embed → do
      getDeckTree st.id >>= traverse_ \tree →
        showDialog $ Dialog.Embed
          (ET.getSharingInput path tree)
          (ET.getVarMaps tree)
    Back.Publish → do
      getDeckTree st.id >>= traverse_ \tree →
        showDialog $ Dialog.Publish
          (ET.getSharingInput path tree)
          (ET.getVarMaps tree)
    Back.DeleteDeck →
      if Array.length st.displayCards <= 1
        then deleteDeck opts
        else showDialog Dialog.DeleteDeck
    Back.Mirror →
      case st.parent of
        Just coord | not isRoot → do
          res ← liftH' $ P.mirrorDeck st.id coord
          for_ res relocateDisplayCards
          H.modify (DCS._displayMode .~ DCS.Normal)
        _ → do
          res ← liftH' $ P.wrapAndMirrorDeck st.id
          for_ res navigateToDeck
    Back.Wrap → do
      res ← liftH' $ P.wrapDeck st.id
      if isRoot
        then for_ res navigateToDeck
        else H.modify $ DCS._displayMode .~ DCS.Normal
    Back.Unwrap → do
      case st.parent of
        Just coord | not isRoot → do
          void $ liftH' $ P.collapseDeck st.id coord
        _ → do
          res ← liftH' $ P.unwrapDeck st.id
          for_ res navigateToDeck
peekBackSide _ _ = pure unit

peekCards ∷ ∀ a. DeckId × CardId → CardQueryP a → DeckDSL Unit
peekCards cardId = const (pure unit) ⨁ peekCardInner cardId

showDialog ∷ Dialog.Dialog → DeckDSL Unit
showDialog dlg = do
  queryDialog $ H.action $ Dialog.Show dlg
  H.modify (DCS._displayMode .~ DCS.Dialog)

queryDialog ∷ Dialog.Query Unit → DeckDSL Unit
queryDialog = void ∘ H.query' cpDialog unit ∘ left

queryCard ∷ ∀ a. DeckId × CardId → CQ.AnyCardQuery a → DeckDSL (Maybe a)
queryCard cid =
  H.query' cpCard cid
    ∘ right
    ∘ H.ChildF unit
    ∘ right

queryCardEval ∷ ∀ a. DeckId × CardId → CQ.CardQuery a → DeckDSL (Maybe a)
queryCardEval cid =
  H.query' cpCard cid ∘ left

queryNextAction ∷ ∀ a. Next.Query a → DeckDSL (Maybe a)
queryNextAction =
  H.query' cpNext unit ∘ left

updateActiveState ∷ DeckDSL Unit
updateActiveState = do
  st ← H.get
  { cache } ← liftH' Wiring.expose
  for_ st.activeCardIndex \cardIndex → do
    liftH' $ Cache.put st.id { cardIndex } cache.activeState
    for_ (DCS.cardCoordFromIndex cardIndex st) \coord →
      void $ queryCardEval coord $ H.action CQ.ActivateCard

updateBackSide ∷ DeckOptions → DeckDSL Unit
updateBackSide { cursor } = do
  st ← H.get
  let
    ty = join (censor <$> DCS.activeCard st)
    tys = Array.mapMaybe censor st.displayCards
  void $ H.query' cpBackSide unit $ H.action $ Back.UpdateCard ty tys

dismissedAccessNextActionCardGuideKey ∷ String
dismissedAccessNextActionCardGuideKey = "dismissedAccessNextActionCardGuide"

getDismissedAccessNextActionCardGuideBefore ∷ DeckDSL Boolean
getDismissedAccessNextActionCardGuideBefore =
  liftH'
    $ either (const $ false) id
    <$> LocalStorage.getLocalStorage dismissedAccessNextActionCardGuideKey

storeDismissedAccessNextActionCardGuide ∷ DeckDSL Unit
storeDismissedAccessNextActionCardGuide =
  liftH' $ LocalStorage.setLocalStorage dismissedAccessNextActionCardGuideKey true

presentAccessNextActionCardGuideAfterDelay ∷ DeckDSL Unit
presentAccessNextActionCardGuideAfterDelay = do
  dismissedBefore ← getDismissedAccessNextActionCardGuideBefore
  focused ← H.gets _.focused
  when (not dismissedBefore && focused) do
    cancelPresentAccessNextActionCardGuide
    canceler ← sendAfter' Config.addCardGuideDelay $ PresentAccessNextActionCardGuide unit
    H.modify $ DCS._presentAccessNextActionCardGuideCanceler .~ Just canceler

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
  whenM cancelPresentAccessNextActionCardGuide
    presentAccessNextActionCardGuideAfterDelay

peekCardInner
  ∷ ∀ a
  . DeckId × CardId
  → H.ChildF Unit InnerCardQuery a
  → DeckDSL Unit
peekCardInner cardCoord = H.runChildF ⋙
  (peekCardEvalQuery cardCoord ⨁ peekAnyCard cardCoord)

peekCardEvalQuery ∷ ∀ a. DeckId × CardId → CEQ.CardEvalQuery a → DeckDSL Unit
peekCardEvalQuery cardCoord = case _ of
  CEQ.ZoomIn _ → raise' $ H.action ZoomIn
  _ → pure unit

peekAnyCard ∷ ∀ a. DeckId × CardId → AnyCardQuery a → DeckDSL Unit
peekAnyCard cardCoord _ =
  resetAccessNextActionCardGuideDelay

peekNextAction ∷ ∀ a. Next.QueryP a → DeckDSL Unit
peekNextAction q = do
  deckId ← H.gets _.id
  for_ (q ^? CoP._Left ∘ Next._AddCardType) $ void ∘ H.liftH ∘ H.liftH ∘ P.addCard deckId
  for_ (q ^? CoP._Left ∘ Next._PresentReason) $ uncurry presentReason

presentReason ∷ Port.Port → CT.CardType → DeckDSL Unit
presentReason input cardType =
  showDialog dialog
  where
  insertableCardType = ICT.fromCardType cardType
  ioType = ICT.fromPort input
  reason = ICT.reason ioType cardType
  cardPaths = ICT.cardPathsBetween ioType insertableCardType
  dialog = Dialog.Reason cardType reason cardPaths

deleteDeck ∷ DeckOptions → DeckDSL Unit
deleteDeck opts = do
  st ← H.get
  res ← liftH' $ P.deleteDeck st.id
  for_ res (maybe navigateToIndex navigateToDeck)

loadDeck ∷ DeckOptions → DeckDSL Unit
loadDeck opts = do
  st ← H.get
  { bus, value } ← liftH' $ P.getDeck' st.id
  breaker ← subscribeToBus' (H.action ∘ HandleEval) bus
  H.modify \s → s
    { stateMode = Ready
    , displayCards = [ Left DCS.PendingCard ]
    , breakers = Array.cons breaker s.breakers
    }
  liftH' (Future.wait value) >>= case _ of
    Left err → do
      providers ←
        Quasar.retrieveAuthProviders <#> case _ of
          Right (Just providers) → providers
          _ → []
      H.modify
        $ (DCS._stateMode .~ Error err)
        ∘ (DCS._providers .~ providers)
    Right deck →
      loadCards opts deck

loadCards ∷ DeckOptions → Model.Deck → DeckDSL Unit
loadCards opts deck = do
  st ← H.get
  let
    coords = Model.cardCoords st.id deck
  case Array.head coords of
    Nothing → setEmptyDeck
    Just coord → do
      evalCells ← liftH' $ P.getEvaluatedCards coords
      case evalCells of
        Just (cells × port) → setEvaluatedDeck st cells port
        Nothing → setUnevaluatedDeck st coord
  where
  setEmptyDeck = do
    H.modify
      $ (DCS._activeCardIndex .~ Just 0)
      ∘ DCS.fromModel
          { name: deck.name
          , parent: deck.parent
          , displayCards: [ Left (DCS.NextActionCard Port.Initial) ]
          }
    updateCardSize

  setUnevaluatedDeck st initialCoord = do
    active ← activeCardIndex st
    H.modify
      $ (DCS._activeCardIndex .~ active)
      ∘ DCS.fromModel
          { name: deck.name
          , parent: deck.parent
          , displayCards: [ Left DCS.PendingCard ]
          }
    liftH' $
      P.queueEval 0 (st.id L.: opts.cursor × initialCoord)
    updateCardSize

  setEvaluatedDeck st cells port = do
    active ← activeCardIndex st
    queryNextAction $ H.action $ Next.UpdateInput port
    H.modify
      $ (DCS.updateDisplayCards (mkDisplayCard <$> cells) port)
      ∘ (DCS._parent .~ deck.parent)
      ∘ (DCS._activeCardIndex .~ active)
    updateCardSize
    updateActiveState

  activeCardIndex st = do
    { cache } ← liftH' Wiring.expose
    map _.cardIndex <$> H.liftH (H.liftH (Cache.get st.id cache.activeState))

handleEval ∷ ED.EvalMessage → DeckDSL Unit
handleEval = case _ of
  ED.Pending coord → do
    st ← H.get
    H.modify
      $ (DCS._pendingCardIndex .~ DCS.cardIndexFromCoord coord st)
      ∘ (DCS.addMetaCard DCS.PendingCard)
  ED.Complete coords port → do
    deckId ← H.gets _.id
    mbCells ← liftH' $ P.getCards coords
    for_ (map mkDisplayCard <$> mbCells) \displayCards → do
      queryNextAction $ H.action $ Next.UpdateInput port
      H.modify (DCS.updateDisplayCards displayCards port)
      updateCardSize
      updateActiveState
  ED.NameChange name → do
    H.modify _ { name = name }
  ED.ParentChange parent → do
    H.modify _ { parent = parent }
  _ →
    pure unit

mkDisplayCard ∷ DeckId × EC.Cell → DCS.CardDef
mkDisplayCard (deckId × c) =
  { coord: deckId × c.value.model.cardId
  , cardType: Card.modelCardType c.value.model.model
  }

relocateDisplayCards ∷ DeckId → DeckDSL Unit
relocateDisplayCards newDeckId = do
  st ← H.get
  displayCards ←
    sequence <$> for st.displayCards case _ of
      Right { coord } | fst coord ≡ st.id → runMaybeT do
        card ← MaybeT $ liftH' $ P.getCard (newDeckId × snd coord)
        pure (Right (mkDisplayCard (newDeckId × card)))
      dc → pure (Just dc)
  for_ displayCards \dc →
    H.modify _ { displayCards = dc }

getDeckTree ∷ DeckId → DeckDSL (Maybe ET.TraverseDeck)
getDeckTree deckId = do
  wiring ← liftH' Wiring.expose
  cards ← liftH' $ Cache.snapshot wiring.eval.cards
  decks ← liftH' $ Cache.snapshot wiring.eval.decks
  pure (ET.unfoldTree cards decks deckId)

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

presentFlipGuideFirstTime ∷ DeckDSL Unit
presentFlipGuideFirstTime = do
  H.gets _.displayMode >>=
    case _ of
      DCS.Backside → do
        { bus } ← liftH' Wiring.expose
        shouldPresentFlipGuide >>=
          if _
          then H.fromAff $ Bus.write Wiring.FlipGuide bus.stepByStep
          else pure unit
      _ → pure unit

shouldPresentFlipGuide ∷ DeckDSL Boolean
shouldPresentFlipGuide =
  liftH' $
    either (const true) not <$> LocalStorage.getLocalStorage Guide.dismissedFlipGuideKey

navigateToDeck ∷ DeckId → DeckDSL Unit
navigateToDeck newId = do
  { path, accessType, varMaps } ← liftH' Wiring.expose
  urlVarMaps ← liftH' $ Cache.snapshot varMaps
  navigate $ WorkspaceRoute path (Just newId) (WA.Load accessType) urlVarMaps

navigateToIndex ∷ DeckDSL Unit
navigateToIndex = do
  { path } ← liftH' Wiring.expose
  void $ H.fromEff $ Browser.setHref $ parentURL $ Left path
