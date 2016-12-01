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

module SlamData.Workspace.Component
  ( comp
  , module SlamData.Workspace.Component.State
  , module SlamData.Workspace.Component.Query
  ) where

import SlamData.Prelude

import Control.Monad.Aff as Aff
import Control.Monad.Aff.AVar (putVar)
import Control.Monad.Aff.Free (class Affable)
import Control.Monad.Eff.Exception as Exn
import Control.Monad.Fork (class MonadFork)
import Control.UI.Browser (setHref, locationObject)

import Data.Array as Array
import Data.Lens ((^.), (.~), (?~))
import Data.List as List
import Data.Path.Pathy ((</>))
import Data.Path.Pathy as Pathy
import Data.String as Str
import Data.Time.Duration (Milliseconds(..))

import DOM.HTML.Location as Location

import Halogen as H
import Halogen.Component.ChildPath (injSlot, injQuery)
import Halogen.Component.Opaque.Unsafe (opaqueState)
import Halogen.Component.Utils (subscribeToBus')
import Halogen.Component.Utils.Throttled (throttledEventSource_)
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP

import SlamData.Analytics as SA
import SlamData.Effects (SlamDataEffects)
import SlamData.FileSystem.Routing (parentURL)
import SlamData.GlobalError as GE
import SlamData.GlobalMenu.Component as GlobalMenu
import SlamData.Guide as Guide
import SlamData.Header.Component as Header
import SlamData.Header.Gripper.Component as Gripper
import SlamData.Monad (Slam)
import SlamData.Notification as N
import SlamData.Notification.Component as NC
import SlamData.Quasar.Class (class QuasarDSL)
import SlamData.Quasar.Data as Quasar
import SlamData.Quasar.Error as QE
import SlamData.Wiring (Wiring(..), putDeck, getDeck)
import SlamData.Wiring as Wiring
import SlamData.Workspace.AccessType as AT
import SlamData.Workspace.Action as WA
import SlamData.Workspace.Card.CardId as CID
import SlamData.Workspace.Card.Draftboard.Common as DBC
import SlamData.Workspace.Card.Draftboard.Orientation as Orn
import SlamData.Workspace.Card.Draftboard.Pane as Pane
import SlamData.Workspace.Class (class WorkspaceDSL, putURLVarMaps, getURLVarMaps)
import SlamData.Workspace.Component.ChildSlot (ChildQuery, ChildSlot, ChildState, cpDeck, cpHeader, cpNotify)
import SlamData.Workspace.Component.Query (QueryP, Query(..), fromWorkspace, fromDeck, toWorkspace, toDeck)
import SlamData.Workspace.Component.State (State, _accessType, _initialDeckId, _loaded, _version, _stateMode, _flipGuideStep, _cardGuideStep, initialState)
import SlamData.Workspace.Component.State as State
import SlamData.Workspace.Deck.Common (wrappedDeck, splitDeck)
import SlamData.Workspace.Deck.Component as Deck
import SlamData.Workspace.Deck.Component.Nested as DN
import SlamData.Workspace.Deck.DeckId (DeckId, freshDeckId)
import SlamData.Workspace.Model as Model
import SlamData.Workspace.Notification as Notify
import SlamData.Workspace.Routing (mkWorkspaceHash)
import SlamData.Workspace.StateMode (StateMode(..))
import SlamData.Workspace.Deck.Component.Render (renderError)

import Utils.DOM (onResize, elementEq)
import Utils.LocalStorage as LocalStorage

type StateP = H.ParentState State ChildState Query ChildQuery Slam ChildSlot
type WorkspaceHTML = H.ParentHTML ChildState Query ChildQuery Slam ChildSlot
type WorkspaceDSL = H.ParentDSL State ChildState Query ChildQuery Slam ChildSlot

comp ∷ H.Component StateP QueryP Slam
comp =
  H.lifecycleParentComponent
    { render
    , eval
    , peek: Just (peek ∘ H.runChildF)
    , initializer: Just $ Init unit
    , finalizer: Nothing
    }

render ∷ State → WorkspaceHTML
render state =
  HH.div
    [ HP.classes
        $ (guard (AT.isReadOnly (state ^. _accessType)) $> HH.className "sd-published")
        ⊕ [ HH.className "sd-workspace" ]
    , HE.onClick (HE.input DismissAll)
    ]
    (preloadGuides ⊕ header ⊕ deck ⊕ notifications ⊕ renderCardGuide ⊕ renderFlipGuide)
  where
  renderCardGuide ∷ Array WorkspaceHTML
  renderCardGuide =
    Guide.renderStepByStepWithArray
      { next: CardGuideStepNext, dismiss: CardGuideDismiss }
      state.cardGuideStep
      Guide.cardGuideSteps

  renderFlipGuide ∷ Array WorkspaceHTML
  renderFlipGuide =
    Guide.renderStepByStepWithArray
      { next: FlipGuideStepNext, dismiss: FlipGuideDismiss }
      state.flipGuideStep
      Guide.flipGuideSteps

  preloadGuides =
    Guide.preloadStepByStepWithArray
      <$> [ Guide.cardGuideSteps, Guide.flipGuideSteps ]

  notifications ∷ Array WorkspaceHTML
  notifications =
    pure $ HH.slot' cpNotify unit \_ →
      { component: NC.comp
      , initialState: NC.initialState
      }

  header ∷ Array WorkspaceHTML
  header = do
    guard $ AT.isEditable (state ^. _accessType)
    pure $ HH.slot' cpHeader unit \_ →
      { component: Header.comp
      , initialState: H.parentState Header.initialState
      }

  deck ∷ Array WorkspaceHTML
  deck =
    pure case state.stateMode, state.initialDeckId of
      Loading, _ →
        HH.div_ []
      Error error, _ → renderError error
      _, Just deckId →
        HH.slot' cpDeck unit \_ →
          let init = opaqueState $ Deck.initialDeck deckId
          in { component: DN.comp (deckOpts deckId) init
             , initialState: DN.initialState
             }
      _, _ → renderError $ QE.Error $ Exn.error "Missing deck id (impossible!)"

  deckOpts deckId =
    { accessType: state.accessType
    , cursor: List.Nil
    }

eval ∷ Query ~> WorkspaceDSL
eval (Init next) = do
  deckId ← H.fromEff freshDeckId
  cardGuideStep ← initialCardGuideStep
  H.modify (_initialDeckId ?~ deckId)
  H.subscribe'
    $ throttledEventSource_ (Milliseconds 100.0) onResize
    $ pure (H.action Resize)
  H.modify $ (_cardGuideStep .~ cardGuideStep)
  Wiring wiring ← H.liftH $ H.liftH ask
  subscribeToBus' (H.action ∘ PresentStepByStepGuide) wiring.presentStepByStepGuide
  -- The deck component isn't initialised before this later has completed
  H.fromAff $ Aff.later (pure unit :: Aff.Aff SlamDataEffects Unit)
  when (isNothing cardGuideStep) (void $ queryDeck $ H.action Deck.DismissedCardGuide)
  pure next
eval (PresentStepByStepGuide stepByStepGuide next) =
  case stepByStepGuide of
    Wiring.CardGuide → H.modify (_cardGuideStep .~ Just 0) $> next
    Wiring.FlipGuide → H.modify (_flipGuideStep .~ Just 0) $> next
eval (CardGuideStepNext next) = H.modify State.cardGuideStepNext $> next
eval (CardGuideDismiss next) = do
  H.liftH $ H.liftH $ LocalStorage.setLocalStorage Guide.dismissedCardGuideKey true
  H.modify (_cardGuideStep .~ Nothing)
  queryDeck $ H.action Deck.DismissedCardGuide
  pure next
eval (FlipGuideStepNext next) = H.modify State.flipGuideStepNext $> next
eval (FlipGuideDismiss next) = do
  H.liftH $ H.liftH $ LocalStorage.setLocalStorage Guide.dismissedFlipGuideKey true
  H.modify (_flipGuideStep .~ Nothing)
  pure next
eval (SetVarMaps urlVarMaps next) = do
  putURLVarMaps urlVarMaps
  pure next
eval (DismissAll ev next) = do
  querySignIn $ H.action GlobalMenu.DismissSubmenu
  eq ← H.fromEff $ elementEq ev.target ev.currentTarget
  when eq $ void $ queryDeck $ H.action Deck.Focus
  pure next
eval (Resize next) = do
  queryDeck $ H.action $ Deck.UpdateCardSize
  pure next
eval (Reset next) = do
  Wiring wiring ← H.liftH $ H.liftH ask
  H.modify _
    { stateMode = Ready
    , accessType = AT.Editable
    }
  queryDeck $ H.action Deck.Reset
  queryDeck $ H.action Deck.Focus
  pure next
eval (Load deckId accessType next) = do
  oldAccessType ← H.gets _.accessType
  H.modify (_accessType .~ accessType)
  case accessType of
    AT.Editable → pure unit
    _ → H.modify (_cardGuideStep .~ Nothing)
  queryNotifications
    $ H.action
    $ NC.UpdateRenderMode
    $ NC.renderModeFromAccessType accessType

  queryDeck (H.request Deck.GetId) >>= \deckId' → do
    case deckId, deckId' of
      Just a, Just b
        | a ≡ b ∧ oldAccessType ≡ accessType → run
      _, _ → load

  pure next

  where
  run = do
    void $ queryDeck $ H.action $ Deck.Run

  load = do
    H.modify _ { stateMode = Loading }
    queryDeck $ H.action Deck.Reset
    maybe loadRoot loadDeck deckId
    void $ queryDeck $ H.action $ Deck.Focus

  loadDeck deckId' = void do
    SA.track (SA.Load deckId' accessType)
    H.modify (_stateMode .~ Ready)
    queryDeck $ H.action $ Deck.Load deckId'

  loadRoot =
    rootDeck >>= either handleError loadDeck

  handleError err = case GE.fromQError err of
    Nothing → H.modify $ _stateMode .~ Error err
    Just ge → GE.raiseGlobalError ge

rootDeck ∷ WorkspaceDSL (Either QE.QError DeckId)
rootDeck = do
  Wiring wiring ← H.liftH $ H.liftH ask
  Model.getRoot (wiring.path </> Pathy.file "index")

peek ∷ ∀ a. ChildQuery a → WorkspaceDSL Unit
peek = ((const $ pure unit) ⨁ peekDeck) ⨁ ((const $ pure unit) ⨁ peekNotification)
  where
  peekNotification ∷ NC.Query a → WorkspaceDSL Unit
  peekNotification =
    case _ of
      NC.Action N.ExpandGlobalMenu _ → do
        queryHeaderGripper $ Gripper.StartDragging 0.0 unit
        queryHeaderGripper $ Gripper.StopDragging unit
      NC.Action (N.Fulfill var) _ →
        void $ H.fromAff $ Aff.attempt $ putVar var unit
      _ → pure unit

  peekDeck :: Deck.Query a → WorkspaceDSL Unit
  peekDeck (Deck.DoAction (Deck.Unwrap decks) _) = void $ runMaybeT do
    state  ← lift H.get
    oldId  ← MaybeT $ queryDeck $ H.request Deck.GetId
    parent ← lift $ join <$> queryDeck (H.request Deck.GetParent)
    newId × deck ← MaybeT $ pure $ List.head $ List.catMaybes $ Pane.toList decks

    let deck' = deck { parent = parent }

    error ← lift $ H.liftH $ H.liftH $ runExceptT do
      req1 ← ExceptT $ putDeck newId deck'
      updateParentPointer oldId newId parent

    case error of
      Left err →
        case GE.fromQError err of
          Nothing →
            Notify.error
              "Failed to collapse deck."
              (Just $ N.Details $ QE.printQError err)
              Nothing
              Nothing
          Just ge →
            GE.raiseGlobalError ge
      Right _  → do
        SA.track (SA.Collapse oldId)
        lift $ H.liftH $ H.liftH $ updateHash state.accessType newId

  peekDeck (Deck.DoAction Deck.Wrap _) = void $ runMaybeT do
    state ← lift H.get
    deck  ← MaybeT $ queryDeck (H.request Deck.GetModel)
    oldId ← MaybeT $ queryDeck (H.request Deck.GetId)
    newId ← lift $ H.fromEff freshDeckId

    let
      deck' = deck { parent = Just (newId × CID.CardId 0) }
      wrapper = (wrappedDeck oldId) { parent = deck.parent }

    error ← lift $ H.liftH $ H.liftH $ runExceptT do
      ExceptT $ map (errors "; ") $ parTraverse id
        [ putDeck oldId deck'
        , putDeck newId wrapper
        ]
      updateParentPointer oldId newId deck.parent

    case error of
      Left err →
        case GE.fromQError err of
          Nothing →
            Notify.error
              "Failed to wrap deck."
              (Just $ N.Details $ QE.printQError err)
              Nothing
              Nothing
          Just ge →
            GE.raiseGlobalError ge
      Right _  → do
        SA.track (SA.Wrap oldId)
        lift $ H.liftH $ H.liftH $ updateHash state.accessType newId

  peekDeck (Deck.DoAction Deck.DeleteDeck _) = void $ runMaybeT do
    state  ← lift H.get
    oldId  ← MaybeT $ queryDeck (H.request Deck.GetId)
    parent ← lift $ join <$> queryDeck (H.request Deck.GetParent)
    error  ← lift $ H.liftH $ H.liftH $ runExceptT do
      case parent of
        Just (deckId × cardId) → do
          parentDeck ← ExceptT $ getDeck deckId
          let cards = DBC.replacePointer oldId Nothing cardId parentDeck.cards
          lift $ for_ cards (DBC.unsafeUpdateCachedDraftboard deckId)
          ExceptT $ putDeck deckId (parentDeck { cards = cards })
        Nothing → do
          Wiring wiring ← lift ask
          ExceptT $ Quasar.delete $ Left wiring.path

    case error of
      Left err →
        Notify.deleteDeckFail err
      Right _  → do
        SA.track (SA.Delete oldId)
        case parent of
          Just (deckId × _) →
            lift $ H.liftH $ H.liftH $ updateHash state.accessType deckId
          Nothing → do
            Wiring wiring ← lift $ H.liftH $ H.liftH ask
            void $ H.fromEff $ setHref $ parentURL $ Left wiring.path

  peekDeck (Deck.DoAction Deck.Mirror _) = void $ runMaybeT do
    pure unit
    state ← lift H.get
    newIdShared ← lift $ H.fromEff freshDeckId
    newIdMirror ← lift $ H.fromEff freshDeckId
    newIdParent ← lift $ H.fromEff freshDeckId
    oldId ← MaybeT $ queryDeck (H.request Deck.GetId)
    oldModel ← MaybeT $ queryDeck (H.request Deck.GetModel)
    let
      freshCard = CID.CardId 0
      parentRef = Just (newIdParent × freshCard)
      wrappedDeck =
        (splitDeck Orn.Vertical (List.fromFoldable [ oldId, newIdMirror ]))
          { parent = oldModel.parent }

    error ← lift $ H.liftH $ H.liftH $ runExceptT do
      ExceptT $ map (errors "; ") $ parTraverse id
        if Array.null oldModel.cards
        then
          let
            mirrored = oldModel { parent = parentRef }
          in
            [ putDeck oldId mirrored
            , putDeck newIdMirror mirrored
            ]
        else
          let
            mirrored = oldModel
              { parent = parentRef
              , mirror = oldModel.mirror <> map (Tuple newIdShared ∘ _.cardId) oldModel.cards
              , cards = []
              , name = oldModel.name
              }
          in
            [ putDeck oldId mirrored
            , putDeck newIdMirror (mirrored { name = "" })
            , putDeck newIdShared (oldModel { name = "" })
            ]
      ExceptT $ putDeck newIdParent wrappedDeck
      updateParentPointer oldId newIdParent oldModel.parent

    case error of
      Left err →
        case GE.fromQError err of
          Nothing →
            Notify.error
              "Failed to mirror deck."
              (Just $ N.Details $ QE.printQError err)
              Nothing
              Nothing
          Just ge →
            GE.raiseGlobalError ge
      Right _  → do
        SA.track (SA.Mirror oldId)
        lift $ H.liftH $ H.liftH $ updateHash state.accessType newIdParent

  peekDeck _ = pure unit

queryNotifications ∷ ∀ a. NC.Query a → WorkspaceDSL (Maybe a)
queryNotifications = H.query' cpNotify unit

queryDeck ∷ ∀ a. Deck.Query a → WorkspaceDSL (Maybe a)
queryDeck = H.query' cpDeck unit ∘ right

queryHeaderGripper ∷ ∀ a. Gripper.Query a → WorkspaceDSL Unit
queryHeaderGripper =
  void
    ∘ H.query' cpHeader unit
    ∘ right
    ∘ H.ChildF (injSlot Header.cpGripper unit)
    ∘ injQuery Header.cpGripper

querySignIn ∷ ∀ a. GlobalMenu.Query a → WorkspaceDSL Unit
querySignIn =
  void
    ∘ H.query' cpHeader unit
    ∘ right
    ∘ H.ChildF (injSlot Header.cpGlobalMenu unit)
    ∘ injQuery Header.cpGlobalMenu
    ∘ left

lefts ∷ ∀ a b. Array (Either a b) → Array a
lefts = Array.mapMaybe fromLeft

fromLeft ∷ ∀ a b. Either a b → Maybe a
fromLeft = either Just (const Nothing)

errors ∷ ∀ a. String → Array (Either QE.QError a) → Either QE.QError Unit
errors m es = case lefts es of
  [] → Right unit
  ss →
    case sequence $ map (\e → maybe (Right $ QE.printQError e) Left $ GE.fromQError e) ss of
      Left ge → Left $ GE.toQError ge
      Right msgs → Left $ QE.Error $ Exn.error (Str.joinWith m msgs)

updateParentPointer
  ∷ ∀ m e
  . (MonadFork e m, QuasarDSL m, Affable SlamDataEffects m, MonadAsk Wiring m)
  ⇒ DeckId
  → DeckId
  → Maybe (DeckId × CID.CardId)
  → ExceptT QE.QError m Unit
updateParentPointer oldId newId = case _ of
  Just (deckId × cardId) → do
    parentDeck ← ExceptT $ getDeck deckId
    let cards = DBC.replacePointer oldId (Just newId) cardId parentDeck.cards
    lift $ for_ cards (DBC.unsafeUpdateCachedDraftboard deckId)
    ExceptT $ putDeck deckId (parentDeck { cards = cards })
  Nothing → do
    Wiring wiring ← lift ask
    ExceptT $ Model.setRoot (wiring.path </> Pathy.file "index") newId

updateHash
  ∷ ∀ m
  . (Bind m, Affable SlamDataEffects m, WorkspaceDSL m, MonadAsk Wiring m)
  ⇒ AT.AccessType
  → DeckId
  → m Unit
updateHash accessType newId = do
  Wiring wiring ← ask
  varMaps ← getURLVarMaps
  H.fromEff do
    let deckHash = mkWorkspaceHash (Deck.deckPath' wiring.path newId) (WA.Load accessType) varMaps
    locationObject >>= Location.setHash deckHash

initialCardGuideStep ∷ WorkspaceDSL (Maybe Int)
initialCardGuideStep =
  H.liftH $ H.liftH
    $ either (const $ Just 0) (if _ then Nothing else Just 0)
    <$> LocalStorage.getLocalStorage Guide.dismissedCardGuideKey
