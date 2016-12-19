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
import Control.Monad.Aff.Bus as Bus
import Control.Monad.Eff.Exception as Exn

import Data.Lens ((.~))
import Data.List as List
import Data.Path.Pathy ((</>))
import Data.Path.Pathy as Pathy
import Data.Time.Duration (Milliseconds(..))

import Halogen as H
import Halogen.Component.ChildPath (injSlot, injQuery)
import Halogen.Component.Opaque.Unsafe (opaqueState)
import Halogen.Component.Utils (liftH', subscribeToBus')
import Halogen.Component.Utils.Throttled (throttledEventSource_)
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP

import SlamData.FileSystem.Resource as R
import SlamData.GlobalError as GE
import SlamData.GlobalMenu.Component as GlobalMenu
import SlamData.Guide as Guide
import SlamData.Header.Component as Header
import SlamData.Header.Gripper.Component as Gripper
import SlamData.Monad (Slam)
import SlamData.Notification as N
import SlamData.Notification.Component as NC
import SlamData.Quasar.Error as QE
import SlamData.Wiring as Wiring
import SlamData.Wiring.Cache as Cache
import SlamData.Workspace.AccessType as AT
import SlamData.Workspace.Action as WA
import SlamData.Workspace.Card.Model as CM
import SlamData.Workspace.Card.Table.Model as JT
import SlamData.Workspace.Class (navigate, Routes(..))
import SlamData.Workspace.Component.ChildSlot (ChildQuery, ChildSlot, ChildState, cpDeck, cpHeader, cpNotify)
import SlamData.Workspace.Component.Query (QueryP, Query(..), fromWorkspace, toWorkspace)
import SlamData.Workspace.Component.State (State, _stateMode, _flipGuideStep, _cardGuideStep, initialState)
import SlamData.Workspace.Component.State as State
import SlamData.Workspace.Deck.Component as Deck
import SlamData.Workspace.Deck.Component.Nested as DN
import SlamData.Workspace.Deck.DeckId (DeckId)
import SlamData.Workspace.Eval.Deck as ED
import SlamData.Workspace.Eval.Persistence as P
import SlamData.Workspace.Model as Model
import SlamData.Workspace.StateMode (StateMode(..))
import SlamData.Workspace.Deck.Component.Render (renderError)

import Utils.DOM (onResize, elementEq)
import Utils.LocalStorage as LocalStorage

type StateP = H.ParentState State ChildState Query ChildQuery Slam ChildSlot
type WorkspaceHTML = H.ParentHTML ChildState Query ChildQuery Slam ChildSlot
type WorkspaceDSL = H.ParentDSL State ChildState Query ChildQuery Slam ChildSlot

comp ∷ AT.AccessType → H.Component StateP QueryP Slam
comp accessType =
  H.lifecycleParentComponent
    { render: render accessType
    , eval
    , peek: Just (peek ∘ H.runChildF)
    , initializer: Just $ Init unit
    , finalizer: Nothing
    }

render ∷ AT.AccessType → State → WorkspaceHTML
render accessType state =
  HH.div
    [ HP.classes
        $ (guard (AT.isReadOnly accessType) $> HH.className "sd-published")
        ⊕ [ HH.className "sd-workspace" ]
    , HE.onClick (HE.input DismissAll)
    ]
    (preloadGuides ⊕ header ⊕ deck ⊕ notifications ⊕ renderCardGuide ⊕ renderFlipGuide)
  where
  renderCardGuide =
    Guide.renderStepByStepWithArray
      { next: CardGuideStepNext, dismiss: CardGuideDismiss }
      state.cardGuideStep
      Guide.cardGuideSteps

  renderFlipGuide =
    Guide.renderStepByStepWithArray
      { next: FlipGuideStepNext, dismiss: FlipGuideDismiss }
      state.flipGuideStep
      Guide.flipGuideSteps

  preloadGuides =
    Guide.preloadStepByStepWithArray
      <$> [ Guide.cardGuideSteps, Guide.flipGuideSteps ]

  notifications =
    pure $ HH.slot' cpNotify unit \_ →
      { component: NC.comp
      , initialState: NC.initialState (NC.renderModeFromAccessType accessType)
      }

  header = do
    guard $ AT.isEditable accessType
    pure $ HH.slot' cpHeader unit \_ →
      { component: Header.comp
      , initialState: H.parentState Header.initialState
      }

  deck =
    pure case state.stateMode, state.deckId of
      Loading, _ →
        HH.div_ []
      Error error, _ → renderError error
      _, Just deckId →
        HH.slot' cpDeck deckId \_ →
          let init = opaqueState $ Deck.initialDeck deckId
          in { component: DN.comp (deckOpts deckId) init
             , initialState: DN.initialState
             }
      _, _ → renderError $ QE.Error $ Exn.error "Missing deck id (impossible!)"

  deckOpts deckId =
    { accessType
    , cursor: List.Nil
    }

eval ∷ Query ~> WorkspaceDSL
eval = case _ of
  Init next → do
    { bus, accessType } ← liftH' Wiring.expose
    cardGuideStep ← initialCardGuideStep
    when (AT.isEditable accessType) do
      H.modify _ { cardGuideStep = cardGuideStep }
    subscribeToBus'
      (H.action ∘ PresentStepByStepGuide)
      bus.stepByStep
    H.subscribe'
      $ throttledEventSource_ (Milliseconds 100.0) onResize
      $ pure (H.action Resize)
    -- The deck component isn't initialised before this later has completed
    H.fromAff $ Aff.later (pure unit)
    when (isNothing cardGuideStep) do
      void $ queryDeck $ H.action Deck.DismissedCardGuide
    pure next
  PresentStepByStepGuide stepByStepGuide next →
    case stepByStepGuide of
      Wiring.CardGuide → H.modify (_cardGuideStep .~ Just 0) $> next
      Wiring.FlipGuide → H.modify (_flipGuideStep .~ Just 0) $> next
  CardGuideStepNext next →
    H.modify State.cardGuideStepNext $> next
  CardGuideDismiss next → do
    liftH' $ LocalStorage.setLocalStorage Guide.dismissedCardGuideKey true
    H.modify (_cardGuideStep .~ Nothing)
    queryDeck $ H.action Deck.DismissedCardGuide
    pure next
  FlipGuideStepNext next →
    H.modify State.flipGuideStepNext $> next
  FlipGuideDismiss next → do
    liftH' $ LocalStorage.setLocalStorage Guide.dismissedFlipGuideKey true
    H.modify (_flipGuideStep .~ Nothing)
    pure next
  DismissAll ev next → do
    querySignIn $ H.action GlobalMenu.DismissSubmenu
    eq ← H.fromEff $ elementEq ev.target ev.currentTarget
    when eq $ void $ queryDeck $ H.action Deck.Focus
    pure next
  Resize next →
    queryDeck (H.action Deck.UpdateCardSize) $> next
  New next → do
    st ← H.get
    when (isNothing st.deckId) do
      runFreshWorkspace mempty
    pure next
  ExploreFile res next → do
    st ← H.get
    when (isNothing st.deckId) do
      runFreshWorkspace
        [ CM.Open (R.File res)
        , CM.Table JT.emptyModel
        ]
    pure next
  Load deckId next → do
    case deckId of
      Nothing → do
        H.modify _ { stateMode = Loading }
        rootDeck >>= case _ of
          Left err →
            case GE.fromQError err of
              Left _ → H.modify _ { stateMode = Error err }
              Right ge → GE.raiseGlobalError ge
          Right deckId' → do
            H.modify _
              { stateMode = Ready
              , deckId = Just deckId'
              }
      Just _ → do
        H.modify _
          { stateMode = Ready
          , deckId = deckId
          }
    void $ queryDeck $ H.action Deck.Focus
    pure next

rootDeck ∷ WorkspaceDSL (Either QE.QError DeckId)
rootDeck = do
  { path } ← liftH' Wiring.expose
  Model.getRoot (path </> Pathy.file "index")

setRoot ∷ DeckId → WorkspaceDSL (Either QE.QError Unit)
setRoot deckId = do
  { path } ← liftH' Wiring.expose
  Model.setRoot (path </> Pathy.file "index") deckId

runFreshWorkspace ∷ Array CM.AnyCardModel → WorkspaceDSL Unit
runFreshWorkspace cards = do
  { path, accessType, varMaps, bus } ← liftH' Wiring.expose
  deckId × cell ← liftH' $ P.freshWorkspace cards
  H.modify _
    { stateMode = Ready
    , deckId = Just deckId
    }
  void $ queryDeck $ H.action Deck.Focus
  let
    wait =
      H.fromAff (Bus.read cell.bus) >>= case _ of
        ED.Pending _ → wait
        ED.Complete _ _ → wait
        _ → pure unit
  wait
  setRoot deckId
  urlVarMaps ← liftH' $ Cache.snapshot varMaps
  navigate $ WorkspaceRoute path (Just deckId) (WA.Load accessType) urlVarMaps

peek ∷ ∀ a. ChildQuery a → WorkspaceDSL Unit
peek = (const (pure unit)) ⨁ const (pure unit) ⨁ peekNotification
  where
  peekNotification ∷ NC.Query a → WorkspaceDSL Unit
  peekNotification = case _ of
    NC.Action N.ExpandGlobalMenu _ → do
      queryHeaderGripper $ Gripper.StartDragging 0.0 unit
      queryHeaderGripper $ Gripper.StopDragging unit
    NC.Action (N.Fulfill var) _ →
      void $ H.fromAff $ Aff.attempt $ putVar var unit
    _ → pure unit

queryDeck ∷ ∀ a. Deck.Query a → WorkspaceDSL (Maybe a)
queryDeck q = do
  deckId ← H.gets _.deckId
  join <$> for deckId \d → H.query' cpDeck d (right q)

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

initialCardGuideStep ∷ WorkspaceDSL (Maybe Int)
initialCardGuideStep =
  liftH'
    $ either (const $ Just 0) (if _ then Nothing else Just 0)
    <$> LocalStorage.getLocalStorage Guide.dismissedCardGuideKey
