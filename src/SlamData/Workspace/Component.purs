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
  ( component
  , module SlamData.Workspace.Component.Query
  ) where

import SlamData.Prelude

import Control.Monad.Aff as Aff
import Control.Monad.Aff.AVar (makeVar, peekVar, takeVar, putVar)
import Control.Monad.Aff.Bus as Bus
import Control.Monad.Eff.Ref (readRef)
import Control.Monad.Fork (fork)
import Control.UI.Browser as Browser

import Data.Coyoneda (liftCoyoneda)
import Data.List as List
import Data.Time.Duration (Milliseconds(..))
import DOM.Classy.Event (currentTarget, target) as DOM
import DOM.Classy.Node (toNode) as DOM

import Halogen as H
import Halogen.Component.Utils (busEventSource)
import Halogen.Component.Utils.Throttled (throttledEventSource_)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.EventSource as ES

import SlamData.AuthenticationMode as AuthenticationMode
import SlamData.FileSystem.Resource as R
import SlamData.GlobalError as GE
import SlamData.GlobalMenu.Bus (SignInMessage(..))
import SlamData.GlobalMenu.Component as GlobalMenu
import SlamData.Guide.StepByStep.Component as Guide
import SlamData.Header.Component as Header
import SlamData.Header.Gripper.Component as Gripper
import SlamData.Monad (Slam)
import SlamData.Notification.Component as NC
import SlamData.Quasar as Quasar
import SlamData.Quasar.Auth.Authentication as Authentication
import SlamData.Quasar.Error as QE
import SlamData.Wiring as Wiring
import SlamData.Wiring.Cache as Cache
import SlamData.Workspace.AccessType as AT
import SlamData.Workspace.Action as WA
import SlamData.Workspace.Card.Model as CM
import SlamData.Workspace.Card.Table.Model as JT
import SlamData.Workspace.Class (navigate, Routes(..))
import SlamData.Workspace.Component.ChildSlot (ChildQuery, ChildSlot, cpDeck, cpGuide, cpHeader, cpNotify)
import SlamData.Workspace.Component.Query (Query(..))
import SlamData.Workspace.Component.State (State, initialState)
import SlamData.Workspace.Deck.Component as Deck
import SlamData.Workspace.Eval.Deck as ED
import SlamData.Workspace.Eval.Persistence as P
import SlamData.Workspace.Eval.Traverse as ET
import SlamData.Workspace.Guide (GuideType(..))
import SlamData.Workspace.Guide as GuideData
import SlamData.Workspace.StateMode (StateMode(..))
import SlamData.Workspace.StateMode as StateMode

import Utils (endSentence)
import Utils.DOM (onResize, nodeEq)
import Utils.LocalStorage as LocalStorage

type WorkspaceHTML = H.ParentHTML Query ChildQuery ChildSlot Slam
type WorkspaceDSL = H.ParentDSL State Query ChildQuery ChildSlot Void Slam

component ∷ AT.AccessType → H.Component HH.HTML Query Unit Void Slam
component accessType =
  H.lifecycleParentComponent
    { initialState: const initialState
    , render: render accessType
    , eval
    , initializer: Just $ Init unit
    , finalizer: Nothing
    , receiver: const Nothing
    }

render ∷ AT.AccessType → State → WorkspaceHTML
render accessType state =
  HH.div
    [ HP.classes
        $ (guard (AT.isReadOnly accessType) $> HH.ClassName "sd-published")
        ⊕ [ HH.ClassName "sd-workspace" ]
    , HE.onClick (HE.input DismissAll)
    ]
    [ header
    , deck
    , notifications
    , cardGuide
    , flipGuide
    ]
  where
  renderError err =
    HH.div
      [ HP.classes [ HH.ClassName "sd-workspace-error" ] ]
      [ HH.h1_
          [ HH.text "Couldn't load this SlamData workspace." ]
      , HH.p_
          [ HH.text $ endSentence $ QE.printQError err ]
      , if (QE.isUnauthorized err)
          then HH.p_ (renderSignInButton <$> state.providers)
          else HH.text ""
      ]

  renderSignInButton providerR =
    HH.button
      [ HE.onClick $ HE.input_ $ SignIn providerR
      , HP.classes [ HH.ClassName "btn", HH.ClassName "btn-primary" ]
      , HP.type_ HP.ButtonButton
      ]
      [ HH.text $ "Sign in with " ⊕ providerR.displayName ]

  cardGuide =
    if state.guide ≡ Just CardGuide
      then HH.div_ [ HH.slot' cpGuide CardGuide Guide.component GuideData.cardGuideSteps (HE.input (HandleGuideMessage CardGuide)) ]
      else HH.text ""

  flipGuide =
    if state.guide ≡ Just FlipGuide
      then HH.div_ [ HH.slot' cpGuide FlipGuide Guide.component GuideData.flipGuideSteps (HE.input (HandleGuideMessage FlipGuide)) ]
      else HH.text ""

  notifications =
    HH.slot' cpNotify unit (NC.component (NC.renderModeFromAccessType accessType)) unit (HE.input HandleNotification)

  header =
    if AT.isEditable accessType
      then HH.slot' cpHeader unit Header.component unit absurd
      else HH.text ""

  deck =
    case state.stateMode, state.cursor of
      Error error, _ → renderError error
      Loading, _ →
        HH.div
          [ HP.class_ $ HH.ClassName "sd-pending-overlay" ]
          [ HH.div_
              [ HH.i_ []
              , HH.span_ [ HH.text "Please wait while the workspace loads" ]
              ]
          ]
      _, List.Cons deckId cursor →
        HH.slot' cpDeck deckId (Deck.component { accessType, cursor, displayCursor: mempty, deckId }) unit (const Nothing)
      _, _ → HH.text "Error"

eval ∷ Query ~> WorkspaceDSL
eval = case _ of
  Init next → do
    { auth, bus } ← H.lift Wiring.expose
    H.subscribe $ busEventSource (H.request ∘ PresentStepByStepGuide) bus.stepByStep
    H.subscribe $ busEventSource (flip HandleSignInMessage ES.Listening) auth.signIn
    H.subscribe $ throttledEventSource_ (Milliseconds 100.0) onResize (H.request Resize)
    pure next
  PresentStepByStepGuide guideType reply → do
    H.modify (_ { guide = Just guideType })
    pure $ reply H.Listening
  DismissAll ev next → do
    void $ H.query' cpHeader unit $ H.action Header.Dismiss
    H.gets _.cursor >>= List.head >>> traverse_ \deckId → do
      eq ← H.liftEff $ nodeEq (DOM.toNode (DOM.target ev)) (DOM.toNode (DOM.currentTarget ev))
      when eq $ Wiring.focusDeck deckId
    pure next
  Resize reply → do
    queryDeck (H.action Deck.UpdateCardSize)
    pure $ reply H.Listening
  New next → do
    st ← H.get
    when (List.null st.cursor) do
      fork $ runFreshWorkspace mempty
      initializeGuides
    pure next
  ExploreFile res next → do
    st ← H.get
    when (List.null st.cursor) do
      fork $ runFreshWorkspace
        [ CM.Open (R.File res)
        , CM.Table JT.emptyModel
        ]
      initializeGuides
    pure next
  Load cursor next → do
    st ← H.get
    case st.stateMode of
      Loading → do
        rootId ← H.lift P.loadWorkspace
        case rootId of
          Left err → do
            providers ←
              Quasar.retrieveAuthProviders <#> case _ of
                Right (Just providers) → providers
                _ → []
            H.modify _
              { stateMode = Error err
              , providers = providers
              }
            for_ (GE.fromQError err) GE.raiseGlobalError
          Right _ → loadCursor cursor
      _ → loadCursor cursor
    initializeGuides
    pure next
  SignIn providerR next → do
    { auth } ← H.lift Wiring.expose
    idToken ← H.liftAff makeVar
    H.liftAff $ Bus.write { providerR, idToken, prompt: true, keySuffix } auth.requestToken
    either signInFailure (const $ signInSuccess) =<< (H.liftAff $ takeVar idToken)
    pure next
  HandleGuideMessage slot Guide.Dismissed next → do
    case slot of
      CardGuide → do
        H.lift $ LocalStorage.setLocalStorage GuideData.dismissedCardGuideKey true
        void $ queryDeck $ H.action Deck.DismissedCardGuide
      FlipGuide → do
        H.lift $ LocalStorage.setLocalStorage GuideData.dismissedFlipGuideKey true
    H.modify (_ { guide = Nothing })
    pure next
  HandleNotification msg next →
    handleNotification msg $> next
  HandleSignInMessage msg next → do
    stateMode ← H.gets _.stateMode
    when
      (msg ≡ GlobalMenu.SignInSuccess ∧ StateMode.isError stateMode)
      (H.liftEff Browser.reload)
    pure next


  where
  loadCursor cursor = do
    cursor' ←
      if List.null cursor
        then do
          wiring ← H.lift Wiring.expose
          rootId ← H.liftAff $ peekVar wiring.eval.root
          pure (pure rootId)
        else
          hydrateCursor cursor
    H.modify _
      { stateMode = Ready
      , cursor = cursor'
      }
    for_ (List.head cursor') Wiring.focusDeck

  hydrateCursor cursor = H.lift do
    wiring ← Wiring.expose
    ET.hydrateCursor
      <$> Cache.snapshot wiring.eval.decks
      <*> Cache.snapshot wiring.eval.cards
      <*> pure cursor

  keySuffix =
    AuthenticationMode.toKeySuffix AuthenticationMode.ChosenProvider

  signInSuccess = do
    { auth } ← H.lift Wiring.expose
    H.liftAff $ Bus.write SignInSuccess $ auth.signIn
    H.liftEff Browser.reload

  signInFailure error = do
    { auth, bus } ← H.lift Wiring.expose
    H.liftAff do
      for_ (Authentication.toNotificationOptions error) $
        flip Bus.write bus.notify
      Bus.write SignInFailure auth.signIn

runFreshWorkspace ∷ Array CM.AnyCardModel → WorkspaceDSL Unit
runFreshWorkspace cards = do
  { path, accessType, varMaps, bus } ← H.lift Wiring.expose
  deckId × cell ← H.lift $ P.freshWorkspace cards
  H.modify _
    { stateMode = Ready
    , cursor = pure deckId
    }
  Wiring.focusDeck deckId
  let
    wait =
      H.liftAff (Bus.read cell.bus) >>= case _ of
        ED.Pending _ → wait
        ED.Complete _ _ → wait
        ED.CardComplete _ → wait
        ED.CardChange _ → H.gets _.cursor
        ED.NameChange _ → H.gets _.cursor
  cursor ← wait
  H.lift P.saveWorkspace
  urlVarMaps ← H.liftEff $ readRef varMaps
  navigate $ WorkspaceRoute path cursor (WA.Load accessType) urlVarMaps

initializeGuides ∷ WorkspaceDSL Unit
initializeGuides = do
  { bus, accessType } ← H.lift Wiring.expose
  initialCardGuideStep >>= case _ of
    Nothing → do
      void $ queryDeck $ H.action Deck.DismissedCardGuide
    Just ix → when (AT.isEditable accessType) do
      void $ H.query' cpGuide CardGuide $ H.action $ Guide.SetActiveStep ix
      H.modify _ { guide = Just CardGuide }

handleNotification ∷ NC.Action → WorkspaceDSL Unit
handleNotification = case _ of
  NC.ExpandGlobalMenu → do
    gripperState ← queryHeaderGripper $ H.request Gripper.GetState
    when (gripperState ≠ Just Gripper.Opened) do
      queryHeaderGripper $ H.action $ Gripper.StartDragging 0.0
      queryHeaderGripper $ H.action Gripper.StopDragging
      pure unit
  NC.Fulfill var →
    void $ H.liftAff $ Aff.attempt $ putVar var unit

queryDeck ∷ ∀ a. Deck.Query a → WorkspaceDSL (Maybe a)
queryDeck q = do
  deckId ← H.gets (List.head ∘ _.cursor)
  join <$> for deckId \d → H.query' cpDeck d q

queryHeaderGripper ∷ ∀ a. Gripper.Query a → WorkspaceDSL (Maybe a)
queryHeaderGripper =
   H.query' cpHeader unit ∘ Header.QueryGripper ∘ liftCoyoneda

initialCardGuideStep ∷ WorkspaceDSL (Maybe Int)
initialCardGuideStep =
  H.lift
    $ either (const $ Just 0) (if _ then Nothing else Just 0)
    <$> LocalStorage.getLocalStorage GuideData.dismissedCardGuideKey
