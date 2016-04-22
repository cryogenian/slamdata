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

module SlamData.Notebook.Component
  ( comp
  , module SlamData.Notebook.Component.State
  , module SlamData.Notebook.Component.Query
  ) where

import SlamData.Prelude

import Control.Coroutine.Stalling (producerToStallingProducer)
import Control.Monad.Eff.Shortcut (onShortcut)
import Control.Monad.Eff.Shortcut.Platform (shortcutPlatform)
import Control.Monad.Error.Class as EC
import Control.Monad.Except.Trans as ET
import Control.UI.Browser (newTab, locationString)

import Data.Array (cons)
import Data.Functor.Coproduct.Nested (coproduct5)
import Data.Lens ((^.), (.~), (%~), (?~))
import Data.Shortcut (print)
import Data.StrMap as SM
import Data.Path.Pathy (printPath)

import DOM.Event.EventTarget (removeEventListener)
import DOM.Event.EventTypes (keydown)

import Halogen as H
import Halogen.HTML.Core (className)
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP
import Halogen.Menu.Component as HalogenMenu
import Halogen.Menu.Submenu.Component as HalogenSubmenu
import Halogen.Query.EventSource (EventSource(..))
import Halogen.Themes.Bootstrap3 as B

import SlamData.Notebook.AccessType (isReadOnly)
import SlamData.Notebook.Action as NA
import SlamData.Notebook.Card.CardId as CID
import SlamData.Notebook.Card.CardType as CT
import SlamData.Notebook.Card.Component.Query as CQ
import SlamData.Notebook.Component.ChildSlot (ChildQuery, ChildSlot, ChildState, cpDialog, cpMenu, cpDeck, cpRename, cpSignIn)
import SlamData.Notebook.Component.Query (QueryP, Query(..), fromDraftboard, fromDeck, fromRename, toDraftboard, toDeck, toRename)
import SlamData.Notebook.Component.State (NotebookShortcut, State, _accessType, _browserFeatures, _keyboardListeners, _loaded, _notebookShortcuts, _parentHref, _version, _viewingCard, initialState, notebookShortcuts)
import SlamData.Notebook.Dialog.Component as Dialog
import SlamData.Notebook.Deck.Component as Deck
import SlamData.Notebook.Deck.Component.ChildSlot (CardSlot(..))
import SlamData.Effects (Slam)
import SlamData.Notebook.FormBuilder.Component as FB
import SlamData.Notebook.FormBuilder.Item.Model as FBI
import SlamData.Notebook.Menu.Component as Menu
import SlamData.Notebook.Rename.Component as Rename
import SlamData.Notebook.Card.Port.VarMap as VM
import SlamData.SignIn.Component as SignIn
import SlamData.Notebook.Routing (mkNotebookCardURL)
import SlamData.Render.Common (logo, icon')
import SlamData.Render.CSS as Rc
import SlamData.Notebook.Deck.BackSide.Component as Back

import Utils.AffableProducer (produce)
import Utils.DOM (documentTarget)
import Utils.Path as UP

type StateP = H.ParentState State (ChildState Slam) Query ChildQuery Slam ChildSlot
type DraftboardHTML = H.ParentHTML (ChildState Slam) Query ChildQuery Slam ChildSlot
type DraftboardDSL = H.ParentDSL State (ChildState Slam) Query ChildQuery Slam ChildSlot

comp ∷ H.Component StateP QueryP Slam
comp =
  H.lifecycleParentComponent
    { render
    , eval
    , peek: Just (peek ∘ H.runChildF)
    , initializer: Just (H.action ActivateKeyboardShortcuts)
    , finalizer: Nothing
    }

render ∷ State → DraftboardHTML
render state =
  HH.div
    [ HP.classes classes
    , HE.onClick (HE.input_ DismissAll)
    ]
    [ HH.nav
        [ HP.classes visibilityClasses ]
        [ renderHeader state ]
    , HH.div
        [ HP.classes [ notebookClass ] ]
        [  HH.slot' cpDeck unit \_ →
          { component: Deck.deckComponent
          , initialState: Deck.initialState (state ^. _browserFeatures)
          }
        ]
    , HH.slot' cpDialog unit \_ →
        { component: Dialog.comp
        , initialState: H.parentState Dialog.initialState
        }
    ]

  where
  shouldHideTopMenu =
    isJust (state ^. _viewingCard)
    ∨ isReadOnly (state ^. _accessType)

  shouldHideEditors =
    isReadOnly (state ^. _accessType)

  classes =
    if shouldHideEditors
      then [ Rc.notebookViewHack ]
      else [ Rc.dashboard ]

  notebookClass =
    if shouldHideTopMenu
      then className "sd-notebook-hidden-top-menu"
      else className "sd-notebook"

  visibilityClasses =
    guard shouldHideTopMenu
      $> Rc.invisible

  renderHeader ∷ State → DraftboardHTML
  renderHeader state =
    HH.div_
      [ HH.div
          [ HP.classes [ B.clearfix ] ]
          [ HH.div
              [ HP.classes [ Rc.header, B.clearfix ] ]
              [ icon' B.glyphiconChevronLeft "Back to parent folder"
                  $ fromMaybe "" (state ^. _parentHref)
              , logo (state ^. _version)
              , HH.slot' cpRename unit \_ →
                  { component: Rename.comp
                  , initialState: Rename.initialState
                  }
              , HH.slot' cpSignIn unit \_ →
                  { component: SignIn.comp
                  , initialState: H.parentState SignIn.initialState
                  }
              , HH.div
                  [ HP.classes $ [ className "sd-menu" ] ⊕ visibilityClasses ]
                  [ HH.slot' cpMenu unit \_ →
                    { component: HalogenMenu.menuComponent
                    , initialState: H.parentState $ Menu.make SM.empty
                    }
                  ]
              ]
          ]
      ]

activateKeyboardShortcuts ∷ DraftboardDSL Unit
activateKeyboardShortcuts = do
  initialShortcuts ← H.gets _.notebookShortcuts
  platform ← H.fromEff shortcutPlatform
  let
    labelShortcut shortcut =
      shortcut { label = Just $ print platform shortcut.shortcut }
    shortcuts = map labelShortcut initialShortcuts
  H.modify (_notebookShortcuts .~ shortcuts)

  queryMenu $ H.action $ HalogenMenu.SetMenu $ Menu.make shortcuts

  H.subscribe' $ EventSource $ producerToStallingProducer $ produce \emit → do
    target ← documentTarget
    let
      evaluateMenuValue' = emit ∘ Left ∘ H.action ∘ EvaluateMenuValue
      addKeyboardListeners = emit ∘ Left ∘ H.action ∘ AddKeyboardListener
      activate shrtct =
        onShortcut platform target (evaluateMenuValue' shrtct.value) shrtct.shortcut
    listeners ← traverse activate shortcuts
    traverse addKeyboardListeners listeners
    pure unit

deactivateKeyboardShortcuts ∷ DraftboardDSL Unit
deactivateKeyboardShortcuts = do
  let
    remove lstnr =
      H.fromEff $ documentTarget
        >>= removeEventListener keydown lstnr false
  H.gets _.keyboardListeners >>= traverse remove
  H.modify (_keyboardListeners .~ [])

eval ∷ Natural Query DraftboardDSL
eval (ActivateKeyboardShortcuts next) =
  activateKeyboardShortcuts $> next
eval (DeactivateKeyboardShortcuts next) =
  deactivateKeyboardShortcuts $> next
eval (EvaluateMenuValue value next) =
  dismissAll *> evaluateMenuValue value $> next
eval (AddKeyboardListener listener next) =
  H.modify (_keyboardListeners %~ cons listener) $> next
eval (SetAccessType aType next) = do
  H.modify (_accessType .~ aType)
  queryDeck $ H.action $ Deck.SetAccessType aType
  pure next
eval (GetAccessType k) = k <$> H.gets _.accessType
eval (SetViewingCard mbcid next) = do
  H.modify (_viewingCard .~ mbcid)
  queryDeck $ H.action $ Deck.SetViewingCard mbcid
  pure next
eval (GetViewingCard k) = k <$> H.gets _.viewingCard
eval (DismissAll next) = dismissAll *> pure next
eval (SetParentHref href next) = H.modify (_parentHref ?~ href) $> next

dismissAll ∷ DraftboardDSL Unit
dismissAll = do
  queryMenu $ H.action HalogenMenu.DismissSubmenu
  querySignIn $ H.action SignIn.DismissSubmenu

peek ∷ ∀ a. ChildQuery a → DraftboardDSL Unit
peek =
  coproduct5
    renamePeek
    signInParentPeek
    menuPeek
    dialogParentPeek
    deckPeek

signInParentPeek ∷ ∀ a. SignIn.QueryP a → DraftboardDSL Unit
signInParentPeek = coproduct (const (pure unit)) (const (pure unit))

dialogParentPeek ∷ ∀ a. Dialog.QueryP a → DraftboardDSL Unit
dialogParentPeek = coproduct dialogPeek (const (pure unit))

dialogPeek ∷ ∀ a. Dialog.Query a → DraftboardDSL Unit
dialogPeek (Dialog.Dismiss _) = activateKeyboardShortcuts
dialogPeek (Dialog.Show _ _) = deactivateKeyboardShortcuts

deckPeek ∷ ∀ a. Deck.QueryP a → DraftboardDSL Unit
deckPeek =
  coproduct
    (const (pure unit))
    \(H.ChildF s q) →
      coproduct
        (either peekCards (\_ _ → pure unit) s)
        backsidePeek
        q
  where
  peekCards (CardSlot cid) q =
    coproduct
      (cardPeek cid)
      (const $ pure unit)
      q

  backsidePeek (Back.UpdateFilter _ _) = pure unit
  backsidePeek (Back.DoAction action _) = case action of
    Back.Trash →
      pure unit
    Back.Share → do
      loc ← H.fromEff locationString
      queryDeck (H.request Deck.GetNotebookPath)
        <#> join
        >>= traverse_ \nPath →
          showDialog
          $ Dialog.Share
          $ loc
          ⊕ "/"
          ⊕ SlamData.Config.notebookUrl
          ⊕ "#"
          ⊕ UP.encodeURIPath (printPath nPath)
          ⊕ "view"
    Back.Embed →
      queryDeck (H.request Deck.GetActiveCardId)
        <#> join
        >>= traverse_ shareCard
    Back.Publish → do
      pure unit
    Back.Mirror →
      pure unit
    Back.Wrap →
      pure unit

shareCard ∷ CID.CardId → DraftboardDSL Unit
shareCard cid = do
  root ← H.fromEff locationString
  showDialog ∘ either Dialog.Error (uncurry Dialog.Embed) =<< ET.runExceptT do
    liftDeckQuery $ H.action Deck.SaveNotebook
    path ←
      liftDeckQuery (H.request Deck.GetNotebookPath)
        >>= maybe (EC.throwError "Could not determine notebook path") pure
    varMap ←
      liftDeckQuery (H.request (Deck.FindCardParent cid))
        >>= maybe (pure SM.empty) hereditaryVarMapDefaults
    pure $
      Tuple
      (root ⊕ "/" ⊕ mkNotebookCardURL path cid NA.ReadOnly SM.empty)
      varMap

hereditaryVarMapDefaults ∷ CID.CardId → ET.ExceptT String DraftboardDSL VM.VarMap
hereditaryVarMapDefaults cid = do
  pid ← liftDeckQuery (H.request (Deck.FindCardParent cid))
  SM.union
    <$> varMapDefaults cid
    <*> (traverse hereditaryVarMapDefaults pid <#> fromMaybe SM.empty)


varMapDefaults ∷ CID.CardId → ET.ExceptT String DraftboardDSL VM.VarMap
varMapDefaults cid = do
  τ ←
    liftDeckQuery (H.request (Deck.GetCardType cid))
      >>= maybe (EC.throwError "Could not determine card type") pure
  case τ of
    CT.API → do
      let
        defaultVarMapValue { defaultValue, fieldType } =
          case FBI.defaultValueToVarMapValue fieldType =<< defaultValue of
            Just val → val
            Nothing → FBI.emptyValueOfFieldType fieldType
        alg =
          SM.insert
          <$> _.name
          <*> defaultVarMapValue
      liftFormBuilderQuery cid (H.request FB.GetItems)
        <#> foldl (flip alg) SM.empty
    _ →
      pure SM.empty



cardPeek ∷ ∀ a. CID.CardId → CQ.CardQuery a → DraftboardDSL Unit
cardPeek cid q =
  case q of
    CQ.ShareCard _ → shareCard cid
    _ → pure unit


liftFormBuilderQuery
  ∷ CID.CardId → Natural FB.Query (ET.ExceptT String DraftboardDSL)
liftFormBuilderQuery cid =
  liftCardQuery cid
    ∘ CQ.APIQuery
    ∘ right
    ∘ H.ChildF unit
    ∘ left

liftCardQuery
  ∷ CID.CardId
  → Natural CQ.AnyCardQuery (ET.ExceptT String DraftboardDSL)
liftCardQuery cid =
  queryCard cid ⋙ lift
    >=> maybe (EC.throwError "Error querying card") pure

liftDeckQuery
  ∷ Natural Deck.Query (ET.ExceptT String DraftboardDSL)
liftDeckQuery =
  queryDeck ⋙ lift
    >=> maybe (EC.throwError "Error querying notebook") pure

showDialog ∷ Dialog.Dialog → DraftboardDSL Unit
showDialog =
  queryDialog
    ∘ H.action
    ∘ Dialog.Show

menuPeek ∷ ∀ a. Menu.QueryP a → DraftboardDSL Unit
menuPeek = coproduct (const (pure unit)) submenuPeek

renamePeek ∷ ∀ a. Rename.Query a → DraftboardDSL Unit
renamePeek (Rename.Submit next) =
  void $ queryDeck $ H.action Deck.SaveNotebook
renamePeek (Rename.SetText name next) =
  void $ queryDeck $ H.action $ Deck.SetName name
renamePeek _ = pure unit

evaluateMenuValue ∷ Menu.Value → DraftboardDSL Unit
evaluateMenuValue =
  either
    presentHelp
    (coproduct
      queryRename
      (coproduct
        queryDialog
        (queryDeck ⋙ void)))

submenuPeek
  ∷ ∀ a
  . (H.ChildF HalogenMenu.SubmenuSlotAddress
      (HalogenSubmenu.SubmenuQuery (Maybe Menu.Value))) a
  → DraftboardDSL Unit
submenuPeek (H.ChildF _ (HalogenSubmenu.SelectSubmenuItem v _)) =
  maybe (pure unit) evaluateMenuValue v

queryDialog ∷ Dialog.Query Unit → DraftboardDSL Unit
queryDialog q = H.query' cpDialog unit (left q) *> pure unit

queryDeck ∷ ∀ a. Deck.Query a → DraftboardDSL (Maybe a)
queryDeck = H.query' cpDeck unit ∘ left

queryCard ∷ ∀ a. CID.CardId → CQ.AnyCardQuery a → DraftboardDSL (Maybe a)
queryCard cid =
  H.query' cpDeck unit
    ∘ right
    ∘ H.ChildF (Left $ CardSlot cid)
    ∘ left
    ∘ right
    ∘ H.ChildF unit
    ∘ right

queryRename ∷ Rename.Query Unit → DraftboardDSL Unit
queryRename q = H.query' cpRename unit q *> pure unit

querySignIn ∷ ∀ a. SignIn.Query a → DraftboardDSL Unit
querySignIn q = H.query' cpSignIn unit (left q) *> pure unit

queryMenu ∷ HalogenMenu.MenuQuery (Maybe Menu.Value) Unit → DraftboardDSL Unit
queryMenu q = H.query' cpMenu unit (left q) *> pure unit

presentHelp ∷ Menu.HelpURI → DraftboardDSL Unit
presentHelp (Menu.HelpURI uri) = H.fromEff $ newTab uri
