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

import Control.Monad.Error.Class as EC
import Control.Monad.Except.Trans as ET
import Control.UI.Browser (locationString)

import Data.Functor.Coproduct.Nested (coproduct3)
import Data.Lens ((^.), (.~), (?~))
import Data.List as L
import Data.StrMap as SM
import Data.Path.Pathy ((</>))
import Data.Path.Pathy as Pathy

import Halogen as H
import Halogen.Component.ChildPath (injSlot)
import Halogen.HTML.Core (ClassName, className)
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP

import SlamData.Effects (Slam)
import SlamData.Header.Component as Header
import SlamData.Workspace.AccessType (isReadOnly)
import SlamData.Workspace.Action as NA
import SlamData.Workspace.Card.CardId as CID
import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.Component.Query as CQ
import SlamData.Workspace.Card.Port.VarMap as VM
import SlamData.Workspace.Component.ChildSlot (ChildQuery, ChildSlot, ChildState, cpDialog, cpDeck, cpHeader)
import SlamData.Workspace.Component.Query (QueryP, Query(..), fromWorkspace, fromDeck, toWorkspace, toDeck)
import SlamData.Workspace.Component.State (State, _accessType, _browserFeatures,  _loaded, _parentHref, _path, _version, _viewingCard, initialState)
import SlamData.Workspace.Deck.BackSide.Component as Back
import SlamData.Workspace.Deck.Component as Deck
import SlamData.Workspace.Deck.Component.ChildSlot (CardSlot(..))
import SlamData.Workspace.Deck.DeckId (DeckId(..))
import SlamData.Workspace.Dialog.Component as Dialog
import SlamData.Workspace.Model as Model
import SlamData.Workspace.FormBuilder.Component as FB
import SlamData.Workspace.FormBuilder.Item.Model as FBI
import SlamData.Workspace.Routing (mkWorkspaceCardURL)
import SlamData.Render.CSS as Rc
import SlamData.SignIn.Component as SignIn

import Utils.Path as UP

type StateP = H.ParentState State ChildState Query ChildQuery Slam ChildSlot
type WorkspaceHTML = H.ParentHTML ChildState Query ChildQuery Slam ChildSlot
type WorkspaceDSL = H.ParentDSL State ChildState Query ChildQuery Slam ChildSlot

comp ∷ H.Component StateP QueryP Slam
comp =
  H.parentComponent
    { render
    , eval
    , peek: Just (peek ∘ H.runChildF)
    }

render ∷ State → WorkspaceHTML
render state =
  HH.div
    [ HP.classes classes
    , HE.onClick (HE.input_ DismissAll)
    ]
    $ header ⊕ [ deck, dialog ]
  where
  header ∷ Array WorkspaceHTML
  header = do
    guard (not shouldHideTopMenu)
    pure $ HH.slot' cpHeader unit \_→
      { component: Header.comp
      , initialState: H.parentState Header.initialState
      }

  deck ∷ WorkspaceHTML
  deck =
    HH.div [ HP.classes [ workspaceClass ] ]
      [ HH.slot' cpDeck unit \_ →
         { component: Deck.comp
         , initialState: Deck.initialState (state.browserFeatures)
         }
      ]

  dialog ∷ WorkspaceHTML
  dialog =
    HH.slot' cpDialog unit \_ →
      { component: Dialog.comp
      , initialState: H.parentState Dialog.initialState
      }

  shouldHideTopMenu ∷ Boolean
  shouldHideTopMenu =
    isJust (state ^. _viewingCard)
    ∨ isReadOnly (state ^. _accessType)

  shouldHideEditors ∷ Boolean
  shouldHideEditors =
    isReadOnly (state ^. _accessType)

  classes ∷ Array ClassName
  classes =
    if shouldHideEditors
      then [ Rc.workspaceViewHack ]
      else [ Rc.dashboard ]

  workspaceClass ∷ ClassName
  workspaceClass =
    if shouldHideTopMenu
      then className "sd-workspace-hidden-top-menu"
      else className "sd-workspace"

eval ∷ Natural Query WorkspaceDSL
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
eval (SetParentHref href next) = H.modify (_parentHref ?~ href) $> next
eval (DismissAll next) = do
  querySignIn $ H.action SignIn.DismissSubmenu
  pure next
eval (GetPath k) = k <$> H.gets _.path
eval (Reset features path next) = do
  H.modify (_path .~ Just path)
  queryDeck $ H.action $ Deck.Reset features path Nothing
  pure next
eval (Load features path deckIds next) = do
  H.modify (_path .~ Just path)
  let load = void ∘ queryDeck ∘ H.action ∘ Deck.Load features path
  case L.head deckIds of
    Just deckId → load deckId
    Nothing → rootDeck path >>= either (const (pure unit)) load
    -- Show load error somehow
  pure next

rootDeck ∷ UP.DirPath → WorkspaceDSL (Either String DeckId)
rootDeck path = map (map DeckId) $ Model.getRoot (path </> Pathy.file "index")


peek ∷ ∀ a. ChildQuery a → WorkspaceDSL Unit
peek =
  coproduct3
    dialogParentPeek
    deckPeek
    (const $ pure unit)

dialogParentPeek ∷ ∀ a. Dialog.QueryP a → WorkspaceDSL Unit
dialogParentPeek = coproduct dialogPeek (const (pure unit))

dialogPeek ∷ ∀ a. Dialog.Query a → WorkspaceDSL Unit
dialogPeek _ = pure unit

deckPeek ∷ ∀ a. Deck.QueryP a → WorkspaceDSL Unit
deckPeek =
  (const $ pure unit)
  ⨁ (\(H.ChildF _ q) → q
     # (const $ pure unit)
     ⨁ backsidePeek
     ⨁ (const $ pure unit))
  where
  backsidePeek (Back.UpdateFilter _ _) = pure unit
  backsidePeek (Back.DoAction action _) = case action of
    Back.Trash →
      pure unit
    Back.Share → do
      loc ← H.fromEff locationString
      queryDeck (H.request Deck.GetPath)
        <#> join
        >>= traverse_ \nPath →
          showDialog
          $ Dialog.Share
          $ loc
          ⊕ "/"
          ⊕ SlamData.Config.workspaceUrl
          ⊕ "#"
          ⊕ UP.encodeURIPath (Pathy.printPath nPath)
          ⊕ "view"
    Back.Embed →
      queryDeck (H.request Deck.GetActiveCardId)
        <#> join
        >>= traverse_ shareCard
    Back.Publish →
      pure unit
    Back.Mirror →
      pure unit
    Back.Wrap →
      pure unit

shareCard ∷ CID.CardId → WorkspaceDSL Unit
shareCard cid = do
  root ← H.fromEff locationString
  showDialog ∘ either Dialog.Error (uncurry Dialog.Embed) =<< ET.runExceptT do
    liftDeckQuery $ H.action Deck.Save
    path ←
      liftDeckQuery (H.request Deck.GetPath)
        >>= maybe (EC.throwError "Could not determine workspace path") pure
    varMap ←
      liftDeckQuery (H.request (Deck.FindCardParent cid))
        >>= maybe (pure SM.empty) hereditaryVarMapDefaults
    pure $
      Tuple
      (root ⊕ "/" ⊕ mkWorkspaceCardURL path cid NA.ReadOnly SM.empty)
      varMap

hereditaryVarMapDefaults ∷ CID.CardId → ET.ExceptT String WorkspaceDSL VM.VarMap
hereditaryVarMapDefaults cid = do
  pid ← liftDeckQuery (H.request (Deck.FindCardParent cid))
  SM.union
    <$> varMapDefaults cid
    <*> (traverse hereditaryVarMapDefaults pid <#> fromMaybe SM.empty)


varMapDefaults ∷ CID.CardId → ET.ExceptT String WorkspaceDSL VM.VarMap
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

liftFormBuilderQuery
  ∷ CID.CardId → Natural FB.Query (ET.ExceptT String WorkspaceDSL)
liftFormBuilderQuery cid =
  liftCardQuery cid
    ∘ CQ.APIQuery
    ∘ right
    ∘ H.ChildF unit
    ∘ left

liftCardQuery
  ∷ CID.CardId
  → Natural CQ.AnyCardQuery (ET.ExceptT String WorkspaceDSL)
liftCardQuery cid =
  queryCard cid ⋙ lift
    >=> maybe (EC.throwError "Error querying card") pure

liftDeckQuery
  ∷ Natural Deck.Query (ET.ExceptT String WorkspaceDSL)
liftDeckQuery =
  queryDeck ⋙ lift
    >=> maybe (EC.throwError "Error querying workspace") pure

showDialog ∷ Dialog.Dialog → WorkspaceDSL Unit
showDialog =
  queryDialog
    ∘ H.action
    ∘ Dialog.Show

queryDialog ∷ Dialog.Query Unit → WorkspaceDSL Unit
queryDialog q = H.query' cpDialog unit (left q) *> pure unit

queryDeck ∷ ∀ a. Deck.Query a → WorkspaceDSL (Maybe a)
queryDeck = H.query' cpDeck unit ∘ left

queryCard ∷ ∀ a. CID.CardId → CQ.AnyCardQuery a → WorkspaceDSL (Maybe a)
queryCard cid =
  H.query' cpDeck unit
    ∘ right
    ∘ H.ChildF (Left $ CardSlot cid)
    ∘ left
    ∘ right
    ∘ H.ChildF unit
    ∘ right

querySignIn ∷ ∀ a. SignIn.Query a → WorkspaceDSL Unit
querySignIn =
  void
    ∘ H.query' cpHeader unit
    ∘ right
    ∘ H.ChildF (injSlot Header.cpSignIn unit)
    ∘ right
    ∘ left
