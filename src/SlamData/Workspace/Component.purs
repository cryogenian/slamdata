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

import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Control.UI.Browser (setHref)

import Data.Lens ((^.), (.~), (?~))
import Data.Path.Pathy ((</>))
import Data.Path.Pathy as Pathy

import Halogen as H
import Halogen.Component.ChildPath (injSlot, injQuery)
import Halogen.Component.Opaque.Unsafe (opaqueQuery, opaqueState, peekOpaqueQuery)
import Halogen.HTML.Core (ClassName, className)
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP
import Halogen.Themes.Bootstrap3 as B

import SlamData.Effects (Slam)
import SlamData.FileSystem.Routing (parentURL)
import SlamData.Header.Component as Header
import SlamData.Quasar.Data as Quasar
import SlamData.Render.CSS as Rc
import SlamData.SignIn.Component as SignIn
import SlamData.Workspace.AccessType as AT
import SlamData.Workspace.Card.CardId as CID
import SlamData.Workspace.Card.Draftboard.Common as DBC
import SlamData.Workspace.Component.ChildSlot (ChildQuery, ChildSlot, ChildState, cpDeck, cpHeader)
import SlamData.Workspace.Component.Query (QueryP, Query(..), fromWorkspace, fromDeck, toWorkspace, toDeck)
import SlamData.Workspace.Component.State (State, _accessType, _loaded, _path, _version, _stateMode, _globalVarMap, _deckId, initialState)
import SlamData.Workspace.Deck.Common (wrappedDeck, defaultPosition)
import SlamData.Workspace.Deck.Component as Deck
import SlamData.Workspace.Deck.DeckId (DeckId, freshDeckId)
import SlamData.Workspace.Deck.DeckLevel as DL
import SlamData.Workspace.Deck.Model as DM
import SlamData.Workspace.Model as Model
import SlamData.Workspace.StateMode (StateMode(..))

import Utils.Path as UP

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
    [ HP.classes classes
    , HE.onClick (HE.input_ DismissAll)
    ]
    $ header ⊕ deck
  where
  header ∷ Array WorkspaceHTML
  header = do
    guard (not shouldHideTopMenu)
    pure $ HH.slot' cpHeader unit \_→
      { component: Header.comp
      , initialState: H.parentState Header.initialState
      }

  deck ∷ Array WorkspaceHTML
  deck =
    pure case state.stateMode, state.path, state.deckId of
      Loading, _, _ →
        HH.div [ HP.classes [ workspaceClass ] ]
          []
      Error err, _, _→ showError err
      _, Just path, Just deckId →
        HH.div [ HP.classes [ workspaceClass ] ]
          [ HH.slot' cpDeck unit \_ →
             { component: Deck.comp
             , initialState:
                 opaqueState $
                   (Deck.initialDeck path deckId)
                     { accessType = state.accessType
                     , globalVarMap = state.globalVarMap
                     }
             }
          ]
      _, Nothing, _ → showError "Missing workspace path"
      _, _, Nothing → showError "Missing deck id (impossible!)"

  showError err =
    HH.div [ HP.classes [ B.alert, B.alertDanger ] ]
      [ HH.h1
          [ HP.class_ B.textCenter ]
          [ HH.text err ]
      ]

  shouldHideTopMenu ∷ Boolean
  shouldHideTopMenu = AT.isReadOnly (state ^. _accessType)

  shouldHideEditors ∷ Boolean
  shouldHideEditors = AT.isReadOnly (state ^. _accessType)

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
eval (Init next) = do
  deckId ← H.fromEff freshDeckId
  H.modify (_deckId ?~ deckId)
  pure next
eval (SetGlobalVarMap varMap next) = do
  H.modify (_globalVarMap .~ varMap)
  queryDeck $ H.action $ Deck.SetGlobalVarMap varMap
  pure next
eval (DismissAll next) = do
  querySignIn $ H.action SignIn.DismissSubmenu
  pure next
eval (Reset path next) = do
  H.modify _
    { path = Just path
    , stateMode = Ready
    , accessType = AT.Editable
    }
  queryDeck $ H.action $ Deck.Reset path
  pure next
eval (Load path deckId accessType next) = do
  oldAccessType <- H.gets _.accessType
  H.modify (_accessType .~ accessType)

  queryDeck (H.request Deck.GetId) >>= \deckId' →
    case deckId, deckId' of
      Just a, Just b | a ≡ b ∧ oldAccessType ≡ accessType → pure unit
      _, _ → load accessType
  pure next

  where
  load accessType = do
    H.modify _
      { stateMode = Loading
      , path = Just path
      , deckId = deckId
      }
    queryDeck $ H.action $ Deck.Reset path
    maybe loadRoot loadDeck deckId
    where
    loadDeck deckId = void do
      H.modify _ { stateMode = Ready, deckId = Just deckId }
      queryDeck $ H.action $ Deck.Load path deckId DL.root accessType

    loadRoot =
      rootDeck path >>=
        either (\err → H.modify $ _stateMode .~ Error err) loadDeck

rootDeck ∷ UP.DirPath → WorkspaceDSL (Either String DeckId)
rootDeck path = Model.getRoot (path </> Pathy.file "index")

peek ∷ ∀ a. ChildQuery a → WorkspaceDSL Unit
peek = (peekOpaqueQuery peekDeck) ⨁ (const $ pure unit)
  where
  peekDeck (Deck.DoAction Deck.Mirror _) = pure unit
  peekDeck (Deck.DoAction Deck.Wrap _) = void $ runMaybeT do
    path ← MaybeT $ H.gets _.path

    let index = path </> Pathy.file "index"

    parent ← lift $ join <$> queryDeck (H.request Deck.GetParent)
    oldId ← MaybeT $ queryDeck (H.request Deck.GetId)
    newId ← lift $ H.fromEff freshDeckId

    let transitionDeck newDeck =
          traverse_ (queryDeck ∘ H.action)
            [ Deck.SetParent (Tuple newId (CID.CardId 0))
            , Deck.Save
            , Deck.Reset path
            , Deck.SetModel newId newDeck DL.root
            , Deck.Save
            ]

    lift case parent of
      Just (Tuple deckId cardId) →
        Quasar.load (DM.deckIndex path deckId)
          >>= flip bind DM.decode
          >>> traverse_ \parentDeck → void do
            let cards = DBC.replacePointer oldId newId cardId parentDeck.cards
            Quasar.save (DM.deckIndex path deckId) $ DM.encode parentDeck { cards = cards }
            transitionDeck $ (wrappedDeck defaultPosition oldId) { parent = parent }
      Nothing → void do
        transitionDeck $ wrappedDeck defaultPosition oldId
        Model.setRoot index newId
  peekDeck (Deck.DoAction Deck.DeleteDeck _) = do
    st ← H.get
    for_ st.path \path → do
      res ← Quasar.delete $ Left path
      case res of
        -- TODO: do something to notify the user deleting failed
        Left err → pure unit
        Right _ → void $ H.fromEff $ setHref $ parentURL $ Left path
  peekDeck _ = pure unit

queryDeck ∷ ∀ a. Deck.Query a → WorkspaceDSL (Maybe a)
queryDeck = H.query' cpDeck unit ∘ opaqueQuery

querySignIn ∷ ∀ a. SignIn.Query a → WorkspaceDSL Unit
querySignIn =
  void
    ∘ H.query' cpHeader unit
    ∘ right
    ∘ H.ChildF (injSlot Header.cpSignIn unit)
    ∘ injQuery Header.cpSignIn
    ∘ left
