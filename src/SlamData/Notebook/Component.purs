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

import Control.Coroutine.Aff (produce)
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
import SlamData.Notebook.Cell.CellId as CID
import SlamData.Notebook.Cell.CellType as CT
import SlamData.Notebook.Cell.Component.Query as CQ
import SlamData.Notebook.Component.ChildSlot (ChildQuery, ChildSlot, ChildState, cpDialog, cpMenu, cpNotebook, cpRename, cpSignIn)
import SlamData.Notebook.Component.Query (QueryP, Query(..), fromDraftboard, fromNotebook, fromRename, toDraftboard, toNotebook, toRename)
import SlamData.Notebook.Component.State (NotebookShortcut, State, _accessType, _browserFeatures, _keyboardListeners, _loaded, _notebookShortcuts, _parentHref, _version, _viewingCell, initialState, notebookShortcuts)
import SlamData.Notebook.Dialog.Component as Dialog
import SlamData.Notebook.Editor.Component as Notebook
import SlamData.Notebook.Editor.Component.CellSlot (CellSlot(..))
import SlamData.Effects (Slam)
import SlamData.Notebook.FormBuilder.Component as FB
import SlamData.Notebook.FormBuilder.Item.Model as FBI
import SlamData.Notebook.Menu.Component as Menu
import SlamData.Notebook.Rename.Component as Rename
import SlamData.SignIn.Component as SignIn
import SlamData.Notebook.Routing (mkNotebookCellURL)
import SlamData.Render.Common (logo, icon')
import SlamData.Render.CSS as Rc

import Utils.DOM (documentTarget)

type StateP = H.ParentState State (ChildState Slam) Query ChildQuery Slam ChildSlot
type DraftboardHTML = H.ParentHTML (ChildState Slam) Query ChildQuery Slam ChildSlot
type DraftboardDSL = H.ParentDSL State (ChildState Slam) Query ChildQuery Slam ChildSlot

comp :: H.Component StateP QueryP Slam
comp =
  H.lifecycleParentComponent
    { render
    , eval
    , peek: Just (peek <<< H.runChildF)
    , initializer: Just (H.action ActivateKeyboardShortcuts)
    , finalizer: Nothing
    }

render :: State -> DraftboardHTML
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
        [  HH.slot' cpNotebook unit \_ ->
          { component: Notebook.notebookComponent
          , initialState: Notebook.initialState (state ^. _browserFeatures)
          }
        ]
    , HH.slot' cpDialog unit \_ ->
        { component: Dialog.comp
        , initialState: H.parentState Dialog.initialState
        }
    ]

  where
  shouldHideTopMenu =
       isJust (state ^. _viewingCell)
    || isReadOnly (state ^. _accessType)

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

  renderHeader :: State -> DraftboardHTML
  renderHeader state =
    HH.div_
      [ HH.div
          [ HP.classes [ B.clearfix ] ]
          [ HH.div
              [ HP.classes [ Rc.header, B.clearfix ] ]
              [ icon' B.glyphiconChevronLeft "Back to parent folder"
                $ fromMaybe "" (state ^. _parentHref)
              , logo (state ^. _version)
              , HH.slot' cpRename unit \_ ->
                  { component: Rename.comp
                  , initialState: Rename.initialState
                  }
              , HH.slot' cpSignIn unit \_ ->
                  { component: SignIn.comp
                  , initialState: H.parentState SignIn.initialState
                  }
              , HH.div
                  [ HP.classes $ [ className "sd-menu" ] <> visibilityClasses ]
                  [ HH.slot' cpMenu unit \_ ->
                    { component: HalogenMenu.menuComponent
                    , initialState: H.parentState $ Menu.make SM.empty
                    }
                  ]
              ]
          ]
      ]

activateKeyboardShortcuts :: DraftboardDSL Unit
activateKeyboardShortcuts = do
  initialShortcuts <- H.gets _.notebookShortcuts
  platform <- H.fromEff shortcutPlatform
  let labelShortcut shortcut =
        shortcut { label = Just $ print platform shortcut.shortcut }
      shortcuts = map labelShortcut initialShortcuts
  H.modify (_notebookShortcuts .~ shortcuts)

  queryMenu $ H.action $ HalogenMenu.SetMenu $ Menu.make shortcuts

  H.subscribe' $ EventSource $ producerToStallingProducer $ produce \emit -> do
    target <- documentTarget
    let evaluateMenuValue' = emit <<< Left <<< H.action <<< EvaluateMenuValue
        addKeyboardListeners = emit <<< Left <<< H.action <<< AddKeyboardListener
        activate shrtct =
          onShortcut platform target (evaluateMenuValue' shrtct.value) shrtct.shortcut
    listeners <- traverse activate shortcuts
    traverse addKeyboardListeners listeners
    pure unit

deactivateKeyboardShortcuts :: DraftboardDSL Unit
deactivateKeyboardShortcuts = do
  let remove lstnr = H.fromEff $ documentTarget
                     >>= removeEventListener keydown lstnr false
  H.gets _.keyboardListeners >>= traverse remove
  H.modify (_keyboardListeners .~ [])

eval :: Natural Query DraftboardDSL
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
  queryNotebook $ H.action $ Notebook.SetAccessType aType
  pure next
eval (GetAccessType k) = k <$> H.gets _.accessType
eval (SetViewingCell mbcid next) = do
  H.modify (_viewingCell .~ mbcid)
  queryNotebook $ H.action $ Notebook.SetViewingCell mbcid
  pure next
eval (GetViewingCell k) = k <$> H.gets _.viewingCell
eval (DismissAll next) = dismissAll *> pure next
eval (SetParentHref href next) = H.modify (_parentHref ?~ href) $> next

dismissAll :: DraftboardDSL Unit
dismissAll = do
  queryMenu $ H.action HalogenMenu.DismissSubmenu
  querySignIn $ H.action SignIn.DismissSubmenu

peek :: forall a. ChildQuery a -> DraftboardDSL Unit
peek =
  coproduct5
    renamePeek
    signInParentPeek
    menuPeek
    dialogParentPeek
    notebookPeek

signInParentPeek :: forall a. SignIn.QueryP a -> DraftboardDSL Unit
signInParentPeek = coproduct (const (pure unit)) (const (pure unit))

dialogParentPeek :: forall a. Dialog.QueryP a -> DraftboardDSL Unit
dialogParentPeek = coproduct dialogPeek (const (pure unit))

dialogPeek :: forall a. Dialog.Query a -> DraftboardDSL Unit
dialogPeek (Dialog.Dismiss _) = activateKeyboardShortcuts
dialogPeek (Dialog.Show _ _) = deactivateKeyboardShortcuts

notebookPeek :: forall a. Notebook.QueryP a -> DraftboardDSL Unit
notebookPeek =
  coproduct
    (const (pure unit))
    \(H.ChildF (CellSlot cid) q) ->
      coproduct
        (cellPeek cid)
        (const (pure unit))
        q

cellPeek :: forall a. CID.CellId -> CQ.CellQuery a -> DraftboardDSL Unit
cellPeek cid q =
  case q of
    CQ.ShareCell _ -> do
      root <- H.fromEff locationString
      showDialog <<< either Dialog.Error (uncurry Dialog.Embed) =<< ET.runExceptT do
        liftNotebookQuery $ H.action Notebook.SaveNotebook
        path <-
          liftNotebookQuery (H.request Notebook.GetNotebookPath)
            >>= maybe (EC.throwError "Could not determine notebook path") pure
        varMap <-
          liftNotebookQuery (H.request (Notebook.FindCellParent cid))
            >>= maybe (pure SM.empty) hereditaryVarMapDefaults
        pure $
          Tuple
            (root <> "/" <> mkNotebookCellURL path cid NA.ReadOnly SM.empty)
            varMap
    _ ->
      pure unit

  where
    hereditaryVarMapDefaults cid = do
      pid <- liftNotebookQuery (H.request (Notebook.FindCellParent cid))
      SM.union
        <$> varMapDefaults cid
        <*> (traverse hereditaryVarMapDefaults pid <#> fromMaybe SM.empty)

    varMapDefaults cid = do
      tau <-
        liftNotebookQuery (H.request (Notebook.GetCellType cid))
          >>= maybe (EC.throwError "Could not determine cell type") pure
      case tau of
        CT.API -> do
          let
            defaultVarMapValue { defaultValue, fieldType } =
              case FBI.defaultValueToVarMapValue fieldType =<< defaultValue of
                Just val -> val
                Nothing -> FBI.emptyValueOfFieldType fieldType

            alg =
              SM.insert
                <$> _.name
                <*> defaultVarMapValue

          liftFormBuilderQuery cid (H.request FB.GetItems)
            <#> foldl (flip alg) SM.empty
        _ ->
          pure SM.empty

    liftFormBuilderQuery
      :: CID.CellId -> Natural FB.Query (ET.ExceptT String DraftboardDSL)
    liftFormBuilderQuery cid =
      liftCellQuery cid
        <<< CQ.APIQuery
        <<< right
        <<< H.ChildF unit
        <<< left

    liftCellQuery
      :: CID.CellId -> Natural CQ.AnyCellQuery (ET.ExceptT String DraftboardDSL)
    liftCellQuery cid =
      queryCell cid >>> lift
        >=> maybe (EC.throwError "Error querying cell") pure

    liftNotebookQuery :: Natural Notebook.Query (ET.ExceptT String DraftboardDSL)
    liftNotebookQuery =
      queryNotebook >>> lift
        >=> maybe (EC.throwError "Error querying notebook") pure

    showDialog =
      queryDialog
        <<< H.action
        <<< Dialog.Show

menuPeek :: forall a. Menu.QueryP a -> DraftboardDSL Unit
menuPeek = coproduct (const (pure unit)) submenuPeek

renamePeek :: forall a. Rename.Query a -> DraftboardDSL Unit
renamePeek (Rename.Submit next) =
  void $ queryNotebook $ H.action Notebook.SaveNotebook
renamePeek (Rename.SetText name next) =
  void $ queryNotebook $ H.action $ Notebook.SetName name
renamePeek _ = pure unit

evaluateMenuValue :: Menu.Value -> DraftboardDSL Unit
evaluateMenuValue =
  either
    presentHelp
    (coproduct
      queryRename
      (coproduct
        queryDialog
        (queryNotebook >>> void)))

submenuPeek
  :: forall a
   . (H.ChildF HalogenMenu.SubmenuSlotAddress
      (HalogenSubmenu.SubmenuQuery (Maybe Menu.Value))) a
  -> DraftboardDSL Unit
submenuPeek (H.ChildF _ (HalogenSubmenu.SelectSubmenuItem v _)) =
  maybe (pure unit) evaluateMenuValue v

queryDialog :: Dialog.Query Unit -> DraftboardDSL Unit
queryDialog q = H.query' cpDialog unit (left q) *> pure unit

queryNotebook :: forall a. Notebook.Query a -> DraftboardDSL (Maybe a)
queryNotebook = H.query' cpNotebook unit <<< left

queryCell :: forall a. CID.CellId -> CQ.AnyCellQuery a -> DraftboardDSL (Maybe a)
queryCell cid =
  H.query' cpNotebook unit
    <<< right
    <<< H.ChildF (CellSlot cid)
    <<< right
    <<< H.ChildF unit
    <<< right

queryRename :: Rename.Query Unit -> DraftboardDSL Unit
queryRename q = H.query' cpRename unit q *> pure unit

querySignIn :: forall a. SignIn.Query a -> DraftboardDSL Unit
querySignIn q = H.query' cpSignIn unit (left q) *> pure unit

queryMenu :: HalogenMenu.MenuQuery (Maybe Menu.Value) Unit -> DraftboardDSL Unit
queryMenu q = H.query' cpMenu unit (left q) *> pure unit

--querySignInMenu :: HalogenMenu.MenuQuery (Maybe Menu.Value) Unit -> DraftboardDSL Unit
--querySignInMenu q = H.query' cpSignIn unit (left q) *> pure unit

presentHelp :: Menu.HelpURI -> DraftboardDSL Unit
presentHelp (Menu.HelpURI uri) = H.fromEff $ newTab uri
