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

import Prelude

import Control.Apply ((*>))
import Control.Bind ((>=>), (=<<))
import Control.Coroutine.Aff (produce)
import Control.Coroutine.Stalling (producerToStallingProducer)
import Control.Monad.Eff.Shortcut (onShortcut)
import Control.Monad.Eff.Shortcut.Platform (shortcutPlatform)
import Control.Monad.Error.Class as EC
import Control.Monad.Except.Trans as ET
import Control.Monad.Trans as MT
import Control.MonadPlus (guard)
import Control.UI.Browser (newTab, locationString)

import Data.Array (cons)
import Data.Either (Either(..), either)
import Data.Foldable as F
import Data.Functor (($>))
import Data.Functor.Coproduct (coproduct, left, right)
import Data.Functor.Eff (liftEff)
import Data.Lens ((^.), (.~), (%~), (?~))
import Data.Maybe (Maybe(..), maybe, fromMaybe, isJust)
import Data.Shortcut (print)
import Data.StrMap as SM
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), uncurry)

import DOM.Event.EventTarget (removeEventListener)
import DOM.Event.EventTypes (keydown)

import Halogen
import Halogen.HTML.Core (className)
import Halogen.HTML.Events.Indexed as E
import Halogen.HTML.Indexed as H
import Halogen.HTML.Properties.Indexed as P
import Halogen.Menu.Component as HalogenMenu
import Halogen.Menu.Submenu.Component as HalogenMenu
import Halogen.Query.EventSource (EventSource(..))
import Halogen.Themes.Bootstrap3 as B

import SlamData.Notebook.AccessType (isReadOnly)
import SlamData.Notebook.Action as NA
import SlamData.Notebook.Cell.CellId as CID
import SlamData.Notebook.Cell.CellType as CT
import SlamData.Notebook.Cell.Component.Query as CQ
import SlamData.Notebook.Component.ChildSlot
import SlamData.Notebook.Component.Query
import SlamData.Notebook.Component.State
import SlamData.Notebook.Dialog.Component as Dialog
import SlamData.Notebook.Editor.Component as Notebook
import SlamData.Notebook.Editor.Component.CellSlot as Notebook
import SlamData.Effects (Slam())
import SlamData.Notebook.FormBuilder.Component as FB
import SlamData.Notebook.FormBuilder.Item.Model as FBI
import SlamData.Notebook.Menu.Component.Query as Menu
import SlamData.Notebook.Menu.Component.State as Menu
import SlamData.Notebook.Rename.Component as Rename
import SlamData.SignIn.Component as SignIn
import SlamData.Notebook.Routing (mkNotebookCellURL)
import SlamData.Render.Common (logo, icon')
import SlamData.Render.CSS as Rc

import Utils.DOM (documentTarget)

type StateP = InstalledState State (ChildState Slam) Query ChildQuery Slam ChildSlot
type DraftboardHTML = ParentHTML (ChildState Slam) Query ChildQuery Slam ChildSlot
type DraftboardDSL = ParentDSL State (ChildState Slam) Query ChildQuery Slam ChildSlot

comp :: Component StateP QueryP Slam
comp = parentComponent' render eval peek

render :: State -> DraftboardHTML
render state =
  H.div
    [ P.classes classes
    , E.onClick (E.input_ DismissAll)
    ]
    [ H.nav
        [ P.classes visibilityClasses ]
        [ renderHeader state ]
    , H.div
        [ P.classes [ notebookClass ] ]
        [  H.slot' cpNotebook unit \_ ->
          { component: Notebook.notebookComponent
          , initialState: Notebook.initialState (state ^. _browserFeatures)
          }
        ]
    , H.slot' cpDialog unit \_ ->
        { component: Dialog.comp
        , initialState: installedState Dialog.initialState
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
    H.div
      [ P.initializer \_ -> action ActivateKeyboardShortcuts ]
      [ H.div
          [ P.classes [ B.clearfix ] ]
          [ H.div
              [ P.classes [ Rc.header, B.clearfix ] ]
              [ icon' B.glyphiconChevronLeft "Back to parent folder"
                $ fromMaybe "" (state ^. _parentHref)
              , logo (state ^. _version)
              , H.slot' cpRename unit \_ ->
                  { component: Rename.comp
                  , initialState: Rename.initialState
                  }
              , H.slot' cpSignIn unit \_ ->
                  { component: SignIn.comp
                  , initialState: installedState SignIn.initialState
                  }
              , H.div
                  [ P.classes $ [ className "sd-menu" ] <> visibilityClasses ]
                  [ H.slot' cpMenu MenuSlot \_ ->
                    { component: HalogenMenu.menuComponent
                    , initialState: installedState $ Menu.make SM.empty
                    }
                  ]
              ]
          ]
      ]

activateKeyboardShortcuts :: DraftboardDSL Unit
activateKeyboardShortcuts = do
  initialShortcuts <- gets _.notebookShortcuts
  platform <- liftEff shortcutPlatform
  let labelShortcut shortcut =
        shortcut { label = Just $ print platform shortcut.shortcut }
      shortcuts = map labelShortcut initialShortcuts
  modify (_notebookShortcuts .~ shortcuts)

  queryMenu $ action $ HalogenMenu.SetMenu $ Menu.make shortcuts

  subscribe' $ EventSource $ producerToStallingProducer $ produce \emit -> do
    target <- documentTarget
    let evaluateMenuValue = emit <<< Left <<< action <<< EvaluateMenuValue
        addKeyboardListeners = emit <<< Left <<< action <<< AddKeyboardListener
        activate shrtct =
          onShortcut platform target (evaluateMenuValue shrtct.value) shrtct.shortcut
    listeners <- traverse activate shortcuts
    traverse addKeyboardListeners listeners
    pure unit

deactivateKeyboardShortcuts :: DraftboardDSL Unit
deactivateKeyboardShortcuts = do
  let remove lstnr = liftEff $ documentTarget
                     >>= removeEventListener keydown lstnr false
  gets _.keyboardListeners >>= traverse remove
  modify (_keyboardListeners .~ [])

eval :: Natural Query DraftboardDSL
eval (ActivateKeyboardShortcuts next) =
  activateKeyboardShortcuts $> next
eval (DeactivateKeyboardShortcuts next) =
  deactivateKeyboardShortcuts $> next
eval (EvaluateMenuValue value next) =
  dismissAll *> evaluateMenuValue value $> next
eval (AddKeyboardListener listener next) =
  modify (_keyboardListeners %~ cons listener) $> next
eval (SetAccessType aType next) = do
  modify (_accessType .~ aType)
  queryNotebook $ action $ Notebook.SetAccessType aType
  pure next
eval (GetAccessType k) = k <$> gets _.accessType
eval (SetViewingCell mbcid next) = do
  modify (_viewingCell .~ mbcid)
  queryNotebook $ action $ Notebook.SetViewingCell mbcid
  pure next
eval (GetViewingCell k) = k <$> gets _.viewingCell
eval (DismissAll next) = dismissAll *> pure next
eval (SetParentHref href next) = modify (_parentHref ?~ href) $> next

dismissAll :: DraftboardDSL Unit
dismissAll = do
  queryMenu $ action HalogenMenu.DismissSubmenu
  querySignIn $ action SignIn.DismissSubmenu

peek :: forall a. ChildF ChildSlot ChildQuery a -> DraftboardDSL Unit
peek (ChildF p q) =
  coproduct
    renamePeek
    (coproduct
      signInParentPeek
      (coproduct
        menuPeek
        (coproduct
          dialogParentPeek
          notebookPeek)))
    q

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
    \(ChildF (Notebook.CellSlot cid) q) ->
      coproduct
        (cellPeek cid)
        (const (pure unit))
        q

cellPeek :: forall a. CID.CellId -> CQ.CellQuery a -> DraftboardDSL Unit
cellPeek cid q =
  case q of
    CQ.ShareCell _ -> do
      root <- liftEff locationString
      showDialog <<< either Dialog.Error (uncurry Dialog.Embed) =<< ET.runExceptT do
        liftNotebookQuery $ action Notebook.SaveNotebook
        path <-
          liftNotebookQuery (request Notebook.GetNotebookPath)
            >>= maybe (EC.throwError "Could not determine notebook path") pure
        varMap <-
          liftNotebookQuery (request (Notebook.FindCellParent cid))
            >>= maybe (pure SM.empty) hereditaryVarMapDefaults
        pure $
          Tuple
            (root <> "/" <> mkNotebookCellURL path cid NA.ReadOnly SM.empty)
            varMap
    _ ->
      pure unit

  where
    hereditaryVarMapDefaults cid = do
      pid <- liftNotebookQuery (request (Notebook.FindCellParent cid))
      SM.union
        <$> varMapDefaults cid
        <*> (traverse hereditaryVarMapDefaults pid <#> fromMaybe SM.empty)

    varMapDefaults cid = do
      tau <-
        liftNotebookQuery (request (Notebook.GetCellType cid))
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

          liftFormBuilderQuery cid (request FB.GetItems)
            <#> F.foldl (flip alg) SM.empty
        _ ->
          pure SM.empty

    liftFormBuilderQuery
      :: CID.CellId -> Natural FB.Query (ET.ExceptT String DraftboardDSL)
    liftFormBuilderQuery cid =
      liftCellQuery cid
        <<< CQ.APIQuery
        <<< right
        <<< ChildF unit
        <<< left

    liftCellQuery
      :: CID.CellId -> Natural CQ.AnyCellQuery (ET.ExceptT String DraftboardDSL)
    liftCellQuery cid =
      queryCell cid >>> MT.lift
        >=> maybe (EC.throwError "Error querying cell") pure

    liftNotebookQuery :: Natural Notebook.Query (ET.ExceptT String DraftboardDSL)
    liftNotebookQuery =
      queryNotebook >>> MT.lift
        >=> maybe (EC.throwError "Error querying notebook") pure

    showDialog =
      queryDialog
        <<< action
        <<< Dialog.Show

menuPeek :: forall a. Menu.QueryP a -> DraftboardDSL Unit
menuPeek = coproduct (const (pure unit)) submenuPeek

renamePeek :: forall a. Rename.Query a -> DraftboardDSL Unit
renamePeek (Rename.Submit next) =
  void $ queryNotebook $ action Notebook.SaveNotebook
renamePeek (Rename.SetText name next) =
  void $ queryNotebook $ action $ Notebook.SetName name
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
   . (ChildF HalogenMenu.SubmenuSlotAddress
      (HalogenMenu.SubmenuQuery (Maybe Menu.Value))) a
  -> DraftboardDSL Unit
submenuPeek (ChildF _ (HalogenMenu.SelectSubmenuItem v _)) =
  maybe (pure unit) evaluateMenuValue v

queryDialog :: Dialog.Query Unit -> DraftboardDSL Unit
queryDialog q = query' cpDialog unit (left q) *> pure unit

queryNotebook :: forall a. Notebook.Query a -> DraftboardDSL (Maybe a)
queryNotebook = query' cpNotebook unit <<< left

queryCell :: forall a. CID.CellId -> CQ.AnyCellQuery a -> DraftboardDSL (Maybe a)
queryCell cid =
  query' cpNotebook unit
    <<< right
    <<< ChildF (Notebook.CellSlot cid)
    <<< right
    <<< ChildF unit
    <<< right

queryRename :: Rename.Query Unit -> DraftboardDSL Unit
queryRename q = query' cpRename unit q *> pure unit

querySignIn :: forall a. SignIn.Query a -> DraftboardDSL Unit
querySignIn q = query' cpSignIn unit (left q) *> pure unit

queryMenu :: HalogenMenu.MenuQuery (Maybe Menu.Value) Unit -> DraftboardDSL Unit
queryMenu q = query' cpMenu MenuSlot (left q) *> pure unit

--querySignInMenu :: HalogenMenu.MenuQuery (Maybe Menu.Value) Unit -> DraftboardDSL Unit
--querySignInMenu q = query' cpSignIn MenuSlot (left q) *> pure unit

presentHelp :: Menu.HelpURI -> DraftboardDSL Unit
presentHelp (Menu.HelpURI uri) = liftEff $ newTab uri
