{-
Copyright 2015 SlamData, Inc.

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

module Dashboard.Component
  ( comp
  , toNotebook
  , fromNotebook
  , toDashboard
  , fromDashboard
  , toRename
  , fromRename
  , QueryP()
  , StateP()
  , ChildState()
  , ChildSlot()
  , ChildQuery()
  , DialogSlot()
  , RenameSlot()
  , MenuSlot()
  , NotebookSlot()
  , module Dashboard.Component.State
  , module Dashboard.Component.Query
  ) where

import Prelude

import Control.Apply ((*>))
import Control.Bind ((>=>), (=<<))
import Control.Coroutine.Aff (produce)
import Control.Coroutine.Stalling (producerToStallingProducer)
import Control.Monad.Eff.Shortcut (onShortcut)
import Control.Monad.Eff.Shortcut.Platform (shortcutPlatform)
import Control.Monad.Except.Trans as ET
import Control.Monad.Error.Class as EC
import Control.Monad.Trans as MT
import Control.UI.Browser (newTab, locationString)

import DOM.Event.EventTarget (removeEventListener)
import DOM.Event.EventTypes (keydown)

import Data.Array (cons)
import Data.Either (Either(..), either)
import Data.Foldable as F
import Data.Functor (($>))
import Data.Functor.Coproduct (Coproduct(), coproduct, left, right)
import Data.Functor.Eff (liftEff)
import Data.Generic (Generic, gEq, gCompare)
import Data.Shortcut (print)
import Data.Lens ((^.), (.~), (%~))
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.StrMap as SM
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), uncurry)

import Halogen
import Halogen.Component.ChildPath (ChildPath(), cpL, cpR, (:>), injSlot)
import Halogen.HTML.Core (className)
import Halogen.HTML.Events.Indexed as E
import Halogen.HTML.Indexed as H
import Halogen.HTML.Properties.Indexed as P
import Halogen.Menu.Component as HalogenMenu
import Halogen.Menu.Submenu.Component as HalogenMenu
import Halogen.Query.EventSource (EventSource(..))
import Halogen.Themes.Bootstrap3 as B

import Dashboard.Component.Query
import Dashboard.Component.State
import Dashboard.Dialog.Component as Dialog
import Dashboard.Menu.Component.State as Menu
import Dashboard.Menu.Component.Query as Menu
import Dashboard.Rename.Component as Rename

import Model.AccessType (isReadOnly)
import Model.Common (mkNotebookCellURL)
import Model.Notebook.Action as NA

import Notebook.Common (Slam())
import Notebook.Component as Notebook
import Notebook.CellSlot as Notebook
import Notebook.Cell.CellId as CID
import Notebook.Cell.Component.Query as CQ
import Notebook.Cell.CellType as CT
import Notebook.FormBuilder.Component as FB
import Notebook.FormBuilder.Item.Model as FBI

import Render.CssClasses as Rc
import Render.Common (icon, logo)
import Utils.DOM (documentTarget)

type DialogSlot = Unit
type NotebookSlot = Unit
type RenameSlot = Unit
data MenuSlot = MenuSlot

derive instance genericMenuSlot :: Generic MenuSlot
instance eqMenuSlot :: Eq MenuSlot where eq = gEq
instance ordMenuSlot :: Ord MenuSlot where compare = gCompare

type ChildSlot =
  Either RenameSlot
  (Either MenuSlot
   (Either DialogSlot
    NotebookSlot))

type ChildQuery =
  Coproduct Rename.Query
  (Coproduct Menu.QueryP
   (Coproduct Dialog.QueryP
    Notebook.QueryP))

type ChildState g =
  Either Rename.State
  (Either (Menu.StateP g)
   (Either Dialog.StateP
    Notebook.StateP))

cpRename
  :: forall g
   . ChildPath
       Rename.State (ChildState g)
       Rename.Query ChildQuery
       RenameSlot ChildSlot
cpRename = cpL

cpDialog
  :: forall g
   . ChildPath
       Dialog.StateP (ChildState g)
       Dialog.QueryP ChildQuery
       DialogSlot ChildSlot
cpDialog = cpR :> cpR :> cpL

cpNotebook
  :: forall g
   . ChildPath
       Notebook.StateP (ChildState g)
       Notebook.QueryP ChildQuery
       NotebookSlot ChildSlot
cpNotebook = cpR :> cpR :> cpR

cpMenu
  :: forall g
   . ChildPath
       (Menu.StateP g) (ChildState g)
       Menu.QueryP ChildQuery
       MenuSlot ChildSlot
cpMenu = cpR :> cpL

toDashboard :: (Unit -> Query Unit) -> QueryP Unit
toDashboard = left <<< action

fromDashboard
  :: forall a. (forall i. (a -> i) -> Query i) -> QueryP a
fromDashboard r = left $ request r

toNotebook :: (Unit -> Notebook.Query Unit) -> QueryP Unit
toNotebook =
      right
  <<< ChildF (injSlot cpNotebook unit)
  <<< right
  <<< right
  <<< right
  <<< left
  <<< action

fromNotebook
  :: forall a. (forall i. (a -> i) -> Notebook.Query i) -> QueryP a
fromNotebook r =
    right
  $ ChildF (injSlot cpNotebook unit)
  $ right
  $ right
  $ right
  $ left
  $ request r

toRename :: (Unit -> Rename.Query Unit) -> QueryP Unit
toRename =
      right
  <<< ChildF (injSlot cpRename unit)
  <<< left
  <<< action

fromRename
  :: forall a. (forall i. (a -> i) -> Rename.Query i) -> QueryP a
fromRename r =
    right
  $ ChildF (injSlot cpRename unit)
  $ left
  $ request r

type QueryP = Coproduct Query (ChildF ChildSlot ChildQuery)
type StateP = InstalledState State (ChildState Slam) Query ChildQuery Slam ChildSlot
type DashboardHTML = ParentHTML (ChildState Slam) Query ChildQuery Slam ChildSlot
type DashboardDSL = ParentDSL State (ChildState Slam) Query ChildQuery Slam ChildSlot

comp :: Component StateP QueryP Slam
comp = parentComponent' render eval peek

render :: State -> DashboardHTML
render state =
  H.div
    [ P.classes classes
    , E.onClick (E.input_ DismissAll)
    ]
    [ H.nav
        [ P.classes visibilityClasses ]
        [ renderHeader (state ^. _version) ]
    , H.div
        [ P.classes $ [ className "sd-menu" ] <> visibilityClasses ]
        [ H.slot' cpMenu MenuSlot \_ ->
          { component: HalogenMenu.menuComponent
          , initialState: installedState $ Menu.make SM.empty
          }
        ]
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
  classes =
    if isReadOnly (state ^. _accessType)
       then [ Rc.notebookViewHack ]
       else [ Rc.dashboard ]

  notebookClass =
    case state ^. _viewingCell of
      Just _ -> className "sd-notebook-viewing-cell"
      Nothing -> className "sd-notebook"

  visibilityClasses =
    case state ^. _viewingCell of
      Just _ -> [ Rc.invisible ]
      Nothing -> []

  renderHeader :: Maybe String -> DashboardHTML
  renderHeader version =
    H.div
      [ P.initializer \_ -> action ActivateKeyboardShortcuts ]
      [ H.div
          [ P.classes [ B.clearfix ] ]
          [ H.div
              [ P.classes [ Rc.header, B.clearfix ] ]
              [ icon B.glyphiconBook ""
              , logo version
              , H.slot' cpRename unit \_ ->
                  { component: Rename.comp
                  , initialState: Rename.initialState
                  }
              ]
          ]
      ]

activateKeyboardShortcuts :: DashboardDSL Unit
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

deactivateKeyboardShortcuts :: DashboardDSL Unit
deactivateKeyboardShortcuts = do
  let remove lstnr = liftEff $ documentTarget
                     >>= removeEventListener keydown lstnr false
  gets _.keyboardListeners >>= traverse remove
  modify (_keyboardListeners .~ [])

eval :: Natural Query DashboardDSL
eval (ActivateKeyboardShortcuts next) =
  activateKeyboardShortcuts $> next
eval (DeactivateKeyboardShortcuts next) =
  deactivateKeyboardShortcuts $> next
eval (EvaluateMenuValue value next) =
  dismissAll *> evaluateMenuValue value $> next
eval (AddKeyboardListener listener next) =
  modify (_keyboardListeners %~ cons listener) $> next
eval (Save next) = pure next
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

dismissAll :: DashboardDSL Unit
dismissAll =
  queryMenu $
    action HalogenMenu.DismissSubmenu

peek :: forall a. ChildF ChildSlot ChildQuery a -> DashboardDSL Unit
peek (ChildF p q) =
  coproduct
    renamePeek
    (coproduct
      menuPeek
      (coproduct
        dialogParentPeek
        notebookPeek))
    q

dialogParentPeek :: forall a. Dialog.QueryP a -> DashboardDSL Unit
dialogParentPeek = coproduct dialogPeek (const (pure unit))

dialogPeek :: forall a. Dialog.Query a -> DashboardDSL Unit
dialogPeek (Dialog.Dismiss _) = activateKeyboardShortcuts
dialogPeek (Dialog.Show _ _) = deactivateKeyboardShortcuts

notebookPeek :: forall a. Notebook.QueryP a -> DashboardDSL Unit
notebookPeek =
  coproduct
    (const (pure unit))
    \(ChildF (Notebook.CellSlot cid) q) ->
      coproduct
        (cellPeek cid)
        (const (pure unit))
        q

cellPeek :: forall a. CID.CellId -> CQ.CellQuery a -> DashboardDSL Unit
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

    liftFormBuilderQuery :: CID.CellId -> Natural FB.Query (ET.ExceptT String DashboardDSL)
    liftFormBuilderQuery cid =
      liftCellQuery cid
        <<< CQ.APIQuery
        <<< right
        <<< ChildF unit
        <<< left

    liftCellQuery :: CID.CellId -> Natural CQ.AnyCellQuery (ET.ExceptT String DashboardDSL)
    liftCellQuery cid =
      queryCell cid >>> MT.lift
        >=> maybe (EC.throwError "Error querying cell") pure

    liftNotebookQuery :: Natural Notebook.Query (ET.ExceptT String DashboardDSL)
    liftNotebookQuery =
      queryNotebook >>> MT.lift
        >=> maybe (EC.throwError "Error querying notebook") pure

    showDialog =
      queryDialog
        <<< action
        <<< Dialog.Show

menuPeek :: forall a. Menu.QueryP a -> DashboardDSL Unit
menuPeek = coproduct (const (pure unit)) submenuPeek

renamePeek :: forall a. Rename.Query a -> DashboardDSL Unit
renamePeek (Rename.Submit next) =
  void $ queryNotebook $ action Notebook.SaveNotebook
renamePeek (Rename.SetText name next) =
  void $ queryNotebook $ action $ Notebook.SetName name
renamePeek _ = pure unit

evaluateMenuValue :: Menu.Value -> DashboardDSL Unit
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
  -> DashboardDSL Unit
submenuPeek (ChildF _ (HalogenMenu.SelectSubmenuItem v _)) =
  maybe (pure unit) evaluateMenuValue v

queryDialog :: Dialog.Query Unit -> DashboardDSL Unit
queryDialog q = query' cpDialog unit (left q) *> pure unit

queryNotebook :: forall a. Notebook.Query a -> DashboardDSL (Maybe a)
queryNotebook = query' cpNotebook unit <<< left

queryCell :: forall a. CID.CellId -> CQ.AnyCellQuery a -> DashboardDSL (Maybe a)
queryCell cid =
  query' cpNotebook unit
    <<< right
    <<< ChildF (Notebook.CellSlot cid)
    <<< right
    <<< ChildF unit
    <<< right

queryRename :: Rename.Query Unit -> DashboardDSL Unit
queryRename q = query' cpRename unit q *> pure unit

queryMenu :: HalogenMenu.MenuQuery (Maybe Menu.Value) Unit -> DashboardDSL Unit
queryMenu q = query' cpMenu MenuSlot (left q) *> pure unit

presentHelp :: Menu.HelpURI -> DashboardDSL Unit
presentHelp (Menu.HelpURI uri) = liftEff $ newTab uri
