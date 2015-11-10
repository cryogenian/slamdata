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
  , QueryP()
  , StateP()
  , ChildState()
  , ChildSlot()
  , ChildQuery()
  , DialogSlot()
  , NotebookSlot()
  , NavbarSlot()
  , module Dashboard.Component.State
  , module Dashboard.Component.Query
  ) where

import Prelude

import Control.Alt ((<|>))
import Dashboard.Component.Query
import Dashboard.Component.State
import Dashboard.Dialog.Component as Dialog
import Dashboard.Navbar.Component as Navbar
import Data.Either (Either())
import Data.Functor.Coproduct (Coproduct(), left, right)
import Data.Lens ((^.), (.~))
import Data.Maybe (fromMaybe)
import Halogen
import Halogen.Component.ChildPath
  (ChildPath(), cpL, cpR, (:>), injSlot, prjSlot, prjQuery)
import Halogen.HTML as H
import Halogen.HTML.Properties as P
import Model.AccessType (isReadOnly)
import Notebook.Common (Slam())
import Notebook.Component as Notebook
import Render.CssClasses as Rc

type DialogSlot = Unit
type NotebookSlot = Unit
type NavbarSlot = Unit

type ChildSlot =
  Either DialogSlot
  (Either NotebookSlot
   NavbarSlot)
type ChildQuery =
  Coproduct Dialog.QueryP
  (Coproduct Notebook.NotebookQueryP
   Navbar.QueryP)

type ChildState =
  Either Dialog.StateP
  (Either Notebook.NotebookStateP
   Navbar.StateP)

cpDialog :: ChildPath
            Dialog.StateP ChildState
            Dialog.QueryP ChildQuery
            DialogSlot ChildSlot
cpDialog = cpL

cpNotebook :: ChildPath
              Notebook.NotebookStateP ChildState
              Notebook.NotebookQueryP ChildQuery
              NotebookSlot ChildSlot
cpNotebook = cpR :> cpL

cpNavbar :: ChildPath
            Navbar.StateP ChildState
            Navbar.QueryP ChildQuery
            NavbarSlot ChildSlot
cpNavbar = cpR :> cpR

toDashboard :: (Unit -> Query Unit) -> QueryP Unit
toDashboard = left <<< action

fromDashboard
  :: forall a. (forall i. (a -> i) -> Query i) -> QueryP a
fromDashboard r = left $ request r


toNotebook :: (Unit -> Notebook.NotebookQuery Unit) -> QueryP Unit
toNotebook =
  right <<< ChildF (injSlot cpNotebook unit)
  <<< right
  <<< left
  <<< left
  <<< action

fromNotebook
  :: forall a. (forall i. (a -> i) -> Notebook.NotebookQuery i) -> QueryP a
fromNotebook r =
  right
  $ ChildF (injSlot cpNotebook unit)
  $ right
  $ left
  $ left
  $ request r



type QueryP =
  Coproduct Query (ChildF ChildSlot ChildQuery)
type StateP =
  InstalledState State ChildState Query ChildQuery Slam ChildSlot
type DashboardHTML =
  ParentHTML ChildState Query ChildQuery Slam ChildSlot
type DashboardDSL =
  ParentDSL State ChildState Query ChildQuery Slam ChildSlot

comp :: Component StateP QueryP Slam
comp = parentComponent' render eval peek

render :: State -> DashboardHTML
render state =
  H.div [ P.classes classes ]
  [ H.slot' cpNavbar unit
    \_ -> { component: Navbar.comp
          , initialState: Navbar.initialState
          }
  , H.slot' cpNotebook unit
    \_ -> { component: Notebook.notebookComponent
          , initialState: Notebook.initialState (state ^. _browserFeatures)
          }
  , H.slot' cpDialog unit
    \_ -> { component: Dialog.comp
          , initialState: installedState (Dialog.initialState)
          }
  ]

  where
  classes = if isReadOnly (state ^. _accessType)
            then [ Rc.notebookViewHack ]
            else [ ]

eval :: Natural Query DashboardDSL
eval (Save next) = pure next
eval (GetPath continue) = map continue $ gets _.path
eval (SetAccessType aType next) = do
  modify (_accessType .~ aType)
  query' cpNotebook unit
    $ left $ action $ Notebook.SetAccessType aType
  pure next
eval (SetViewingCell mbcid next) = do
  modify (_viewingCell .~ mbcid)
  query' cpNotebook unit
    $ left $ action $ Notebook.SetViewingCell mbcid
  pure next

peek :: forall a. ChildF ChildSlot ChildQuery a -> DashboardDSL Unit
peek (ChildF p q) =
  fromMaybe (pure unit)
  $   (dialogPeek <$> prjSlot cpDialog p <*> prjQuery cpDialog q)
  <|> (notebookPeek <$> prjSlot cpNotebook p <*> prjQuery cpNotebook q)
  <|> (navbarPeek <$> prjSlot cpNavbar p <*> prjQuery cpNavbar q)


dialogPeek :: forall a. DialogSlot -> Dialog.QueryP a -> DashboardDSL Unit
dialogPeek s q = pure unit

notebookPeek
  :: forall a. NotebookSlot -> Notebook.NotebookQueryP a -> DashboardDSL Unit
notebookPeek s q = pure unit

navbarPeek :: forall a. NavbarSlot -> Navbar.QueryP a -> DashboardDSL Unit
navbarPeek s q = pure unit
