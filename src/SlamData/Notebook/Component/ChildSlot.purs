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

module SlamData.Notebook.Component.ChildSlot where

import Prelude

import Data.Either
import Data.Functor.Coproduct (Coproduct())
import Data.Generic (Generic, gEq, gCompare)

import Halogen.Component.ChildPath (ChildPath(), cpL, cpR, (:>))

import SlamData.Notebook.Dialog.Component as Dialog
import SlamData.Notebook.Editor.Component.Query as Notebook
import SlamData.Notebook.Editor.Component.State as Notebook
import SlamData.Notebook.Menu.Component.Query as Menu
import SlamData.Notebook.Menu.Component.State as Menu
import SlamData.Notebook.Rename.Component as Rename
import SlamData.SignIn.Component as SignIn

type DialogSlot = Unit
type NotebookSlot = Unit
type RenameSlot = Unit
type SignInSlot = Unit
data MenuSlot = MenuSlot

derive instance genericMenuSlot :: Generic MenuSlot
instance eqMenuSlot :: Eq MenuSlot where eq = gEq
instance ordMenuSlot :: Ord MenuSlot where compare = gCompare

type ChildSlot =
  Either RenameSlot
  (Either SignInSlot
    (Either MenuSlot
     (Either DialogSlot
      NotebookSlot)))

type ChildQuery =
  Coproduct Rename.Query
  (Coproduct SignIn.QueryP
    (Coproduct Menu.QueryP
     (Coproduct Dialog.QueryP
      Notebook.QueryP)))

type ChildState g =
  Either Rename.State
  (Either SignIn.StateP
    (Either (Menu.StateP g)
     (Either Dialog.StateP
      Notebook.StateP)))

cpRename
  :: forall g
   . ChildPath
       Rename.State (ChildState g)
       Rename.Query ChildQuery
       RenameSlot ChildSlot
cpRename = cpL

cpSignIn
  :: forall g
   . ChildPath
       SignIn.StateP (ChildState g)
       SignIn.QueryP ChildQuery
       SignInSlot ChildSlot
cpSignIn = cpR :> cpL

cpDialog
  :: forall g
   . ChildPath
       Dialog.StateP (ChildState g)
       Dialog.QueryP ChildQuery
       DialogSlot ChildSlot
cpDialog = cpR :> cpR :> cpR :> cpL

cpNotebook
  :: forall g
   . ChildPath
       Notebook.StateP (ChildState g)
       Notebook.QueryP ChildQuery
       NotebookSlot ChildSlot
cpNotebook = cpR :> cpR :> cpR :> cpR

cpMenu
  :: forall g
   . ChildPath
       (Menu.StateP g) (ChildState g)
       Menu.QueryP ChildQuery
       MenuSlot ChildSlot
cpMenu = cpR :> cpR :> cpL

