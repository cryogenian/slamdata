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

import SlamData.Prelude

import Data.Either.Nested (Either5)
import Data.Functor.Coproduct.Nested (Coproduct5)

import Halogen.Component.ChildPath (ChildPath, cpL, cpR, (:>))

import SlamData.Notebook.Dialog.Component as Dialog
import SlamData.Notebook.Editor.Component as Notebook
import SlamData.Notebook.Menu.Component as Menu
import SlamData.Notebook.Rename.Component as Rename
import SlamData.SignIn.Component as SignIn

type ChildQuery =
  Coproduct5
    Rename.Query
    SignIn.QueryP
    Menu.QueryP
    Dialog.QueryP
    Notebook.QueryP

type ChildState g =
  Either5
    Rename.State
    SignIn.StateP
    (Menu.StateP g)
    Dialog.StateP
    Notebook.StateP

type ChildSlot = Either5 Unit Unit Unit Unit Unit

cpRename
  :: forall g
   . ChildPath
       Rename.State (ChildState g)
       Rename.Query ChildQuery
       Unit ChildSlot
cpRename = cpL :> cpL :> cpL :> cpL

cpSignIn
  :: forall g
   . ChildPath
       SignIn.StateP (ChildState g)
       SignIn.QueryP ChildQuery
       Unit ChildSlot
cpSignIn = cpL :> cpL :> cpL :> cpR

cpMenu
  :: forall g
   . ChildPath
       (Menu.StateP g) (ChildState g)
       Menu.QueryP ChildQuery
       Unit ChildSlot
cpMenu = cpL :> cpL :> cpR

cpDialog
  :: forall g
   . ChildPath
       Dialog.StateP (ChildState g)
       Dialog.QueryP ChildQuery
       Unit ChildSlot
cpDialog = cpL :> cpR

cpNotebook
  :: forall g
   . ChildPath
       Notebook.StateP (ChildState g)
       Notebook.QueryP ChildQuery
       Unit ChildSlot
cpNotebook = cpR
