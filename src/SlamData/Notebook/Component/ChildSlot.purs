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

import Data.Either.Nested (Either6)
import Data.Functor.Coproduct.Nested (Coproduct6)

import Halogen.Component.ChildPath (ChildPath, cpL, cpR, (:>))

import SlamData.Notebook.Dialog.Component as Dialog
import SlamData.Notebook.Deck.Component as Deck
import SlamData.Notebook.Menu.Component as Menu
import SlamData.Notebook.Rename.Component as Rename
import SlamData.SignIn.Component as SignIn
import SlamData.Notebook.HeaderGripper.Component as HeaderGripper

type ChildQuery =
  Coproduct6
    Rename.Query
    SignIn.QueryP
    Menu.QueryP
    Dialog.QueryP
    Deck.QueryP
    HeaderGripper.Query

type ChildState g =
  Either6
    Rename.State
    SignIn.StateP
    (Menu.StateP g)
    Dialog.StateP
    Deck.StateP
    HeaderGripper.State

type ChildSlot = Either6 Unit Unit Unit Unit Unit Unit

cpRename
  ∷ ∀ g
  . ChildPath
       Rename.State (ChildState g)
       Rename.Query ChildQuery
       Unit ChildSlot
cpRename = cpL :> cpL :> cpL :> cpL :> cpL

cpSignIn
  ∷ ∀ g
  . ChildPath
       SignIn.StateP (ChildState g)
       SignIn.QueryP ChildQuery
       Unit ChildSlot
cpSignIn = cpL :> cpL :> cpL :> cpL :> cpR

cpMenu
  ∷ ∀ g
  . ChildPath
       (Menu.StateP g) (ChildState g)
       Menu.QueryP ChildQuery
       Unit ChildSlot
cpMenu = cpL :> cpL :> cpL :> cpR

cpDialog
  ∷ ∀ g
  . ChildPath
       Dialog.StateP (ChildState g)
       Dialog.QueryP ChildQuery
       Unit ChildSlot
cpDialog = cpL :> cpL :> cpR

cpDeck
  ∷ ∀ g
  . ChildPath
       Deck.StateP (ChildState g)
       Deck.QueryP ChildQuery
       Unit ChildSlot
cpDeck = cpL :> cpR

cpHeaderGripper
  ∷ ∀ g
  . ChildPath
      HeaderGripper.State (ChildState g)
      HeaderGripper.Query ChildQuery
      Unit ChildSlot
cpHeaderGripper = cpR
