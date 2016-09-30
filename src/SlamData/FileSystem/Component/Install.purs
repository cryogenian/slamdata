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

module SlamData.FileSystem.Component.Install where

import SlamData.Prelude

import Halogen (ChildF(..), ParentState, Action, action)
import Halogen.Component.ChildPath (ChildPath, cpL, cpR, (:>), injSlot, injQuery)

import SlamData.Monad (Slam)
import SlamData.FileSystem.Breadcrumbs.Component as Breadcrumbs
import SlamData.FileSystem.Component.Query (Query)
import SlamData.FileSystem.Component.State (State)
import SlamData.FileSystem.Dialog.Component as Dialog
import SlamData.FileSystem.Listing.Component as Listing
import SlamData.FileSystem.Search.Component as Search
import SlamData.Header.Component as Header
import SlamData.SignInPrompt.Component as SignInPrompt

type ChildState =
  Listing.StateP
  ⊹ Search.State
  ⊹ Breadcrumbs.State
  ⊹ Dialog.StateP
  ⊹ Header.StateP
  ⊹ SignInPrompt.State

type ChildQuery =
  Listing.QueryP
  ⨁ Search.Query
  ⨁ Breadcrumbs.Query
  ⨁ Dialog.QueryP
  ⨁ Header.QueryP
  ⨁ SignInPrompt.Query

type ChildSlot =
  Unit
  ⊹ Unit
  ⊹ Unit
  ⊹ Unit
  ⊹ Unit
  ⊹ Unit

cpListing
  ∷ ChildPath
      Listing.StateP ChildState
      Listing.QueryP ChildQuery
      Unit ChildSlot
cpListing = cpL

cpSearch
  ∷ ChildPath
      Search.State ChildState
      Search.Query ChildQuery
      Unit ChildSlot
cpSearch = cpR :> cpL

cpBreadcrumbs
  ∷ ChildPath
      Breadcrumbs.State ChildState
      Breadcrumbs.Query ChildQuery
      Unit ChildSlot
cpBreadcrumbs = cpR :> cpR :> cpL

cpDialog
  ∷ ChildPath
      Dialog.StateP ChildState
      Dialog.QueryP ChildQuery
      Unit ChildSlot
cpDialog = cpR :> cpR :> cpR :> cpL

cpHeader
  ∷ ChildPath
      Header.StateP ChildState
      Header.QueryP ChildQuery
      Unit ChildSlot
cpHeader = cpR :> cpR :> cpR :> cpR :> cpL

cpSignInPrompt
  ∷ ChildPath
      SignInPrompt.State ChildState
      SignInPrompt.Query ChildQuery
      Unit ChildSlot
cpSignInPrompt = cpR :> cpR :> cpR :> cpR :> cpR

toFs ∷ Action Query → QueryP Unit
toFs = left ∘ action

toListing ∷ Action Listing.Query → QueryP Unit
toListing =
  right
    ∘ ChildF (injSlot cpListing unit)
    ∘ injQuery cpListing
    ∘ left
    ∘ action

toSearch ∷ Action Search.Query → QueryP Unit
toSearch =
  right
    ∘ ChildF (injSlot cpSearch unit)
    ∘ injQuery cpSearch
    ∘ action

toDialog ∷ Action Dialog.Query → QueryP Unit
toDialog =
  right
    ∘ ChildF (injSlot cpDialog unit)
    ∘ injQuery cpDialog
    ∘ left
    ∘ action

type StateP = ParentState State ChildState Query ChildQuery Slam ChildSlot
type QueryP = Query ⨁ (ChildF ChildSlot ChildQuery)
