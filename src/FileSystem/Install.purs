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

module FileSystem.Install where

import Prelude

import Data.Either (Either())
import Data.Function (on)
import Data.Functor.Coproduct (Coproduct(), left, right)
import Data.Generic (Generic, gEq, gCompare)
import Data.Path.Pathy (printPath)

import Halogen
import Halogen.Component.ChildPath (ChildPath(), cpL, cpR, (:>), injSlot)

import FileSystem.Breadcrumbs as Breadcrumbs
import FileSystem.Common (Slam())
import FileSystem.Dialog as Dialog
import FileSystem.Items as Items
import FileSystem.Query
import FileSystem.Search as Search
import FileSystem.State
import Utils.Path (DirPath())

type ChildState =
  Either Items.StateP
  (Either Search.State
   (Either Breadcrumbs.State
    Dialog.StateP))

type ChildQuery =
  Coproduct Items.QueryP
  (Coproduct Search.Query
   (Coproduct Breadcrumbs.Query
    Dialog.QueryP))


data ItemsSlot = ItemsSlot
derive instance genericItemsSlot :: Generic ItemsSlot
instance eqItemsSlot :: Eq ItemsSlot where eq = gEq
instance ordItemsSlot :: Ord ItemsSlot where compare = gCompare


data SearchSlot = SearchSlot
derive instance genericSearchSlot :: Generic SearchSlot
instance eqSearchSlot :: Eq SearchSlot where eq = gEq
instance ordSearcSlot :: Ord SearchSlot where compare = gCompare


newtype BreadcrumbsSlot = BreadcrumbsSlot DirPath

instance eqBreadcrumbsSlot :: Eq BreadcrumbsSlot where
  eq (BreadcrumbsSlot p) (BreadcrumbsSlot p') = (on eq printPath) p p'
instance ordBreadcrumbsSlot :: Ord BreadcrumbsSlot where
  compare (BreadcrumbsSlot p) (BreadcrumbsSlot p') = (on compare printPath) p p'

data DialogSlot = DialogSlot
derive instance genericDialogSlot :: Generic DialogSlot
instance eqDialogSlot :: Eq DialogSlot where eq = gEq
instance ordDialogSLot :: Ord DialogSlot where compare = gCompare

type ChildSlot =
  Either ItemsSlot
  (Either SearchSlot
   (Either BreadcrumbsSlot
    DialogSlot))

cpDialog :: ChildPath
            Dialog.StateP ChildState
            Dialog.QueryP ChildQuery
            DialogSlot ChildSlot
cpDialog = cpR :> cpR :> cpR

cpBreadcrumbs :: ChildPath
                 Breadcrumbs.State ChildState
                 Breadcrumbs.Query ChildQuery
                 BreadcrumbsSlot ChildSlot
cpBreadcrumbs = cpR :> cpR :> cpL

cpSearch :: ChildPath
            Search.State ChildState
            Search.Query ChildQuery
            SearchSlot ChildSlot
cpSearch = cpR :> cpL

cpItems :: ChildPath
           Items.StateP ChildState
           Items.QueryP ChildQuery
           ItemsSlot ChildSlot
cpItems = cpL

toFs :: (Unit -> Query Unit) -> QueryP Unit
toFs = left <<< action

toItems :: (Unit -> Items.Query Unit) -> QueryP Unit
toItems =
  right <<< ChildF (injSlot cpItems ItemsSlot)
  <<< left <<< left <<< action

toSearch :: (Unit -> Search.Query Unit) -> QueryP Unit
toSearch =
  right <<< ChildF (injSlot cpSearch SearchSlot)
  <<< right <<< left <<< action

toDialog :: (Unit -> Dialog.Query Unit) -> QueryP Unit
toDialog =
  right <<< ChildF (injSlot cpDialog DialogSlot)
  <<< right <<< right <<< right <<< left <<< action

type StateP = InstalledState State ChildState Query ChildQuery Slam ChildSlot
type QueryP = Coproduct Query (ChildF ChildSlot ChildQuery)
type Algebra = ParentDSL State ChildState Query ChildQuery Slam ChildSlot
