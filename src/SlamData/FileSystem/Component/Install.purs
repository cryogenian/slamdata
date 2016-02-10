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

import Prelude

import Data.Either (Either())
import Data.Function (on)
import Data.Functor.Coproduct (Coproduct(), left, right)
import Data.Generic (Generic, gEq, gCompare)
import Data.Path.Pathy (printPath)

import Halogen
import Halogen.Component.ChildPath (ChildPath(), cpL, cpR, (:>), injSlot)

import SlamData.FileSystem.Breadcrumbs.Component as Breadcrumbs
import SlamData.FileSystem.Component.Query
import SlamData.FileSystem.Component.State
import SlamData.FileSystem.Dialog.Component as Dialog
import SlamData.FileSystem.Effects (Slam())
import SlamData.FileSystem.Listing.Component as Items
import SlamData.FileSystem.Search.Component as Search
import SlamData.StylesContainer.Component as Styles
import SlamData.StylesContainer.Model (StyleURL())

import Utils.Path (DirPath())

type ChildState =
  Either Styles.State
  (Either Items.StateP
   (Either Search.State
    (Either Breadcrumbs.State
     Dialog.StateP)))

type ChildQuery =
  Coproduct Styles.Query
  (Coproduct Items.QueryP
   (Coproduct Search.Query
    (Coproduct Breadcrumbs.Query
     Dialog.QueryP)))

data StylesSlot = StylesSlot
derive instance genericeStylesSlot :: Generic StylesSlot
instance eqStylesSlot :: Eq StylesSlot where eq = gEq
instance ordStylesSlot :: Ord StylesSlot where compare = gCompare

data ItemsSlot = ItemsSlot
derive instance genericItemsSlot :: Generic ItemsSlot
instance eqItemsSlot :: Eq ItemsSlot where eq = gEq
instance ordItemsSlot :: Ord ItemsSlot where compare = gCompare


data SearchSlot = SearchSlot
derive instance genericSearchSlot :: Generic SearchSlot
instance eqSearchSlot :: Eq SearchSlot where eq = gEq
instance ordSearcSlot :: Ord SearchSlot where compare = gCompare

data BreadcrumbsSlot = BreadcrumbsSlot DirPath (Array StyleURL)

instance eqBreadcrumbsSlot :: Eq BreadcrumbsSlot where
  eq (BreadcrumbsSlot p e) (BreadcrumbsSlot p' e') =
    ((on eq printPath) p p')
    &&
    e == e'
instance ordBreadcrumbsSlot :: Ord BreadcrumbsSlot where
  compare (BreadcrumbsSlot p e) (BreadcrumbsSlot p' e') =
    ((on compare printPath) p p')
    <>
    compare e e'

data DialogSlot = DialogSlot
derive instance genericDialogSlot :: Generic DialogSlot
instance eqDialogSlot :: Eq DialogSlot where eq = gEq
instance ordDialogSLot :: Ord DialogSlot where compare = gCompare

type ChildSlot =
  Either StylesSlot
  (Either ItemsSlot
   (Either SearchSlot
    (Either BreadcrumbsSlot
     DialogSlot)))

cpDialog :: ChildPath
            Dialog.StateP ChildState
            Dialog.QueryP ChildQuery
            DialogSlot ChildSlot
cpDialog = cpR :> cpR :> cpR :> cpR

cpBreadcrumbs :: ChildPath
                 Breadcrumbs.State ChildState
                 Breadcrumbs.Query ChildQuery
                 BreadcrumbsSlot ChildSlot
cpBreadcrumbs = cpR :> cpR :> cpR :> cpL

cpSearch :: ChildPath
            Search.State ChildState
            Search.Query ChildQuery
            SearchSlot ChildSlot
cpSearch = cpR :> cpR :> cpL

cpItems :: ChildPath
           Items.StateP ChildState
           Items.QueryP ChildQuery
           ItemsSlot ChildSlot
cpItems = cpR :> cpL

cpStyles :: ChildPath
            Styles.State ChildState
            Styles.Query ChildQuery
            StylesSlot ChildSlot
cpStyles = cpL

toFs :: (Unit -> Query Unit) -> QueryP Unit
toFs = left <<< action

toItems :: (Unit -> Items.Query Unit) -> QueryP Unit
toItems =
  right
  <<< ChildF (injSlot cpItems ItemsSlot)
  <<< right
  <<< left
  <<< left
  <<< action

toSearch :: (Unit -> Search.Query Unit) -> QueryP Unit
toSearch =
  right
  <<< ChildF (injSlot cpSearch SearchSlot)
  <<< right
  <<< right
  <<< left
  <<< action

toDialog :: (Unit -> Dialog.Query Unit) -> QueryP Unit
toDialog =
  right
  <<< ChildF (injSlot cpDialog DialogSlot)
  <<< right
  <<< right
  <<< right
  <<< right
  <<< left
  <<< action

toStyles :: (Unit -> Styles.Query Unit) -> QueryP Unit
toStyles =
  right
  <<< ChildF (injSlot cpStyles StylesSlot)
  <<< left
  <<< action

type StateP = InstalledState State ChildState Query ChildQuery Slam ChildSlot
type QueryP = Coproduct Query (ChildF ChildSlot ChildQuery)
type Algebra = ParentDSL State ChildState Query ChildQuery Slam ChildSlot
