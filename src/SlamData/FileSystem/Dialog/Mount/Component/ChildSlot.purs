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

module SlamData.FileSystem.Dialog.Mount.Component.ChildSlot where

import SlamData.Prelude

import Halogen.Component.ChildPath (ChildPath, cpL, cpR, (:>))

import SlamData.FileSystem.Dialog.Mount.MongoDB.Component as MongoDB
import SlamData.FileSystem.Dialog.Mount.SQL2.Component as SQL2
import SlamData.FileSystem.Dialog.Mount.Couchbase.Component as Couchbase
import SlamData.FileSystem.Dialog.Mount.MarkLogic.Component as MarkLogic
import SlamData.FileSystem.Dialog.Mount.SparkHDFS.Component as Spark

type ChildState
  = MongoDB.State
  ⊹ SQL2.StateP
  ⊹ Couchbase.State
  ⊹ MarkLogic.State
  ⊹ Spark.State

type ChildQuery
  = MongoDB.Query
  ⨁ SQL2.QueryP
  ⨁ Couchbase.Query
  ⨁ MarkLogic.Query
  ⨁ Spark.Query

type ChildSlot
  = Unit
  ⊹ Unit
  ⊹ Unit
  ⊹ Unit
  ⊹ Unit

cpMongoDB ∷ ChildPath MongoDB.State ChildState MongoDB.Query ChildQuery Unit ChildSlot
cpMongoDB = cpL

cpSQL ∷ ChildPath SQL2.StateP ChildState SQL2.QueryP ChildQuery Unit ChildSlot
cpSQL = cpR :> cpL

cpCouchbase ∷ ChildPath Couchbase.State ChildState Couchbase.Query ChildQuery Unit ChildSlot
cpCouchbase = cpR :> cpR :> cpL

cpMarkLogic ∷ ChildPath MarkLogic.State ChildState MarkLogic.Query ChildQuery Unit ChildSlot
cpMarkLogic = cpR :> cpR :> cpR :> cpL

cpSpark ∷ ChildPath Spark.State ChildState Spark.Query ChildQuery Unit ChildSlot
cpSpark = cpR :> cpR :> cpR :> cpR
