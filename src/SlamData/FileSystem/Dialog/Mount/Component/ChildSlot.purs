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

import Halogen.Component.ChildPath as CP

import SlamData.FileSystem.Dialog.Mount.Couchbase.Component as Couchbase
import SlamData.FileSystem.Dialog.Mount.MarkLogic.Component as MarkLogic
import SlamData.FileSystem.Dialog.Mount.MongoDB.Component as MongoDB
import SlamData.FileSystem.Dialog.Mount.SparkHDFS.Component as Spark
import SlamData.FileSystem.Dialog.Mount.SQL2.Component as SQL2

type ChildQuery
  = MongoDB.Query
  ⨁ SQL2.Query
  ⨁ Couchbase.Query
  ⨁ MarkLogic.Query
  ⨁ Spark.Query
  ⨁ Const Void

type ChildSlot
  = Unit
  ⊹ Unit
  ⊹ Unit
  ⊹ Unit
  ⊹ Unit
  ⊹ Void

cpMongoDB ∷ CP.ChildPath MongoDB.Query ChildQuery Unit ChildSlot
cpMongoDB = CP.cp1

cpSQL ∷ CP.ChildPath SQL2.Query ChildQuery Unit ChildSlot
cpSQL = CP.cp2

cpCouchbase ∷ CP.ChildPath Couchbase.Query ChildQuery Unit ChildSlot
cpCouchbase = CP.cp3

cpMarkLogic ∷ CP.ChildPath MarkLogic.Query ChildQuery Unit ChildSlot
cpMarkLogic = CP.cp4

cpSpark ∷ CP.ChildPath Spark.Query ChildQuery Unit ChildSlot
cpSpark = CP.cp5
