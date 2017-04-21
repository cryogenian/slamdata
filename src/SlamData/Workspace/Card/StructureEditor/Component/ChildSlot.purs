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

module SlamData.Workspace.Card.StructureEditor.Component.ChildSlot where

import SlamData.Prelude

import Halogen.Component.ChildPath as CP

import SlamData.Workspace.MillerColumns.Component as MC
import SlamData.Workspace.Card.StructureEditor.Common (ColumnItem, ColumnPath)
import SlamData.Workspace.Card.StructureEditor.Message (Message)

type ColumnsQuery = MC.Query ColumnItem ColumnPath Message
type ColumnsMessage = MC.Message' ColumnItem ColumnPath Message

type ChildQuery = ColumnsQuery ⨁ Const Void
type ChildSlot = Unit ⊹ Void

cpColumns ∷ CP.ChildPath ColumnsQuery ChildQuery Unit ChildSlot
cpColumns = CP.cp1
