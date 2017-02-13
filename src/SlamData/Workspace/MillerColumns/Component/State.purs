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

module SlamData.Workspace.MillerColumns.Component.State where

import Data.List (List, (:))
import Data.List as L
import Data.Traversable (scanr)

import SlamData.Workspace.MillerColumns.Column.Component as Column

type State = List

-- | Get a list of paths for the columns.
columnPaths
  ∷ ∀ a i f o
  . Column.ColumnOptions a i f o
  → State i
  → List (List i)
columnPaths { isLeaf } path =
  L.dropWhile isLeaf (scanr (:) L.Nil path)
