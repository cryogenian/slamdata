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

import SlamData.Prelude

import Data.List ((:))
import Data.List as L

import SlamData.Workspace.MillerColumns.Column.Component as Column

type State a i = i × L.List a

-- | Mash the values in the state into a list where each item corresponds to a
-- | path for a column and the selected item within that column.
columnPaths
  ∷ ∀ a i f o
  . Column.ColumnOptions a i f o
  → State a i
  → L.List (Int × Maybe a × i)
columnPaths colSpec (root × selection) =
  let
    paths = (colSpec.id <$> selection) `L.snoc` root
    sels = Nothing : (Just <$> selection)
    cols = snd $ foldr (\item (i × acc) → i + 1 × ((i × item) : acc)) (0 × L.Nil) $ L.zip sels paths
  in
    case L.head paths of
      Just selPath | colSpec.isLeaf selPath → L.drop 1 cols
      _ → cols
