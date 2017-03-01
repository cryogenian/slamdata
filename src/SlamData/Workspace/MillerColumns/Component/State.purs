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

type State a i =
  { cycle ∷ Int
  , columns ∷ ColumnsData a i
  }

modifyColumns
  ∷ ∀ a i
  . (ColumnsData a i → ColumnsData a i)
  → State a i
  → State a i
modifyColumns f st = { cycle: st.cycle, columns: f st.columns }

-- | The current state of the columns component - the `i` value is the root
-- | path id, the list of `a`s is the items selected in each column. The list
-- | is in "reverse" order, where the head item corresponds to the last
-- | selection in the view, and the last item is the selection in the first
-- | column.
type ColumnsData a i = i × L.List a

-- | Mash the values in the state into a list where each item corresponds to an
-- | index for the column, the selected item within that column, and the path
-- | id for the column.
-- |
-- | For example, if the input state is `A × [D, C, B]` the result will be:
-- | ```
-- | [ 0 × Just B × A
-- | , 1 × Just C × B
-- | , 2 × Just D × C
-- | , 3 × Nothing × D
-- | ]
-- | ```
-- |
-- | The exception to this is if the last selection is a leaf according to the
-- | provided `ColumnOptions`, in which case the last entry will be omitted:
-- | ```
-- | [ 0 × Just B × A
-- | , 1 × Just C × B
-- | , 2 × Just D × C
-- | ]
-- | ```
columnPaths
  ∷ ∀ a i f o
  . Column.ColumnOptions a i f o
  → ColumnsData a i
  → L.List (Int × Maybe a × i)
columnPaths colSpec (root × selection) =
  let
    paths = (colSpec.id <$> selection) `L.snoc` root
    sels = Nothing : (Just <$> selection)
    cols = L.zip sels paths
  in
    L.mapWithIndex (flip Tuple) $ L.reverse $
      case L.head paths of
        Just selPath | colSpec.isLeaf selPath → L.drop 1 cols
        _ → cols
