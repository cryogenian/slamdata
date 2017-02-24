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

module SlamData.Workspace.Card.Setups.DimensionPicker.JCursor where

import SlamData.Prelude

import Control.Comonad.Cofree (Cofree)
import Control.Comonad.Cofree as Cofree

import Data.Argonaut as J
import Data.List (List, (:))
import Data.Map as Map

import SlamData.Workspace.Card.Setups.DimensionPicker.Node (discriminateNodes)

type JCursorNode = Either J.JCursor J.JCursor

groupJCursors
  ∷ List J.JCursor
  → Cofree List JCursorNode
groupJCursors ls = discriminateNodes $ Cofree.mkCofree J.JCursorTop (group ls)
  where
  group l =
    fin (foldl go Map.empty l)

  fin m =
    map (uncurry Cofree.mkCofree) (Map.toList (map group m))

  go m cur = case cur of
    J.JField ix J.JCursorTop →
      Map.insert cur mempty m
    J.JIndex ix J.JCursorTop →
      Map.insert cur mempty m
    J.JField ix next →
      push (J.JField ix J.JCursorTop) next m
    J.JIndex ix next →
      push (J.JIndex ix J.JCursorTop) next m
    J.JCursorTop →
      Map.insert cur mempty m

  push k v =
    Map.alter
      case _ of
        Just vs → Just (v : vs)
        Nothing → Just (pure v)
      k

showJCursor ∷ J.JCursor → String
showJCursor (J.JField i c) = i <> show c
showJCursor J.JCursorTop = "value"
showJCursor c = show c

flattenJCursors ∷ JCursorNode → J.JCursor
flattenJCursors = either id id
