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

module Input.File.Item
  ( ItemInput(..)
  , inputItem
  ) where

import Prelude
import Data.Array ((!!), filter, sortBy, updateAt, (:), zip, range, length)
import Data.Tuple (Tuple(..))
import Data.Maybe (maybe)
import Model.File.Item (Item(..), sortItem, itemResource)
import Model.File.Sort (Sort())

data ItemInput
  = ItemAdd Item
  | ItemRemove Item
  | ItemSelect Int
  | Resort

inputItem :: Sort -> Boolean -> ItemInput -> Array Item -> Array Item
inputItem sort searching input items = case input of

  ItemAdd item ->
    resort sort searching $ item : items

  ItemRemove item ->
    let r = itemResource item
    in resort sort searching $ filter (\x -> itemResource x /= r) items

  ItemSelect ix ->
    modify toggleItem items ix

toggleItem :: Boolean -> Item -> Item
toggleItem true (Item r) = SelectedItem r
toggleItem false (SelectedItem r) = Item r
toggleItem _ i = i

resort :: Sort -> Boolean -> Array Item -> Array Item
resort sort searching = sortBy (sortItem searching sort)

modify :: (Boolean -> Item -> Item) -> Array Item -> Int -> Array Item
modify func items ix =
  modifyIfIx ix <$> tpls
  where
  tpls = zip (range 0 $ length items) items
  modifyIfIx ix (Tuple i item) =
    if ix == i
    then func true item
    else func false item
 
