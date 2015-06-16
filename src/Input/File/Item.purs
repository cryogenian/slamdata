module Input.File.Item
  ( ItemInput(..)
  , inputItem
  ) where

import Data.Array ((!!), filter, sortBy, updateAt)
import Data.Maybe (maybe)
import Model.File.Item (Item(..), sortItem, itemResource)
import Model.File.Sort (Sort())

data ItemInput
  = ItemAdd Item
  | ItemRemove Item
  | ItemSelect Number
  | Resort

inputItem :: Sort -> Boolean -> ItemInput -> [Item] -> [Item]
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

resort :: Sort -> Boolean -> [Item] -> [Item]
resort sort searching = sortBy (sortItem searching sort)

modify :: (Boolean -> Item -> Item) -> [Item] -> Number -> [Item]
modify func items ix =
  let unmodify = func false <$> items
      el = func true <$> unmodify !! ix
  in maybe unmodify (\el' -> updateAt ix el' unmodify) el
