module Input.File.Item
  ( ItemInput(..)
  , inputItem
  ) where

import Data.Array ((!!), filter, sortBy, updateAt)
import Data.Maybe (maybe)
import Model.File.Item (Item(), sortItem)
import Model.Sort (Sort())

data ItemInput
  = ItemAdd Item
  | ItemRemove Item
  | ItemHover Number Boolean
  | ItemSelect Number Boolean
  | Resort

inputItem :: Sort -> Boolean -> [Item] -> ItemInput -> [Item]
inputItem sort searching items input = case input of

  ItemAdd item ->
    resort sort searching $ item : items

  ItemRemove item ->
    resort sort searching $ filter (\x -> not $ x.name == item.name && x.root == item.root) items

  ItemHover ix h ->
    modify (flip _ { hovered = _ }) items ix

  ItemSelect ix h ->
    modify (flip _ { selected = _ }) items ix

resort :: Sort -> Boolean -> [Item] -> [Item]
resort sort searching = sortBy (sortItem searching sort)

modify :: (Boolean -> Item -> Item) -> [Item] -> Number -> [Item]
modify func items ix =
  let unmodify = func false <$> items
      el = func true <$> unmodify !! ix
  in maybe unmodify (\el' -> updateAt ix el' unmodify) el
