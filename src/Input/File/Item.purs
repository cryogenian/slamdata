module Input.File.Item
  ( ItemInput(..)
  , inputItem
  ) where

import Data.Maybe (maybe)
import Model.Item
import Model.Sort
import qualified Data.Array as A

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
    resort sort searching $ A.filter (\x -> not $ x.name == item.name && x.root == item.root) items

  ItemHover ix h ->
    modify (flip _ { hovered = _ }) items ix

  ItemSelect ix h ->
    modify (flip _ { selected = _ }) items ix

resort :: Sort -> Boolean -> [Item] -> [Item]
resort sort searching = A.sortBy (sortItem searching sort)

modify :: (Boolean -> Item -> Item) -> [Item] -> Number -> [Item]
modify func items ix =
  let unmodify = func false <$> items
      el = func true <$> unmodify A.!! ix
  in maybe unmodify (\el' -> A.updateAt ix el' unmodify) el
