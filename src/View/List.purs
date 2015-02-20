-- | This component has no spec, it won't be rendered alone
module View.List where

import DOM
import View.Shortcuts
import Utils
import Signal hiding (zip)
import Signal.Channel
import Signal.Effectful
import VirtualDOM
import VirtualDOM.VTree
import Control.Monad.Eff

import Data.Tuple 
import Data.Traversable (for)
import Data.Array ((..), length, updateAt, (!!), sortBy)
import Data.Maybe (fromMaybe)

import Model
import Component
import qualified View.Item as Item

-- | Its state has a list of children
type State = {
  items :: [Item.State]
  }

initialState :: State
initialState = {
  items: [{isSelected: false,
           isHovered: false,
           logic: {resource: Item.Notebook, id: "a", name: "foo"}},
          {isSelected: false,
           isHovered: false,
           logic: {resource: Item.File, id: "b", name: "bar"}},
          {isSelected: false,
           isHovered: false,
           logic: {resource: Item.Database, id: "c", name: "baz"}}]
  }

-- | External messages will be marked with index of child
-- | that send it
data Action = Init | ItemAction Number Item.Action | SortAction Sort

-- | Rendering of list
view :: Receiver Action _ -> State -> Eff _ VTree
view send st = do
  let len = length st.items
  -- enumerating children
  let enumerated = zip (0..len) st.items
  -- producing list of vtrees with
  -- receiver that is child receiver wrapped with ItemAction and index 
  children <- for enumerated (\(Tuple i st) -> do
                                 Item.view (\x -> send $ ItemAction i x) st)
  return $ div {"className": "list-group"} children

foldState :: Action -> State -> Eff _ State
foldState action state@{items: items} =
  case action of
    Init -> return state
    ItemAction i action -> do
      -- on select we remove selected status from all children
      let states = 
            case action of
              Item.Activate -> (\x -> x{isSelected = false}) <$> items
              _ -> items
      -- and then apply all other messages
      newItem <- Item.foldState action $
                 fromMaybe Item.initialState $
                 states !! i
      let newStates = updateAt i newItem states

      return state{items = newStates}
    SortAction direction -> do
      let project = _.logic >>> _.name
      let sortFn a b = case direction of
            Desc -> compare (project a) (project b)
            Asc -> compare (project b) (project a)
      return state{items = sortBy sortFn items}

