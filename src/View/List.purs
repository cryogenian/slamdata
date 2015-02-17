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
import Data.Array ((..), length, updateAt, (!!))
import Data.Maybe (fromMaybe)

import Component
import qualified View.Item as Item

type State = {
  items :: [Item.State]
  }

initialState :: State
initialState = {
  items: [{isSelected: false, isHovered: false},
          {isSelected: false, isHovered: false}]
  }

data Action = Init | ItemAction Number Item.Action

view :: Receiver Action _ -> State -> Eff _ VTree
view send st = do
  let len = length st.items
  let enumerated = zip (0..len) st.items
  children <- for enumerated (\(Tuple i st) -> do
                                 Item.view (\x -> send $ ItemAction i x) st)
  return $ div {"className": "list-group"} children

foldState :: Action -> State -> Eff _ State
foldState action state@{items: items} =
  case action of
    Init -> return state
    ItemAction i action -> do
      let states = 
            case action of
              Item.Activate -> (\x -> x{isSelected = false}) <$> items
              _ -> items
      newItem <- Item.foldState action $
                 fromMaybe Item.initialState $
                 states !! i
      let newStates = updateAt i newItem states

      return state{items = newStates}


