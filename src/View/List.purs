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

import Data.Either
import Data.Tuple 
import Data.Traversable (for)
import Data.Array ((..), length, updateAt, (!!), sortBy)
import Data.Maybe 

import Model
import Component
import qualified View.Item as Item
import qualified Hash as Hash
import qualified Router as Router 
import qualified Api.Fs as Fs

import Text.SlamSearch.Parser
import Text.SlamSearch.Parser.Terms
import Text.SlamSearch.Printer

-- | Its state has a list of children
type State = {
  sort :: Sort,
  items :: [Item.State]
  }

initialState :: State
initialState = {
  sort: Asc,
  items: []
  }

-- | External messages will be marked with index of child
-- | that send it
data Action = Init
            | ItemAction Number Item.Action
            | SortAction Sort
            | Update [Item.State]


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
    Update children -> return state{items = sortBy (Item.sort state.sort) children}
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
    SortAction direction -> do
      return state{items = sortBy (Item.sort direction) items, sort = direction}


hookFn :: Receiver Action _ -> Eff _ Unit
hookFn sendBack = do
  Hash.changed $ do
    route <- Router.getRoute
    sendBack (SortAction route.sort)
    case parseSearchQuery route.search of
      Left _ -> return unit
      Right ast -> do
        let path = case getPathTerm ast of
              Nothing -> ""
              Just term ->
                case extractSimpleTerm term of
                  SearchTermSimple _ p -> 
                    prettyPredicate p
        Fs.metadata path $ \res -> do
          sendBack $ Update  (Item.fromMetadata <$> res)



