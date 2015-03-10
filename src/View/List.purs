-- | This component has no spec, it won't be rendered alone
module View.List where

import Control.Monad.Eff
import Data.Maybe
import Data.Either
import Data.Tuple 

import DOM (DOM())
import View.Shortcuts (div)
import Signal hiding (zip)
import Signal.Channel (Chan())
import VirtualDOM.VTree (VTree())
import Data.Traversable (for)
import Data.Array ((..), length, updateAt, (!!), sortBy, reverse, drop, filter)
import Data.String (split, joinWith)

import Model (Sort(..), conformQuery)
import Component (Receiver())
import qualified View.Item as Item
import qualified Hash as Hash
import qualified Router as Router 
import qualified Api.Fs as Fs

import Text.SlamSearch.Parser (parseSearchQuery, SearchQuery(..))

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
            | UpNav Item.Action
            | SortAction Sort
            | Update [Item.State]


view :: forall e.
        Receiver Action (dom::DOM, chan::Chan|e) -> State ->
        Eff (dom::DOM, chan::Chan|e) VTree
view send state = do
  let items = state.items
      len = length items
      enumerated = zip (0..len) items
  
  children <- for enumerated
              (\(Tuple i st) -> do
                  Item.view (\x -> send $ ItemAction i x) st)

  return $ div {"className": "list-group"} children
    

foldState :: forall e. Action -> State -> Eff e State
foldState action state@{items: items} =
  case action of
    Init -> return state
    Update children -> return state{items = sortBy (Item.sort state.sort) children}
    ItemAction i action -> do
      states <- case action of
              Item.Activate -> return $ (\x -> x{isSelected = false}) <$> items
              _ -> return items
      newItem <- Item.foldState action $
                 fromMaybe Item.initialState $
                 states !! i
      let newStates = updateAt i newItem states

      return state{items = newStates}
    SortAction direction -> do
      return state{items = sortBy (Item.sort direction) items, sort = direction}


hookFn :: forall e. Receiver Action (chan::Chan, dom::DOM|e) -> 
          Eff (chan::Chan, dom::DOM|e) Unit
hookFn sendBack = do
  Hash.changed $ do
    route <- Router.getRoute
    let path = Router.extractPath route
    let ast = either (const EmptyQuery) id $ parseSearchQuery route.search
    sendBack (SortAction route.sort)
    Fs.metadata path $ \res -> do
      let resData = Item.fromMetadata <$> res
          filtered = filter (conformQuery ast <<< _.logic) resData
          toSend = if path == "/" || path == "" then filtered
                   else Item.upNavState:filtered
      sendBack $ Update toSend



