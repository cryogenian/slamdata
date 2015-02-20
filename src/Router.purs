-- | Router component. It works with only one route aggregate
-- | now :) 
module Router where

import Control.Monad.Eff
import Control.Monad.Eff.Ref
import Signal
import Signal.Channel
import Signal.Effectful
import Data.String.Regex
import Data.Maybe
import Data.Array hiding (map, append)

import Utils 
import Component
import qualified Hash as Hash
import Model 


-- | state of routing is sort direction and search string
type State eff = {
  sort :: Sort,
  search :: String,
  hash :: Service Hash.Action Hash.State eff
  }

-- | Incoming messages can be
-- | HashChanged is external message
data Action = Init | Search String | Sorting Sort | HashChanged String

-- | Make hash
toHash :: forall o. {sort::Sort,search::String|o} -> String
toHash state =
  "?sort=" <> sortToString state.sort <> "&search=" <> state.search

-- | Get sort from hash
extractSort :: String -> Maybe Sort 
extractSort query = do
  let sortR = regex "sort=([^&]+)" noFlags
  strings <- match sortR query
  group <- strings !! 1
  sortFromString group

-- | Get search from hash
extractSearch :: String -> String 
extractSearch query =
  let sortS = regex "q=([^&]+)" noFlags
  in fromMaybe "" $ do
    strings <- match sortS query
    strings !! 1

-- | Make state without hash-component from hash-string
fromHash :: String -> {sort::Sort, search::String}
fromHash hash =
  {sort: fromMaybe Asc $ extractSort hash,
   search: extractSearch hash}

-- | shortcut for setting search string
-- | I think, that after some other services will be
-- | developed pattern will arise
setSearch :: String -> Eff _ Unit
setSearch search = do
  current <- Hash.initialState
  let st = fromHash current
      newSt = st{search = search}
      newHash = toHash newSt
  Hash.setHash newHash

-- | make initial state 
initialState :: Eff _ (State _)
initialState = do
  -- it uses initial state of Hash
  hash <- Hash.construct
  current <- Hash.initialState
  
  return $ {
    sort: fromMaybe Asc $ extractSort current,
    search: extractSearch current,
    hash: hash
    }

-- | Updating router state by messages
foldState :: forall e. Action -> State (chan::Chan|e) ->
             Eff (chan::Chan|e) (State (chan::Chan|e))
foldState action state = do
  let new = case action of
        -- Inner messages
        Init -> state
        Search string -> state{search = string}
        Sorting sort -> state{sort = sort}
        -- External messages
        HashChanged hash ->
          let mbSort = extractSort hash
              search = extractSearch hash
          in state{
            search = search,
            sort = fromMaybe state.sort mbSort
            }
  -- if message is inner then update hash-string
  case action of
    HashChanged _ -> return unit
    _ -> new.hash.send $ toHash new

  return new
 
construct :: Eff _ (Service Action (State _) _)
construct = do
  -- same stuff as everywhere, init, construct output
  init <- initialState
  chan <- channel Init
  signal <- foldpE foldState init (subscribe chan)

  -- listen messages from hash-component and translate them
  -- to router messages
  runSignal $ init.hash.signal ~> \hash -> send chan $ HashChanged hash  
    
  return $ {
    signal: signal,
    send: send chan
    }
  


