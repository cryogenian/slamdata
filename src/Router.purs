-- | Router component. It works with only one route aggregate.
module Router where

import Control.Monad.Eff
import Control.Monad.Eff.Ref
import Signal
import Signal.Channel
import Signal.Effectful
import Data.String.Regex
import Data.Maybe
import Data.Tuple (Tuple(..))
import Data.StrMap (fromList)
import Data.Array hiding (map, append)
import Data.DOM.Simple.Encode
import Debug.Foreign

import Utils 
import qualified Hash as Hash
import Model 


-- | state of routing is sort direction and search string
type State = {
  sort :: Sort,
  search :: String
  }

-- | Make hash
toHash :: forall o. {sort::Sort,search::String|o} -> String
toHash state =
  let encoded = paramatize $ fromList [
        Tuple "sort" $ sortToString state.sort,
        Tuple "q" $ state.search
        ]

  in "?" <> encoded


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
   search: decodeURIComponent (extractSearch hash)}

-- | shortcut for setting search string
-- | I think, that after some other services will be
-- | developed pattern will arise
setSearch :: String -> Eff _ Unit
setSearch search = do
  current <- Hash.getHash
  let st = fromHash current
      
      newSt = st{search = search}
      newHash = toHash newSt
  Hash.setHash newHash

getRoute :: forall e. Eff e State
getRoute = fromHash <$> Hash.getHash



