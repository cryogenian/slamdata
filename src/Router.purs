-- | Router component. It works with only one route aggregate.
module Router where

import Control.Monad.Eff
import Data.Maybe
import Data.Either
import Data.Tuple (Tuple(..))
import DOM (DOM())
import Data.String.Regex (regex, match, noFlags)
import Data.StrMap (fromList)
import Data.Array hiding (map, append)
import Data.DOM.Simple.Encode (paramatize, decodeURIComponent)
import Data.String (split, joinWith, replace)
import Control.Monad.Error (strMsg)

import Utils (log)
import qualified Hash as Hash
import Model (extractSimpleTerm, Sort(..),
              getPathTerm, getNotPathTerms, sortFromString, sortToString)
import Text.SlamSearch.Parser (parseSearchQuery)
import Text.SlamSearch.Parser.Terms (SearchTermSimple(..))
import Text.SlamSearch.Printer (prettyPredicate, prettyTerm)

type State = {
  sort :: Sort,
  search :: String
  }

toHash :: forall o. {sort::Sort,search::String|o} -> String
toHash state =
  let encoded = paramatize $ fromList [
        Tuple "sort" $ sortToString state.sort,
        Tuple "q" $ state.search
        ]

  in "?" <> encoded


extractSort :: String -> Maybe Sort 
extractSort query = do
  let sortR = regex "sort=([^&]+)" noFlags
  strings <- match sortR query
  group <- strings !! 1
  sortFromString group


extractSearch :: String -> String 
extractSearch query =
  let sortS = regex "q=([^&]+)" noFlags
  in fromMaybe "" $ do
    strings <- match sortS query
    strings !! 1


fromHash :: String -> {sort::Sort, search::String}
fromHash hash =
  {sort: fromMaybe Asc $ extractSort hash,
   search: decodeURIComponent (extractSearch hash)}

setSearch :: forall e. String -> Eff (dom::DOM|e) Unit
setSearch search = do
  current <- Hash.getHash
  let st = fromHash current
      
      newSt = st{search = search}
      newHash = toHash newSt
  Hash.setHash newHash

setSort :: forall e. Sort -> Eff (dom::DOM|e) Unit
setSort sort = do
  current <- Hash.getHash
  let st = fromHash current
      newSt = st{sort = sort}
      newHash = toHash newSt
  Hash.setHash newHash

getRoute :: forall e. Eff e State
getRoute = fromHash <$> Hash.getHash

extractPath :: State -> String
extractPath {search: search} =
  let eitherTerm = do
        ast <- parseSearchQuery search
        term <- maybe (Left (strMsg "There is no path")) Right (getPathTerm ast)
        case extractSimpleTerm term of
          SearchTermSimple _ p -> return $ prettyPredicate p
  in either (const "") id eitherTerm

setPath :: forall e. String -> Eff (dom::DOM|e) Unit
setPath path = do
  let pathParts = split "/" path
      removeUp lst acc =
        case lst of
          [] -> acc
          x:"..":xs -> removeUp xs acc
          "":xs -> removeUp xs acc
          x:xs -> removeUp xs (x:acc)
      filtered = reverse $ removeUp pathParts []
      correct = replace "//" "/" $ "/" <> (joinWith "/" filtered) <> "/"

  {search: search} <- getRoute
  let eitherQuery = do
        ast <- parseSearchQuery search
        let terms = getNotPathTerms ast
            strs = prettyTerm <$> terms
        return $ joinWith " " strs 
  setSearch $ (either (const "") id eitherQuery) <> " path:" <> correct
        
