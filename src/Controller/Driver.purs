-- | Module handles outer messages to `halogen` application
-- | Mostly consists of routing functions 
module Controller.Driver (
  outside,
  getPath,
  searchPath,
  updateSort,
  updateQ,
  setSort,
  addToPath,
  updatePath
  ) where

import Control.Monad.Eff
import Control.Monad.Eff.Class
import Control.Apply
import Control.Alt
import Data.Monoid.First
import Data.Foldable
import Data.Maybe
import Data.Either
import Data.Semiring.Free
import Data.Array (filter, reverse, (!!), drop)

import qualified Utils as U
import qualified Data.String as Str
import qualified Data.String.Regex as Rgx
import qualified Control.Timer as Tm
import qualified Halogen as Hl
import qualified Routing as R
import qualified Routing.Match as R
import qualified Routing.Match.Class as R
import qualified Routing.Hash as Rh
import qualified Text.SlamSearch as S
import qualified Text.SlamSearch.Types as S
import qualified Text.SlamSearch.Printer as S
import qualified Data.Minimatch as MM
import qualified Control.Monad.Aff as Aff
import qualified Model as M
import qualified Model.Item as Mi
import qualified Model.Sort as Ms
import qualified Model.Resource as Mr
import qualified Api.Fs as Api
import qualified Network.HTTP.Affjax as Af
import qualified Control.Monad.Aff as Aff

-- | Entry, used in `halogen` app
outside :: forall e. Hl.Driver M.Input (timer :: Tm.Timer, ajax :: Af.Ajax|e) ->
           Eff (Hl.HalogenEffects (timer :: Tm.Timer, ajax :: Af.Ajax|e)) Unit
outside driver = handleRoute driver 

-- | Routing schema
data Routes
  = SortAndQ Ms.Sort S.SearchQuery
  | Sort Ms.Sort
  | Index 

-- | Route parsing match objects
routing :: R.Match Routes
routing = bothRoute <|> oneRoute <|> index
  where bothRoute = SortAndQ <$> sort <*> query
        oneRoute = Sort <$> sort
        index = pure Index
        sort = R.eitherMatch (Ms.string2sort <$> R.param "sort")
        query = R.eitherMatch (S.mkQuery <<< U.decodeURIComponent <$> R.param "q")

-- | Stream of routes 
handleRoute :: forall e. Hl.Driver M.Input (timer :: Tm.Timer, ajax :: Af.Ajax |e) ->
           Eff (Hl.HalogenEffects (timer :: Tm.Timer, ajax :: Af.Ajax|e)) Unit
handleRoute driver = do
  R.matches routing \_ new -> do
    case new of
      -- Fired when both _sort_ and _q_ setted
      SortAndQ sort query -> do
        let path = cleanPath $ fromMaybe "" $ searchPath query
        driver $ M.SetPath path 
        driver $ M.SearchSet (S.strQuery query)
        Aff.launchAff $ do
          unfiltered <- Api.listing path
          let items = _{root = path} <$>
                      (if (path /= "/" && path /= "") then [Mi.upLink] else []) <>
                      (filter (filterByQuery query) unfiltered)
          liftEff (driver $ M.ItemsUpdate items sort)

      -- Fired by default -> redirect to sorted route
      Index -> do
        Rh.setHash $ setSort Ms.Asc 
      -- Only sort setted -> redirects to `q=` 
      Sort sort -> do
        driver $ M.SearchSet ""
        driver $ M.SetPath ""
        Aff.launchAff $ do
          items <- Api.listing ""
          liftEff (driver $ M.ItemsUpdate items sort)
          
  where cleanPath :: String -> String
        cleanPath input =
          let rgx = Rgx.regex "\"" Rgx.noFlags{global=true}
          in Str.trim $ Rgx.replace rgx "" input



-- | check if string satisfies predicate from `purescript-search`
check :: S.Predicate -> String -> Boolean
check p prj =
  case p of
    S.Contains (S.Text str) -> MM.minimatch str prj
    S.Gt (S.Text str) -> compare str > 0
    S.Gte (S.Text str) -> compare str >= 0 
    S.Lt (S.Text str) -> compare str < 0 
    S.Lte (S.Text str) -> compare str <= 0
    S.Ne (S.Text str) -> compare str /= 0
    S.Eq (S.Text str) -> compare str == 0
    -- since we use _minimatch_ to check `Contains` predicate
    -- `Like` predicate works exactly like `Contains` if we
    -- replace `% -> *` and `_ -> ?`
    S.Like s -> flip check prj $ S.Contains $ S.Text $ like2glob s
    S.Contains (S.Range str str') ->
      let c = flip check prj in
      (c (S.Gte (S.Text str)) && c (S.Lte (S.Text str'))) ||
      (c (S.Lte (S.Text str)) && c (S.Gte (S.Text str')))
    -- filters only when it means something. S.Eq S.Range - meaningless
    -- searching by tag in this context has no meaning too
    _ -> true
  where compare = Str.localeCompare prj
        like2glob str =
          Rgx.replace percentRgx "*" $ Rgx.replace underscoreRgx "?" $ str
          where percentRgx = Rgx.regex "%" flags
                underscoreRgx = Rgx.regex "_" flags
                flags = Rgx.noFlags{global = true}

-- | Filtering function for items and predicates
filterByTerm :: Mi.Item -> S.Term -> Boolean
filterByTerm {name: name, resource: resource}
  (S.Term {predicate: predicate, labels: labels, include: include}) =
  let res = Mr.resource2str resource
      -- checking _include_ field in predicate
      mbNot :: Boolean -> Boolean
      mbNot = if include then id else not
      check' str = mbNot $ check predicate str
  in
  case labels of
    -- no labels -> check by both fields
    [] -> check' name || check' res
    -- we've already checked _path_ when had got it from backend
    [S.Common "path"] -> true
    -- check _name_
    [(S.Common "name")] -> check' name
    -- check _type_
    [(S.Common "type")] -> check' res
    -- check _type_ 
    [(S.Common "resource")] -> check' res 
    _ -> false 

-- | Filter by full search query 
filterByQuery :: S.SearchQuery -> Mi.Item -> Boolean
filterByQuery query item = 
  S.check item query filterByTerm
  
-- | Extract path predicate from search query
searchPath :: S.SearchQuery -> Maybe String
searchPath query =
  runFirst $ foldMap fn query 
  where fn term = First case term of
          S.Term {include: true,
                  predicate: S.Contains (S.Text path),
                  labels: [S.Common "path"]} -> Just path
          _ -> Nothing

----------------------------------------------------------------------
--        Setters
----------------------------------------------------------------------
sortRgx :: Rgx.Regex
sortRgx = Rgx.regex "(sort=)([^/&]*)" Rgx.noFlags

qRgx :: Rgx.Regex
qRgx = Rgx.regex "(q=)([^/&]*)" Rgx.noFlags

updateSort :: Ms.Sort -> String -> String
updateSort sort = 
  Rgx.replace sortRgx res
  where res = "$1" <> Ms.sort2string sort
        
updateQ :: String -> String -> String
updateQ q = 
  Rgx.replace qRgx res 
  where res = "$1" <> U.encodeURIComponent q


setSort :: Ms.Sort -> String
setSort sort = "?sort=" <> Ms.sort2string sort <> "&q="

-- | extract path from full hash
getPath :: String -> String
getPath hash = 
  fromMaybe "" do
    matches <- Rgx.match qRgx hash
    q <- U.decodeURIComponent <$> matches !! 2
    query <- either (const Nothing) Just (S.mkQuery q)
    searchPath query

-- | set _path_ value in path-labeled predicate in route
updatePath :: String -> String -> String
updatePath str oldHash =
  let pathOld = "path:" <> getPath oldHash
      pathNew = "path:" <> escape str 
      replaced = Str.replace pathOld pathNew oldHash
  in if replaced == oldHash then
       updateQ pathNew oldHash
     else
       replaced
  where escape :: String -> String
        escape str = if Str.indexOf " " str /= -1 then
                       "\"" <> str <> "\""
                       else str
                            
-- | Add to _path_ value in path-labeled predicate 
addToPath :: String -> String -> String
addToPath str oldHash = 
  let pathOld = getPath oldHash
      pathNew = flip (<>) "/" $
                if str /= ".." then
                  pathOld <> str 
                else
                  Str.joinWith "/" $
                  reverse $ drop 1 $ reverse $ 
                  filter (/= "") $ Str.split "/" $ U.trimQuotes pathOld
    in updatePath pathNew oldHash

