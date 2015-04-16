-- | Module handles outer messages to `halogen` application
-- | Mostly consists of routing functions 
module Driver.File (
  outside,
  getPath,
  searchPath,
  updateSort,
  updateQ,
  updateSalt,
  setSort,
  updatePath
  ) where

import Control.Monad.Eff
import Control.Monad.Eff.Class
import Control.Monad.Eff.Random
import Control.Monad.Eff.Exception
import Control.Apply
import Control.Alt
import Data.Monoid.First
import Data.Foldable
import Data.Traversable
import Data.Maybe
import Data.Either
import Data.Tuple
import Data.Semiring.Free
import Data.Monoid
import Data.Array (filter, reverse, (!!), drop, length, take)
import Optic.Refractor.Lens
import Optic.Core

import qualified Utils as U
import qualified Data.String as Str
import qualified Data.String.Regex as Rgx
import qualified Data.Map as M 
import qualified Control.Timer as Tm
import qualified Halogen as Hl
import qualified Routing as R
import qualified Routing.Match as R
import qualified Routing.Match.Class as R
import qualified Routing.Hash.Aff as Rh
import qualified Text.SlamSearch as S
import qualified Text.SlamSearch.Types as S
import qualified Text.SlamSearch.Printer as S
import qualified Data.Minimatch as MM
import qualified Control.Monad.Aff as Aff
import qualified Control.Monad.Aff.AVar as A
import qualified Model.File as M
import qualified Model.Item as Mi
import qualified Model.Sort as Ms
import qualified Model.Path as Mp
import qualified Model.Resource as Mr
import qualified Api.Fs as Api
import qualified Network.HTTP.Affjax as Af
import EffectTypes 


-- | Entry, used in `halogen` app
outside :: forall e. Hl.Driver M.Input (FileComponentEff e) -> 
           Eff (FileAppEff e) Unit
outside driver = handleRoute driver 

-- | Routing schema
data Routes
  = Salted Ms.Sort S.SearchQuery String
  | SortAndQ Ms.Sort S.SearchQuery 
  | Sort Ms.Sort
  | Index 

getSalt :: String -> Either String String
getSalt input =
  if input /= "" then Right input
  else Left "incorrect salt"

-- | Route parsing match objects
routing :: R.Match Routes
routing = salted <|> bothRoute <|> oneRoute <|> index
  where salted = Salted <$> sort <*> query <*> salt 
        bothRoute = SortAndQ <$> sort <*> query 
        oneRoute = Sort <$> sort
        index = pure Index
        sort = R.eitherMatch (Ms.string2sort <$> R.param "sort")
        query = R.eitherMatch (S.mkQuery <$> R.param "q")
        salt = R.eitherMatch (getSalt <$> R.param "salt")

-- | Stream of routes 
handleRoute :: forall e.
               Hl.Driver M.Input (FileComponentEff e) -> 
               Eff (FileAppEff e) Unit
handleRoute driver = 
  Aff.launchAff $ do
    var <- A.makeVar' initialAVar
    Tuple mbOld new <- R.matchesAff routing

    case new of
      Salted sort query salt -> do
        let newPage = maybe true id $ do
              old <- mbOld
              Tuple oldQuery oldSalt <- case old of
                Salted _ oldQuery oldSalt -> pure $ Tuple oldQuery oldSalt
                _ -> Nothing
              pure $ oldQuery /= query || oldSalt == salt
        Tuple c _ <- A.takeVar var
        Aff.cancel c $ error "cancel search"
        A.putVar var initialAVar
        liftEff (driver $ M.Loading false)
        if newPage then do  
          let path = Mp.cleanPath $ fromMaybe "/" $ searchPath query
          liftEff $ do
            driver $ M.Loading true
            driver $ M.SetPath path 
            driver $ M.SearchSet (Mp.hidePath path $ S.strQuery query)
            driver $ M.ItemsUpdate [] sort
            driver $ M.SetSearching (isSearchQuery query)
          listPath query 0 var path
          else do
          pure unit
          
      Index -> do
        Rh.setHash $ setSort Ms.Asc 
      Sort sort -> do
        Rh.modifyHash $ updatePath "/"
      SortAndQ sort query -> do
        rnd <- show <$> (liftEff $ randomInt 1000000 2000000)
        Rh.modifyHash $ updateSalt rnd
    where initialAVar = Tuple mempty M.empty
          
          listPath :: S.SearchQuery -> Number ->
                      A.AVar (Tuple (Aff.Canceler _) (M.Map Number Number)) ->
                      String -> Aff.Aff _ Unit
          listPath query deep var path = do
            A.modifyVar
              (\t -> t # _2 %~
                     M.alter (maybe (Just 1) (\x -> Just (x + 1))) deep) var

            canceler <- Aff.forkAff do
              ei <- Aff.attempt $ Api.listing path
              case ei of
                Right unfiltered -> do
                  let items = _{root = path} <$> unfiltered
                      test = filterByQuery query
                      children = filter (\x -> x.resource == Mr.Directory ||
                                               x.resource == Mr.Database) items
                  traverse_ (liftEff <<< driver <<< M.ItemAdd) $ 
                    filter test items
                  if isSearchQuery query then 
                    traverse_ ((listPath query (deep + 1) var) <<<
                               Mi.itemPath) items 
                    else pure unit

                _ -> pure unit 

              A.modifyVar
                (\t -> t # _2 %~ M.update (\v -> if v > 1 then Just (v - 1)
                                                 else Nothing) deep) var
                                              
              Tuple c r <- A.takeVar var
              if (foldl (+) 0 $ M.values r) == 0 then do
                liftEff do
                  driver $ M.Loading false
                A.putVar var initialAVar
                else
                A.putVar var (Tuple c r)            
            A.modifyVar (\t -> t # _1 %~ (<> canceler)) var

          
isSearchQuery :: S.SearchQuery -> Boolean
isSearchQuery query =
  not $ S.check unit query (\_ -> isNotSearchTerm)
  where isNotSearchTerm :: S.Term -> Boolean
        isNotSearchTerm (S.Term {predicate: p, labels: ls, include: i}) =
          case ls of
            [S.Common "path"] -> true
            _ -> false            
    
-- | check if string satisfies predicate from `purescript-search`
check :: S.Predicate -> String -> Boolean
check p prj =
  case p of
    S.Contains (S.Text str) -> match $ "*" <> str <> "*"
    S.Gt (S.Text str) -> compare str > 0
    S.Gte (S.Text str) -> compare str >= 0 
    S.Lt (S.Text str) -> compare str < 0 
    S.Lte (S.Text str) -> compare str <= 0
    S.Ne (S.Text str) -> compare str /= 0
    S.Eq (S.Text str) -> compare str == 0
    -- since we use _minimatch_ to check `Contains` predicate
    -- `Like` predicate works exactly like `Contains` if we
    -- replace `% -> *` and `_ -> ?`
    S.Like s -> match $ like2glob s 
    S.Contains (S.Range str str') ->
      let c = flip check prj in
      (c (S.Gte (S.Text str)) && c (S.Lte (S.Text str'))) ||
      (c (S.Lte (S.Text str)) && c (S.Gte (S.Text str')))
    -- filters only when it means something. S.Eq S.Range - meaningless
    -- searching by tag in this context has no meaning too
    _ -> true
  where percentRgx = Rgx.regex "%" flags
        underscoreRgx = Rgx.regex "_" flags
        flags = Rgx.noFlags{global = true}
        match a = MM.minimatch (Str.toLower a) (Str.toLower prj)
        compare = Str.localeCompare prj
        like2glob str = 
          Rgx.replace percentRgx "*" $ Rgx.replace underscoreRgx "?" $ str


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

saltRgx :: Rgx.Regex
saltRgx = Rgx.regex "(salt=)([^/&]*)" Rgx.noFlags

updateSort :: Ms.Sort -> String -> String
updateSort sort = 
  Rgx.replace sortRgx res
  where res = "$1" <> Ms.sort2string sort
        
updateQ :: String -> String -> String
updateQ q = 
  Rgx.replace qRgx res 
  where res = "$1" <> U.encodeURIComponent q

updateSalt :: String -> String -> String
updateSalt salt old =
  if attempt /= old then attempt
  else updateSalt (salt <> salt) old
  where attempt = Rgx.replace saltRgx res old
        res = "$1" <> salt

setSort :: Ms.Sort -> String
setSort sort = "?sort=" <> Ms.sort2string sort <> "&q=&salt="

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
  where upped :: String -> String
        upped input =
          let arr = reverse $ filter (/= "") $ Str.split "/" $ U.trimQuotes input in
          ("/" <>) $ 
          flip (<>) "/" $ 
          Str.joinWith "/" $
          reverse $
          case take 1 arr of 
            [".."] -> drop 2 arr
            _ -> arr
        escape :: String -> String
        escape str =
          Str.replace "//" "/" $ 
          (\x -> if Str.indexOf " " x /= -1 then
                   "\"" <> x <> "\""
                 else x) $
          upped str
