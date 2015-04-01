-- | Module handles outer messages to `halogen` application
-- | Mostly consists of routing functions 
module Controller.Driver (
  outside,
  getPath,
  searchPath,
  PathSetter(..),
  SortSetter(..),
  QSetter(..),
  PathAdder(..)
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
import qualified Routing.Setter as RS 
import qualified Text.SlamSearch as S
import qualified Text.SlamSearch.Types as S
import qualified Text.SlamSearch.Printer as S
import qualified Data.Minimatch as MM
import qualified Control.Monad.Aff as Aff
import qualified Model as M
import qualified Api.Fs as Api
import qualified Network.HTTP.Affjax as Af

-- | Entry, used in `halogen` app
outside :: forall e. Hl.Driver M.Input (timer :: Tm.Timer, ajax :: Af.Ajax|e) ->
           Eff (Hl.HalogenEffects (timer :: Tm.Timer, ajax :: Af.Ajax|e)) Unit
outside driver = handleRoute driver 

-- | Routing schema
data Routes = SortAndQ M.Sort S.SearchQuery | Sort M.Sort | Index 

-- | Route parsing match objects
routing :: R.Match Routes
routing = bothRoute <|> oneRoute <|> index
  where bothRoute = SortAndQ <$> sort <*> query
        oneRoute = Sort <$> sort
        index = pure Index
        sort = R.lit "sort" *>
               ((R.lit "asc" *> pure M.Asc) <|> (R.lit "desc" *> pure M.Desc))

        query = R.eitherMatch (S.mkQuery <$> R.param "q")

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
--          
          (Api.Metadata {children: cs}) <- _.response <$> Api.meta path
          liftEff $ U.log cs

        Api.metadata path $ \items -> do
          driver $ M.ItemsUpdate $
            (_{root = path}) <$> 
            (if (path /= "/" && path /= "") then [M.upLink] else []) <>
            (filter (filterByQuery query) items)
          driver $ M.Sorting sort
      -- Fired by default -> redirect to sorted route
      Index -> do
        RS.setRouteState $ SortSetter M.Asc
      -- Only sort setted -> redirects to `q=` 
      Sort sort -> do
        driver $ M.SearchSet ""
        driver $ M.SetPath ""
        Api.metadata "" $ \items -> do
          driver $ M.ItemsUpdate items
          driver $ M.Sorting sort
          
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
filterByTerm :: M.Item -> S.Term -> Boolean
filterByTerm {name: name, resource: resource}
  (S.Term {predicate: predicate, labels: labels, include: include}) =
  let res = M.resource2str resource
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
filterByQuery :: S.SearchQuery -> M.Item -> Boolean
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
sortRgx = Rgx.regex "(sort/)([^/]*)" Rgx.noFlags

qRgx :: Rgx.Regex
qRgx = Rgx.regex "(q=)([^/&]*)" Rgx.noFlags

-- | Set _sort_ in route
data SortSetter = SortSetter M.Sort

instance routeModifierSortSetter :: RS.RouteModifier SortSetter where
  toHashModifier (SortSetter sort) =
    Rgx.replace sortRgx res
    where res = "$1" <> M.sort2string sort 

-- | Set _q_ in route
data QSetter = QSetter String

instance routeModifierQSetter :: RS.RouteModifier QSetter where
  toHashModifier (QSetter str) =
    Rgx.replace qRgx res 
    where res = "$1" <> U.encodeURIComponent str

instance routeSetterSortSetter :: RS.RouteState SortSetter where
  toHash (SortSetter sort) = "sort/" <> M.sort2string sort <> "/?q="

-- | extract path from full hash
getPath :: String -> String
getPath hash = 
  fromMaybe "" do
    matches <- Rgx.match qRgx hash
    q <- U.decodeURIComponent <$> matches !! 2
    query <- either (const Nothing) Just (S.mkQuery q)
    searchPath query

-- | set _path_ value in path-labeled predicate in route
newtype PathSetter = PathSetter String

instance routeModifierPathSetter :: RS.RouteModifier PathSetter where
  toHashModifier (PathSetter str) oldHash =
    let pathOld = "path:" <> getPath oldHash
        pathNew = "path:" <>  str 
        replaced = Str.replace pathOld pathNew oldHash
    in if replaced == oldHash then
         RS.toHashModifier (QSetter pathNew) oldHash 
       else
         replaced

-- | Add to _path_ value in path-labeled predicate 
newtype PathAdder = PathAdder String

instance routeModifierPathAdder :: RS.RouteModifier PathAdder where
  toHashModifier (PathAdder str) oldHash =
    let pathOld = getPath oldHash
        pathNew = flip (<>) "/" $
                  if str /= ".." then
                    pathOld <> str 
                  else
                    Str.joinWith "/" $
                    reverse $ drop 1 $ reverse $ 
                    filter (/= "") $ Str.split "/" pathOld
    in RS.toHashModifier (PathSetter pathNew) oldHash


