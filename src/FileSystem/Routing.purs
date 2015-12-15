{-
Copyright 2015 SlamData, Inc.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
-}

module FileSystem.Routing
 ( Routes(..)
 , routing
 , routeSignal
 ) where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Aff (Aff(), Canceler(), cancel, forkAff, attempt)
import Control.Monad.Aff.AVar (makeVar', takeVar, putVar, modifyVar, AVar())
import Control.Monad.Eff.Class (MonadEff, liftEff)
import Control.Monad.Eff.Exception (error, message, Error())
import Control.MonadPlus (guard)
import Control.UI.Browser (replaceLocation)

import Data.Array (filter, mapMaybe)
import Data.Either (Either(..), either)
import Data.Foldable (foldl, traverse_)
import Data.Functor (($>))
import Data.Functor.Coproduct (left)
import Data.Lens ((%~), (<>~), _1, _2)
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Monoid (mempty)
import Data.Path.Pathy ((</>), rootDir, parseAbsDir, sandbox, currentDir)
import Data.These (These(..))
import Data.Tuple (Tuple(..), uncurry)

import Halogen.Driver (Driver())
import Halogen.Query (action)

import Text.SlamSearch (mkQuery)
import Text.SlamSearch.Printer (strQuery)
import Text.SlamSearch.Types (SearchQuery())

import FileSystem
import FileSystem.Dialog as Dialog
import FileSystem.Effects
import FileSystem.Items as Items
import FileSystem.Routing.Search (isSearchQuery, searchPath, filterByQuery)
import FileSystem.Search as Search
import Model.Common (browseURL)
import Model.Item (Item(..))
import Model.Resource (Resource(..), getPath)
import Model.Salt (Salt(..), newSalt)
import Model.Sort (Sort(..), string2sort)
import Quasar.Aff (mountInfo, children)
import Routing (matchesAff)
import Routing.Match (Match(), eitherMatch)
import Routing.Match.Class (param)
import Utils.Path (DirPath(), hidePath, renderPath)

data Routes
  = Salted Sort SearchQuery Salt
  | SortAndQ Sort SearchQuery
  | Sort Sort
  | Index

routing :: Match Routes
routing = salted <|> bothRoute <|> oneRoute <|> index
  where
  salted = Salted <$> sort <*> query <*> salt
  bothRoute = SortAndQ <$> sort <*> query
  oneRoute = Sort <$> sort
  index = pure Index
  sort = eitherMatch (string2sort <$> param "sort")
  query = eitherMatch (mkQuery <$> param "q")
  salt = eitherMatch (getSalt <$> param "salt")

getSalt :: String -> Either String Salt
getSalt input =
  if input /= ""
  then Right $ Salt input
  else Left "incorrect salt"

initialAVar :: Tuple (Canceler FileSystemEffects) (M.Map Int Int)
initialAVar = Tuple mempty M.empty

routeSignal :: Driver QueryP FileSystemRawEffects
               -> Aff FileSystemEffects Unit
routeSignal driver = do
  avar <- makeVar' initialAVar
  routeTpl <- matchesAff routing
  pure unit
  uncurry (redirects driver avar) routeTpl


redirects :: Driver QueryP FileSystemRawEffects
          -> AVar (Tuple (Canceler FileSystemEffects) (M.Map Int Int))
          -> Maybe Routes -> Routes
          -> Aff FileSystemEffects Unit
redirects _ _ _ Index = updateURL Nothing Asc Nothing rootDir
redirects _ _ _ (Sort sort) = updateURL Nothing sort Nothing rootDir
redirects _ _ _ (SortAndQ sort query) =
  let queryParts = splitQuery query
  in updateURL queryParts.query sort Nothing queryParts.path
redirects driver var mbOld (Salted sort query salt) = do
  Tuple canceler _ <- takeVar var
  cancel canceler $ error "cancel search"
  putVar var initialAVar
  driver $ toItems $ Items.SetIsSearching $ isSearchQuery query
  if isNewPage
    then do
    driver $ toItems Items.Reset
    driver $ toFs $ SetPath queryParts.path
    driver $ toFs $ SetSort sort
    driver $ toFs $ SetSalt salt
    driver $ toFs $ SetIsMount false
    driver $ toSearch $ Search.SetLoading true
    driver $ toSearch $ Search.SetValue $ maybe (This "") That queryParts.query
    driver $ toSearch $ Search.SetValid true
    listPath query zero var queryParts.path driver
    maybe (checkMount queryParts.path driver) (const $ pure unit) queryParts.query
    else
    driver $ toSearch $ Search.SetLoading false
  where

  queryParts = splitQuery query
  isNewPage = fromMaybe true do
    old <- mbOld
    Tuple oldQuery oldSalt <- case old of
      Salted _ oldQuery oldSalt -> pure $ Tuple oldQuery oldSalt
      _ -> Nothing
    pure $ oldQuery /= query || oldSalt == salt




checkMount :: DirPath -> Driver QueryP FileSystemRawEffects
              -> Aff FileSystemEffects Unit
checkMount path driver = do
  result <- attempt $ mountInfo (Database path)
  case result of
    Left _ -> pure unit
    Right _ -> driver $ left $ action $ SetIsMount true

listPath :: SearchQuery -> Int
            -> AVar (Tuple (Canceler FileSystemEffects) (M.Map Int Int))
            -> DirPath -> Driver QueryP FileSystemRawEffects
            -> Aff FileSystemEffects Unit
listPath query deep var dir driver = do
  modifyVar (_2 %~ M.alter (maybe one (add one >>> pure))  deep) var
  canceler <- forkAff goDeeper
  modifyVar (_1 <>~ canceler) var
  where
  goDeeper = do
    (attempt $ children dir) >>= either sendError getChildren
    modifyVar (_2 %~ M.update (\v -> guard (v > one) $> (v - one)) deep) var
    Tuple c r <- takeVar var
    if (foldl (+) zero $ M.values r) == zero
      then do
      driver $ toSearch $ Search.SetLoading false
      putVar var initialAVar
      else
      putVar var (Tuple c r)

  sendError :: Error -> Aff FileSystemEffects Unit
  sendError err =
    driver $ toDialog $ Dialog.Show
    $ Dialog.Error ("There is a problem listing current directory: "
                   <> message err)


  getChildren :: Array Resource -> Aff FileSystemEffects Unit
  getChildren ress = do
    let next = mapMaybe (either (const Nothing) Just <<< getPath) ress
        toAdd = map Item $ filter (filterByQuery query) ress

    driver $ toItems $ Items.Adds toAdd
    traverse_ (\n -> listPath query (deep + one) var n driver)
      $ if isSearchQuery query
        then do
          next
        else [ ]

updateURL :: Maybe String -> Sort -> Maybe Salt -> DirPath
             -> Aff FileSystemEffects Unit
updateURL query sort salt path = liftEff do
  salt' <- maybe newSalt pure salt
  replaceLocation $ browseURL query sort salt' path


splitQuery :: SearchQuery -> { path :: DirPath, query :: Maybe String }
splitQuery q =
  { path: path
  , query: query
  }
  where
  path = rootDir </> fromMaybe currentDir
         (searchPath q >>= parseAbsDir >>= sandbox rootDir)
  query = do
    guard $ isSearchQuery q
    pure $ hidePath (renderPath $ Right path) (strQuery q)
