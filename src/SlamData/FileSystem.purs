{-
Copyright 2016 SlamData, Inc.

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


module SlamData.FileSystem (main) where

import Prelude

import Ace.Config as AceConfig

import Control.Apply ((*>))
import Control.Monad (when)
import Control.Monad.Aff (Aff(), Canceler(), cancel, runAff, forkAff, attempt)
import Control.Monad.Aff.AVar (makeVar', takeVar, putVar, modifyVar, AVar())
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Exception (error, message, Error(), throwException)
import Control.MonadPlus (guard)
import Control.UI.Browser (setTitle, replaceLocation)

import Data.Array (filter, mapMaybe)
import Data.Either (Either(..), either)
import Data.Foldable (foldl, traverse_)
import Data.Functor (($>))
import Data.Functor.Coproduct (left)
import Data.Functor.Eff (liftEff)
import Data.Lens ((%~), (<>~), _1, _2)
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Monoid (mempty)
import Data.Path.Pathy ((</>), rootDir, parseAbsDir, sandbox, currentDir)
import Data.These (These(..))
import Data.Tuple (Tuple(..), uncurry)

import DOM (DOM())

import Halogen.Component (installedState)
import Halogen.Driver (Driver())
import Halogen.Driver (runUI)
import Halogen.Query (action)
import Halogen.Util (appendToBody, onLoad)

import Quasar.Aff (mountInfo, children)

import Routing (matchesAff)

import SlamData.Config as Config
import SlamData.Config.Version as Version
import SlamData.FileSystem.Component
import SlamData.FileSystem.Dialog.Component as Dialog
import SlamData.FileSystem.Effects
import SlamData.FileSystem.Listing.Component as Items
import SlamData.FileSystem.Listing.Item (Item(..))
import SlamData.FileSystem.Listing.Sort (Sort(..))
import SlamData.FileSystem.Resource (Resource(..), getPath)
import SlamData.FileSystem.Routing (Routes(..), routing, browseURL)
import SlamData.FileSystem.Routing.Salt (Salt(), newSalt)
import SlamData.FileSystem.Routing.Search (isSearchQuery, searchPath, filterByQuery)
import SlamData.FileSystem.Search.Component as Search
import SlamData.StylesContainer.Component as Styles
import SlamData.StylesContainer.Model as Styles

import Text.SlamSearch.Printer (strQuery)
import Text.SlamSearch.Types (SearchQuery())

import Utils.Path (DirPath(), hidePath, renderPath)

main :: Eff FileSystemEffects Unit
main = do
  AceConfig.set AceConfig.basePath (Config.baseUrl ++ "js/ace")
  AceConfig.set AceConfig.modePath (Config.baseUrl ++ "js/ace")
  AceConfig.set AceConfig.themePath (Config.baseUrl ++ "js/ace")
  runAff throwException (const (pure unit)) do
    halogen <- runUI comp (installedState initialState)
    onLoad (appendToBody halogen.node)
    forkAff do
      let version = Version.slamDataVersion
      setSlamDataTitle version
      halogen.driver (left $ action $ SetVersion version)
    forkAff $ routeSignal halogen.driver

setSlamDataTitle :: forall e. String -> Aff (dom :: DOM|e) Unit
setSlamDataTitle version =
  liftEff $ setTitle $ "SlamData " <> version

initialAVar :: Tuple (Canceler FileSystemEffects) (M.Map Int Int)
initialAVar = Tuple mempty M.empty

routeSignal :: Driver QueryP FileSystemRawEffects
               -> Aff FileSystemEffects Unit
routeSignal driver = do
  avar <- makeVar' initialAVar
  routeTpl <- matchesAff routing
  Debug.Trace.traceAnyA routeTpl
  uncurry (redirects driver avar) routeTpl


redirects
  :: Driver QueryP FileSystemRawEffects
  -> AVar (Tuple (Canceler FileSystemEffects) (M.Map Int Int))
  -> Maybe (Tuple Routes (Array Styles.StyleURL))
  -> Tuple Routes (Array Styles.StyleURL)
  -> Aff FileSystemEffects Unit
redirects _ _ _ (Tuple Index ss) = updateURL Nothing Asc Nothing rootDir ss
redirects _ _ _ (Tuple (Sort sort) ss) = updateURL Nothing sort Nothing rootDir ss
redirects _ _ _ (Tuple (SortAndQ sort query) ss) =
  let queryParts = splitQuery query
  in updateURL queryParts.query sort Nothing queryParts.path ss
redirects driver var mbOld (Tuple (Salted sort query salt) styles) = do
  Tuple canceler _ <- takeVar var
  cancel canceler $ error "cancel search"
  putVar var initialAVar
  Debug.Trace.traceAnyA styles
  driver $ toItems $ Items.SetIsSearching $ isSearchQuery query

  driver $ toFs $ SetStyleSheets
    $ (map Styles.StyleURL SlamData.Config.defaultStyleSheets) <> styles
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
      Tuple (Salted _ oldQuery oldSalt) _ -> pure $ Tuple oldQuery oldSalt
      _ -> Nothing
    pure $ oldQuery /= query || oldSalt == salt

checkMount
  :: DirPath
  -> Driver QueryP FileSystemRawEffects
  -> Aff FileSystemEffects Unit
checkMount path driver = do
  result <- attempt $ mountInfo (Database path)
  case result of
    Left _ -> pure unit
    Right _ -> driver $ left $ action $ SetIsMount true

listPath
  :: SearchQuery
  -> Int
  -> AVar (Tuple (Canceler FileSystemEffects) (M.Map Int Int))
  -> DirPath
  -> Driver QueryP FileSystemRawEffects
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
    when ((not $ isSearchQuery query) || deep == zero)
    $ driver $ toDialog $ Dialog.Show
    $ Dialog.Error ("There is a problem listing current directory: "
                   <> message err)


  getChildren :: Array Resource -> Aff FileSystemEffects Unit
  getChildren ress = do
    let next = mapMaybe (either (const Nothing) Just <<< getPath) ress
        toAdd = map Item $ filter (filterByQuery query) ress

    driver $ toItems $ Items.Adds toAdd
    traverse_ (\n -> listPath query (deep + one) var n driver)
      (guard (isSearchQuery query) *> next)


updateURL
  :: Maybe String
  -> Sort
  -> Maybe Salt
  -> DirPath
  -> Array Styles.StyleURL
  -> Aff FileSystemEffects Unit
updateURL query sort salt path stylesheets = liftEff do
  salt' <- maybe newSalt pure salt
  replaceLocation $ browseURL query sort salt' path stylesheets


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
