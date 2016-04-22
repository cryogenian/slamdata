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

import SlamData.Prelude

import Ace.Config as AceConfig

import Control.Monad.Aff (Aff, Canceler, cancel, forkAff)
import Control.Monad.Aff.AVar (makeVar', takeVar, putVar, modifyVar, AVar)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (error, message, Error)
import Control.UI.Browser (setTitle, replaceLocation)

import Data.Array (filter, mapMaybe)
import Data.Lens ((%~), (<>~), _1, _2)
import Data.Map as M
import Data.Path.Pathy ((</>), rootDir, parseAbsDir, sandbox, currentDir)
import Data.These (These(..))

import DOM (DOM)

import Halogen.Component (parentState)
import Halogen.Driver (Driver, runUI)
import Halogen.Query (action)
import Halogen.Util (runHalogenAff, awaitBody)

import Routing (matchesAff)

import SlamData.Config as Config
import SlamData.Config.Version (slamDataVersion)
import SlamData.Effects (SlamDataEffects, SlamDataRawEffects)
import SlamData.FileSystem.Component (QueryP, Query(..), toListing, toDialog, toSearch, toFs, initialState, comp)
import SlamData.FileSystem.Dialog.Component as Dialog
import SlamData.FileSystem.Listing.Component as Listing
import SlamData.FileSystem.Listing.Item (Item(..))
import SlamData.FileSystem.Listing.Sort (Sort(..))
import SlamData.FileSystem.Resource (Resource, getPath)
import SlamData.FileSystem.Routing (Routes(..), routing, browseURL)
import SlamData.FileSystem.Routing.Salt (Salt, newSalt)
import SlamData.FileSystem.Routing.Search (isSearchQuery, searchPath, filterByQuery)
import SlamData.FileSystem.Search.Component as Search
import SlamData.Quasar.FS (children) as Quasar
import SlamData.Quasar.Mount (mountInfo) as Quasar

import Text.SlamSearch.Printer (strQuery)
import Text.SlamSearch.Types (SearchQuery)

import Utils.Path (DirPath, hidePath, renderPath)

main ∷ Eff SlamDataEffects Unit
main = do
  AceConfig.set AceConfig.basePath (Config.baseUrl ⊕ "js/ace")
  AceConfig.set AceConfig.modePath (Config.baseUrl ⊕ "js/ace")
  AceConfig.set AceConfig.themePath (Config.baseUrl ⊕ "js/ace")
  runHalogenAff do
    driver ← runUI comp (parentState initialState) =<< awaitBody
    forkAff do
      setSlamDataTitle slamDataVersion
      driver (left $ action $ SetVersion slamDataVersion)
    forkAff $ routeSignal driver

setSlamDataTitle ∷ ∀ e. String → Aff (dom ∷ DOM|e) Unit
setSlamDataTitle version =
  liftEff $ setTitle $ "SlamData " ⊕ version

initialAVar ∷ Tuple (Canceler SlamDataEffects) (M.Map Int Int)
initialAVar = Tuple mempty M.empty

routeSignal
  ∷ Driver QueryP SlamDataRawEffects
  → Aff SlamDataEffects Unit
routeSignal driver = do
  avar ← makeVar' initialAVar
  routeTpl ← matchesAff routing
  pure unit
  uncurry (redirects driver avar) routeTpl


redirects
  ∷ Driver QueryP SlamDataRawEffects
  → AVar (Tuple (Canceler SlamDataEffects) (M.Map Int Int))
  → Maybe Routes → Routes
  → Aff SlamDataEffects Unit
redirects _ _ _ Index = updateURL Nothing Asc Nothing rootDir
redirects _ _ _ (Sort sort) = updateURL Nothing sort Nothing rootDir
redirects _ _ _ (SortAndQ sort query) =
  let queryParts = splitQuery query
  in updateURL queryParts.query sort Nothing queryParts.path
redirects driver var mbOld (Salted sort query salt) = do
  Tuple canceler _ ← takeVar var
  cancel canceler $ error "cancel search"
  putVar var initialAVar
  driver $ toListing $ Listing.SetIsSearching $ isSearchQuery query
  if isNewPage
    then do
    driver $ toListing Listing.Reset
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
    old ← mbOld
    Tuple oldQuery oldSalt ← case old of
      Salted _ oldQuery' oldSalt' → pure $ Tuple oldQuery' oldSalt'
      _ → Nothing
    pure $ oldQuery ≠ query ∨ oldSalt ≡ salt

checkMount
  ∷ DirPath
  → Driver QueryP SlamDataRawEffects
  → Aff SlamDataEffects Unit
checkMount path driver = do
  result ← Quasar.mountInfo path
  case result of
    Left _ → pure unit
    Right _ → driver $ left $ action $ SetIsMount true

listPath
  ∷ SearchQuery
  → Int
  → AVar (Tuple (Canceler SlamDataEffects) (M.Map Int Int))
  → DirPath
  → Driver QueryP SlamDataRawEffects
  → Aff SlamDataEffects Unit
listPath query deep var dir driver = do
  modifyVar (_2 %~ M.alter (maybe one (add one >>> pure))  deep) var
  canceler ← forkAff goDeeper
  modifyVar (_1 <>~ canceler) var
  where
  goDeeper = do
    Quasar.children dir >>= either sendError getChildren
    modifyVar (_2 %~ M.update (\v → guard (v > one) $> (v - one)) deep) var
    Tuple c r ← takeVar var
    if (foldl (+) zero $ M.values r) ≡ zero
      then do
      driver $ toSearch $ Search.SetLoading false
      putVar var initialAVar
      else
      putVar var (Tuple c r)

  sendError ∷ Error → Aff SlamDataEffects Unit
  sendError err =
    when ((not $ isSearchQuery query) ∨ deep ≡ zero)
    $ driver $ toDialog $ Dialog.Show
    $ Dialog.Error ("There is a problem listing current directory: "
                   ⊕ message err)


  getChildren ∷ Array Resource → Aff SlamDataEffects Unit
  getChildren ress = do
    let next = mapMaybe (either Just (const Nothing) <<< getPath) ress
        toAdd = map Item $ filter (filterByQuery query) ress

    driver $ toListing $ Listing.Adds toAdd
    traverse_ (\n → listPath query (deep + one) var n driver)
      (guard (isSearchQuery query) *> next)


updateURL
  ∷ Maybe String
  → Sort
  → Maybe Salt
  → DirPath
  → Aff SlamDataEffects Unit
updateURL query sort salt path = liftEff do
  salt' ← maybe newSalt pure salt
  replaceLocation $ browseURL query sort salt' path


splitQuery
  ∷ SearchQuery
  → { path ∷ DirPath, query ∷ Maybe String }
splitQuery q =
  { path: path
  , query: query
  }
  where
  path =
    rootDir </> fromMaybe currentDir
      (searchPath q >>= parseAbsDir >>= sandbox rootDir)
  query = do
    guard $ isSearchQuery q
    pure $ hidePath (renderPath $ Left path) (strQuery q)
