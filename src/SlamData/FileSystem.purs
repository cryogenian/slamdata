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

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Free (fromAff, fromEff)
import Control.Monad.Aff.AVar (AVAR, makeVar', takeVar, putVar, modifyVar, AVar)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Fork (fork, Canceler, cancel)
import Control.Monad.Eff.Exception (error)
import Control.UI.Browser (setTitle, replaceLocation)

import Data.Array (null, filter, mapMaybe)
import Data.Lens ((%~), (<>~), _1, _2)
import Data.Map as M
import Data.Path.Pathy ((</>), rootDir, parseAbsDir, sandbox, currentDir)

import DOM (DOM)

import Halogen.Component (parentState, interpret)
import Halogen.Driver (Driver, runUI)
import Halogen.Query (action)
import Halogen.Util (runHalogenAff, awaitBody)

import Quasar.Error as QE

import Routing (matchesAff)

import SlamData.Analytics as Analytics
import SlamData.Common.Sort (Sort(..))
import SlamData.Config as Config
import SlamData.Config.Version (slamDataVersion)
import SlamData.Effects (SlamDataEffects, SlamDataRawEffects)
import SlamData.FileSystem.Component (QueryP, Query(..), toListing, toDialog, toSearch, toFs, initialState, comp)
import SlamData.FileSystem.Dialog.Component as Dialog
import SlamData.FileSystem.Listing.Component as Listing
import SlamData.FileSystem.Listing.Item (Item(..))
import SlamData.FileSystem.Resource (Resource, getPath)
import SlamData.FileSystem.Routing (Routes(..), routing, browseURL)
import SlamData.FileSystem.Routing.Salt (Salt, newSalt)
import SlamData.FileSystem.Routing.Search (isSearchQuery, searchPath, filterByQuery)
import SlamData.FileSystem.Search.Component as Search
import SlamData.GlobalError as GE
import SlamData.Monad (Slam, runSlam)
import SlamData.Quasar.FS (children) as Quasar
import SlamData.Quasar.Mount (mountInfo) as Quasar
import SlamData.Wiring (makeWiring)

import Text.SlamSearch.Printer (strQuery)
import Text.SlamSearch.Types (SearchQuery)

import Utils.Path (DirPath, hidePath, renderPath)

main ∷ Eff SlamDataEffects Unit
main = do
  AceConfig.set AceConfig.basePath (Config.baseUrl ⊕ "js/ace")
  AceConfig.set AceConfig.modePath (Config.baseUrl ⊕ "js/ace")
  AceConfig.set AceConfig.themePath (Config.baseUrl ⊕ "js/ace")
  runHalogenAff do
    fork Analytics.enableAnalytics
    wiring ← makeWiring
    let ui = interpret (runSlam wiring) comp
    driver ← runUI ui (parentState initialState) =<< awaitBody
    fork do
      setSlamDataTitle slamDataVersion
      driver (left $ action $ SetVersion slamDataVersion)
    runSlam wiring $ fork $ routeSignal driver

setSlamDataTitle ∷ ∀ e. String → Aff (dom ∷ DOM|e) Unit
setSlamDataTitle version =
  liftEff $ setTitle $ "SlamData " ⊕ version

initialAVar ∷ Tuple (Canceler Slam) (M.Map Int Int)
initialAVar = Tuple mempty M.empty

routeSignal ∷ Driver QueryP SlamDataRawEffects → Slam Unit
routeSignal driver = do
  avar ← (fromAff :: forall eff. Aff (avar :: AVAR | eff) ~> Slam) $ makeVar' initialAVar
  routeTpl ← (fromAff :: Aff SlamDataEffects ~> Slam) $ matchesAff routing
  uncurry (redirects driver avar) routeTpl

redirects
  ∷ Driver QueryP SlamDataRawEffects
  → AVar (Tuple (Canceler Slam) (M.Map Int Int))
  → Maybe Routes → Routes
  → Slam Unit
redirects driver var mbOld = case _ of
  Index →
    updateURL Nothing Asc Nothing rootDir
  Sort sort →
    updateURL Nothing sort Nothing rootDir
  SortAndQ sort query →
    let queryParts = splitQuery query
    in updateURL queryParts.query sort Nothing queryParts.path
  Salted sort query salt → do
    Tuple canceler _ ← fromAff $ takeVar var
    cancel (error "cancel search") canceler
    fromAff $ putVar var initialAVar
    fromAff $ driver $ toListing $ Listing.SetIsSearching $ isSearchQuery query
    let
      queryParts = splitQuery query
      isNewPage = fromMaybe true do
        old ← mbOld
        Tuple oldQuery oldSalt ← case old of
          Salted _ oldQuery' oldSalt' → pure $ Tuple oldQuery' oldSalt'
          _ → Nothing
        pure $ oldQuery ≠ query ∨ oldSalt ≡ salt
    if isNewPage
      then do
        fromAff do
          driver $ toListing Listing.Reset
          driver $ toFs $ SetPath queryParts.path
          driver $ toFs $ SetSort sort
          driver $ toFs $ SetSalt salt
          driver $ toFs $ SetIsMount false
          driver $ toSearch $ Search.SetLoading true
          driver $ toSearch $ Search.SetValue $ fromMaybe "" queryParts.query
          driver $ toSearch $ Search.SetValid true
          driver $ toSearch $ Search.SetPath queryParts.path
        listPath query zero var queryParts.path driver
        maybe (checkMount queryParts.path driver) (const $ pure unit) queryParts.query
      else
        fromAff $ driver $ toSearch $ Search.SetLoading false

checkMount
  ∷ DirPath
  → Driver QueryP SlamDataRawEffects
  → Slam Unit
checkMount path driver = do
  Quasar.mountInfo path >>= case _ of
    Left _ →
      -- When Quasar has no mounts configured we want to enable the root to be
      -- configured as a mount - if `/` is not a mount and also has no children
      -- then we know it's in this unconfigured state.
      when (path == rootDir) do
        void $ Quasar.children path >>= traverse \children ->
          when (null children) $
            fromAff $ driver $ left $ action $ SetIsMount true
    Right _ →
      fromAff $ driver $ left $ action $ SetIsMount true

listPath
  ∷ SearchQuery
  → Int
  → AVar (Tuple (Canceler Slam) (M.Map Int Int))
  → DirPath
  → Driver QueryP SlamDataRawEffects
  → Slam Unit
listPath query deep var dir driver = do
  fromAff $ modifyVar (_2 %~ M.alter (pure ∘ maybe 1 (_ + 1)) deep) var
  canceler ← fork goDeeper
  fromAff $ modifyVar (_1 <>~ canceler) var
  where
  goDeeper = do
    Quasar.children dir >>= either sendError getChildren
    fromAff do
      modifyVar (_2 %~ M.update (\v → guard (v > one) $> (v - one)) deep) var
      Tuple c r ← takeVar var
      if (foldl (+) zero $ M.values r) ≡ zero
        then do
        driver $ toSearch $ Search.SetLoading false
        putVar var initialAVar
        else
        putVar var (Tuple c r)

  sendError ∷ QE.QError → Slam Unit
  sendError err =
    case GE.fromQError err of
      Left msg →
        presentError $
          "There was a problem accessing this directory listing. " <> msg
      Right ge →
        GE.raiseGlobalError ge

  presentError message =
    when ((not $ isSearchQuery query) ∨ deep ≡ zero)
      $ fromAff
      $ driver
      $ toDialog $ Dialog.Show
      $ Dialog.Error message

  getChildren ∷ Array Resource → Slam Unit
  getChildren ress = do
    let next = mapMaybe (either Just (const Nothing) <<< getPath) ress
        toAdd = map Item $ filter (filterByQuery query) ress
    fromAff $ driver $ toListing $ Listing.Adds toAdd
    traverse_ (\n → listPath query (deep + one) var n driver)
      (guard (isSearchQuery query) *> next)

updateURL
  ∷ Maybe String
  → Sort
  → Maybe Salt
  → DirPath
  → Slam Unit
updateURL query sort salt path = fromEff do
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
