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
import Control.Monad.Aff.AVar (AVar, makeVar', modifyVar, putVar, takeVar)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Fork (Canceler(..), fork, cancel)
import Control.Monad.Eff.Exception (Error, error)
import Control.UI.Browser (setTitle, replaceLocation)

import Data.Array (null, filter, mapMaybe)
import Data.Lens (Lens', lens, (%~), (<>~))
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
  AceConfig.set AceConfig.basePath $ Config.baseUrl ⊕ "js/ace"
  AceConfig.set AceConfig.modePath $ Config.baseUrl ⊕ "js/ace"
  AceConfig.set AceConfig.themePath $ Config.baseUrl ⊕ "js/ace"

  runHalogenAff do
    fork Analytics.enableAnalytics

    wiring ←
      makeWiring rootDir mempty

    let
      ui = interpret (runSlam wiring) comp

    driver ←
      runUI ui (parentState initialState) =<< awaitBody

    fork do
      setSlamDataTitle slamDataVersion
      driver $ left $ action $ SetVersion slamDataVersion

    runSlam wiring $ fork $ routeSignal driver

setSlamDataTitle ∷ ∀ e. String → Aff (dom ∷ DOM|e) Unit
setSlamDataTitle version =
  liftEff $ setTitle $ "SlamData " ⊕ version


type ListingState =
  { canceler ∷ Canceler Error Slam
    -- depth ↔ active requests
  , requestMap ∷ M.Map Int Int
  }

initialListingState ∷ ListingState
initialListingState =
  { canceler: mempty
  , requestMap: M.singleton 0 0
  }

_canceler ∷ ∀ a r. Lens' { canceler ∷ a | r } a
_canceler = lens _.canceler _{ canceler = _ }

_requestMap ∷ ∀ a r. Lens' { requestMap ∷ a | r } a
_requestMap = lens _.requestMap _{ requestMap = _ }

routeSignal ∷ Driver QueryP SlamDataRawEffects → Slam Unit
routeSignal driver = do
  avar ←
    fromAff $ makeVar' initialListingState

  routeTpl ←
    fromAff $ matchesAff routing

  flip uncurry routeTpl
    $ redirects driver avar

redirects
  ∷ Driver QueryP SlamDataRawEffects
  → AVar ListingState
  → Maybe Routes → Routes
  → Slam Unit
redirects driver var mbOld = case _ of
  Index →
    updateURL Nothing Asc Nothing rootDir

  Sort sort →
    updateURL Nothing sort Nothing rootDir

  SortAndQ sort query →
    let
      queryParts = splitQuery query
    in
      updateURL queryParts.query sort Nothing queryParts.path

  Salted sort query salt → do
    {canceler} ← fromAff $ takeVar var

    cancel canceler $ error "cancel search"

    fromAff
      $ putVar var initialListingState

    fromAff
      $ driver
      $ toListing
      $ Listing.SetIsSearching
      $ isSearchQuery query

    let
      queryParts = splitQuery query

      isNewPage = fromMaybe true do
        old ← mbOld
        oldQuery × oldSalt ← case old of
          Salted _ oldQuery' oldSalt' → pure $ oldQuery' × oldSalt'
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

      when (isNothing queryParts.query)
        $ checkMount queryParts.path driver
      else
        fromAff $ driver $ toSearch $ Search.SetLoading false

checkMount
  ∷ DirPath
  → Driver QueryP SlamDataRawEffects
  → Slam Unit
checkMount path driver = do
  let
    setIsMount =
      fromAff
        $ driver
        $ left
        $ action
        $ SetIsMount true

  Quasar.mountInfo (Left path) >>= case _ of
    Left _ →
      -- When Quasar has no mounts configured we want to enable the root to be
      -- configured as a mount - if `/` is not a mount and also has no children
      -- then we know it's in this unconfigured state.
      when (path ≡ rootDir)
        $ void
        $ Quasar.children path >>= traverse \children →
            when (null children) $ setIsMount

    Right _ →
      setIsMount


listPath
  ∷ SearchQuery
  → Int
  → AVar ListingState
  → DirPath
  → Driver QueryP SlamDataRawEffects
  → Slam Unit
listPath query depth var dir driver = do
  fromAff
    $ flip modifyVar var
    $ _requestMap %~ M.alter memThisRequest depth

  canceler ←
    map Canceler $ fork goDeeper

  fromAff
    $ flip modifyVar var
    $ _canceler <>~ canceler

  where
  memThisRequest = case _ of
    Nothing → Just 1
    Just n → Just $ n + 1

  markThisRequestAsFinished = case _ of
    Just v | v > 1 → Just $ v - 1
    _ → Nothing

  -- Note that initialState is (zero ↔ zero) and final Ø
  allRequestsAreFinished = M.isEmpty

  goDeeper = do
    Quasar.children dir >>= either sendError getChildren
    fromAff do
      flip modifyVar var
        $ _requestMap %~ M.alter markThisRequestAsFinished depth

      st@{canceler: c, requestMap: r} ←
        takeVar var

      if allRequestsAreFinished r
        then do
        driver $ toSearch $ Search.SetLoading false
        putVar var initialListingState
        else
        putVar var st

  sendError ∷ QE.QError → Slam Unit
  sendError err =
    case GE.fromQError err of
      Left msg →
        presentError $
          "There was a problem accessing this directory listing. " <> msg
      Right ge →
        GE.raiseGlobalError ge

  presentError message =
    when ((not $ isSearchQuery query) ∨ depth ≡ zero)
      $ fromAff
      $ driver
      $ toDialog $ Dialog.Show
      $ Dialog.Error message

  getChildren ∷ Array Resource → Slam Unit
  getChildren ress = do
    let
      next = do
        guard $ isSearchQuery query
        flip mapMaybe ress $ either Just (const Nothing) ∘ getPath
      toAdd =
        map Item $ flip filter ress $ filterByQuery query

    fromAff
      $ driver
      $ toListing
      $ Listing.Adds toAdd

    flip parTraverse_ next \n →
      listPath query (depth + one) var n driver


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
