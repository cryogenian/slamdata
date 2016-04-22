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

module SlamData.Quasar.Auth.Permission
  ( module SlamData.Quasar.Auth.Permission
  , module Quasar.Advanced.Auth
  ) where

import SlamData.Prelude

import Control.Monad.Aff (later')
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Random (random)

import Data.Array as Arr
import Data.NonEmpty as Ne
import Data.String as Str
import Data.String.Regex as Rgx

import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Location as Location
import DOM.HTML.Window as Window

import Network.HTTP.RequestHeader (RequestHeader(..))

import Quasar.Advanced.Auth (PermissionToken(..), runPermissionToken)

import SlamData.Effects (Slam)
import SlamData.FileSystem.Resource as R

import Utils.Path (FilePath, parseFilePath)

type Permissions =
  {
    add :: Boolean
  , read :: Boolean
  , modify :: Boolean
  , delete :: Boolean
  }

isPermissionsEmpty
  :: Permissions
  -> Boolean
isPermissionsEmpty {add, read, modify, delete} =
  not (add || read || modify || delete)


newtype Group = Group FilePath
runGroup :: Group -> FilePath
runGroup (Group fp) = fp

newtype User = User String
runUser :: User -> String
runUser (User s) = s

type PermissionShareRequest =
  {
    resource :: R.Resource
  , targets :: Either (Ne.NonEmpty Array Group) (Ne.NonEmpty Array User)
  , permissions :: Permissions
  }

requestForGroups
  :: PermissionShareRequest
  -> Boolean
requestForGroups {targets} =
  isLeft targets

requestForUsers
  :: PermissionShareRequest
  -> Boolean
requestForUsers {targets} =
  isRight targets

permissionsHeader
  :: Array PermissionToken
  -> Maybe RequestHeader
permissionsHeader ps = do
  guard (not $ Arr.null ps)
  pure
    $ RequestHeader "X-Extra-PermissionTokens"
    $ Str.joinWith ","
    $ map runPermissionToken ps

retrievePermissionTokens
  :: forall e
   . Eff (dom :: DOM|e) (Array PermissionToken)
retrievePermissionTokens =
  window
    >>= Window.location
    >>= Location.search
    <#> permissionTokens
    <#> map PermissionToken
  where
  permissionRegex :: Rgx.Regex
  permissionRegex = Rgx.regex "permissionsToken=([^&]+)" Rgx.noFlags

  extractPermissionTokensString :: String -> Maybe String
  extractPermissionTokensString str =
    Rgx.match permissionRegex str
      >>= flip Arr.index 1
      >>= id
      <#> Global.decodeURIComponent

  permissionTokens :: String -> Array String
  permissionTokens s =
    fromMaybe []
      $ Str.split ","
      <$> extractPermissionTokensString s

-- A mock for generating new tokens
genToken :: Slam PermissionToken
genToken =
  later' 1000 $ liftEff $ PermissionToken <$> show <$> random

-- One more mock
doPermissionShareRequest
  :: PermissionShareRequest
  -> Slam Unit
doPermissionShareRequest _ =
  later' 1000 $ pure unit


permissionsForResource
  :: R.Resource
  -> Slam Permissions
permissionsForResource _ =
  later' 1000 $ pure {add: true, read: true, modify: true, delete: true}

getGroups
  :: Slam (Array Group)
getGroups =
  later' 1000
    $ pure
    $ map Group
    $ Arr.catMaybes
    $ map parseFilePath ["/foo", "/foo/bar", "/foo/bar/baz", "/foo/quux" ]
