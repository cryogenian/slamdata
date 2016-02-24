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

module Quasar.Auth.Permission where

import Prelude

import Control.Monad.Aff (later')
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Random (random)
import Control.MonadPlus (guard)
import Control.UI.Browser (decodeURIComponent)

import Data.Functor.Eff (liftEff)
import Data.String as Str
import Data.String.Regex as Rgx
import Data.Maybe as M
import Data.Array as Arr

import DOM (DOM())
import DOM.HTML (window)
import DOM.HTML.Location as Location
import DOM.HTML.Window as Window

import Network.HTTP.RequestHeader (RequestHeader(..))

import SlamData.Effects (Slam())

newtype Permission = Permission String
runPermission :: Permission -> String
runPermission (Permission s) = s

permissionsHeader
  :: Array Permission
  -> M.Maybe RequestHeader
permissionsHeader ps = do
  guard (not $ Arr.null ps)
  pure
    $ RequestHeader "X-Extra-Permissions"
    $ Str.joinWith ","
    $ map runPermission ps

retrievePermissions
  :: forall e
   . Eff (dom :: DOM|e) (Array Permission)
retrievePermissions =
  window
    >>= Window.location
    >>= Location.search
    <#> permissionTokens
    <#> map Permission
  where
  permissionRegex :: Rgx.Regex
  permissionRegex = Rgx.regex "permissionsToken=([^&]+)" Rgx.noFlags

  extractPermissionsString :: String -> M.Maybe String
  extractPermissionsString str =
    Rgx.match permissionRegex str
      >>= flip Arr.index 1
      >>= id
      <#> decodeURIComponent

  permissionTokens :: String -> Array String
  permissionTokens s =
    M.fromMaybe []
      $ Str.split ","
      <$> extractPermissionsString s

-- A mock for generating new tokens
genToken :: Slam Permission
genToken =
  later' 1000 $ liftEff $ Permission <$> show <$> random
