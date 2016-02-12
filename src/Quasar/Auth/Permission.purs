module Quasar.Auth.Permission where

import Prelude

import Control.Monad.Eff (Eff())
import Control.MonadPlus (guard)
import Control.UI.Browser (decodeURIComponent)

import Data.String as Str
import Data.String.Regex as Rgx
import Data.Maybe as M
import Data.Array as Arr

import DOM (DOM())
import DOM.HTML (window)
import DOM.HTML.Location as Location
import DOM.HTML.Window as Window

import Network.HTTP.RequestHeader (RequestHeader(..))


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
