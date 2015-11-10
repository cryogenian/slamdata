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

module Dialog.Mount.State where

import Prelude

import Data.Array (replicate)
import Data.Char (fromCharCode)
import Data.Either (either, Either(..))
import Data.List (fromList)
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.Path.Pathy (printPath)
import Data.StrMap (toList)
import Data.String as S
import Data.Tuple (Tuple(..))
import Data.URI (printAbsoluteURI)
import Data.URI.Types as Uri
import Model.Resource as R
import Data.Lens (LensP(), lens)
import Utils.Path (DirPath())

type State =
  { new :: Boolean
  , parent :: DirPath
  , name :: String
  , connectionURI :: String
  , hosts :: (Array MountHostRec)
  , path :: String
  , user :: String
  , password :: String
  , props :: (Array MountPropRec)
  , message :: Maybe String
  , externalValidationError :: Maybe String
  , valid :: Boolean
  , inProgress :: Boolean
  , saved :: Maybe R.Resource
  }

type MountHostRec =
  { host :: String
  , port :: String
  }

type MountPropRec =
  { name :: String
  , value :: String
  }

_name :: forall a. LensP { name :: String | a } String
_name = lens _.name (_ { name = _ })

_connectionURI :: LensP State String
_connectionURI = lens _.connectionURI (_ { connectionURI = _ })

_hosts :: LensP State (Array MountHostRec)
_hosts = lens _.hosts (_ { hosts = _ })

_path :: LensP State String
_path = lens _.path (_ { path = _ })

_user :: LensP State String
_user = lens _.user (_ { user = _ })

_password :: LensP State String
_password = lens _.password (_ { password = _ })

_inProgress :: LensP State Boolean
_inProgress = lens _.inProgress (_ { inProgress = _ })

_message :: LensP State (Maybe String)
_message = lens _.message (_ { message = _ })

_externalValidationError :: LensP State (Maybe String)
_externalValidationError = lens _.externalValidationError (_ { externalValidationError = _ })

_props :: LensP State (Array MountPropRec)
_props = lens _.props (_ { props = _ })

_host :: LensP MountHostRec String
_host = lens _.host (_ { host = _ })

_port :: LensP MountHostRec String
_port = lens _.port (_ { port = _ })

_value :: LensP MountPropRec String
_value = lens _.value (_ { value = _ })

_saved :: LensP State (Maybe R.Resource)
_saved = lens _.saved _{saved = _}

initialState :: DirPath -> State
initialState parent =
  { new: true
  , parent: parent
  , name: ""
  , connectionURI: ""
  , hosts: [initialMountHost, initialMountHost]
  , path: ""
  , user: ""
  , password: ""
  , props: [initialMountProp]
  , message: Nothing
  , externalValidationError: Nothing
  , valid: false
  , inProgress: false
  , saved: Nothing
  }

initialMountHost :: MountHostRec
initialMountHost =
  { host: ""
  , port: ""
  }

initialMountProp :: MountPropRec
initialMountProp =
  { name: ""
  , value: ""
  }

isEmptyHost :: MountHostRec -> Boolean
isEmptyHost h = h.host == "" && h.port == ""

isEmptyProp :: MountPropRec -> Boolean
isEmptyProp p = p.name == "" && p.value == ""

mountDialogFromURI :: R.Resource -> Uri.AbsoluteURI -> State
mountDialogFromURI res uri =
  (initialState $ R.resourceDir res)
    { connectionURI = printAbsoluteURI uri
    , hosts = hostsFromURI uri ++ [initialMountHost]
    , path = pathFromURI uri
    , user = userFromURI uri
    , password = passwordFromURI uri
    , props = propsFromURI uri ++ [initialMountProp]
    , new = false
    , name = if res == R.root
             then "/"
             else R.resourceName res
    , valid = true
    }

schemeFromURI :: Uri.AbsoluteURI -> String
schemeFromURI (Uri.AbsoluteURI (Just (Uri.URIScheme s)) _ _) = s
schemeFromURI _ = ""

hostsFromURI :: Uri.AbsoluteURI -> Array MountHostRec
hostsFromURI (Uri.AbsoluteURI _ (Uri.HierarchicalPart (Just (Uri.Authority _ hs)) _) _) = go <$> hs
  where
  go :: Tuple Uri.Host (Maybe Uri.Port) -> MountHostRec
  go (Tuple h p) = { host: getHost h, port: maybe "" show p }
  getHost :: Uri.Host -> String
  getHost (Uri.IPv6Address s) = s
  getHost (Uri.IPv4Address s) = s
  getHost (Uri.NameAddress s) = s
hostsFromURI _ = []

pathFromURI :: Uri.AbsoluteURI -> String
pathFromURI (Uri.AbsoluteURI _ (Uri.HierarchicalPart _ (Just p)) _) = either printPath printPath p
pathFromURI _ = ""

userFromURI :: Uri.AbsoluteURI -> String
userFromURI (Uri.AbsoluteURI _ (Uri.HierarchicalPart (Just (Uri.Authority (Just ui) _)) _) _) =
  maybe ui (\ix -> S.take ix ui) $ S.indexOf ":" ui
userFromURI _ = ""

passwordFromURI :: Uri.AbsoluteURI -> String
passwordFromURI (Uri.AbsoluteURI _ (Uri.HierarchicalPart (Just (Uri.Authority (Just ui) _)) _) _) =
  maybe "" (\ix -> S.drop (ix + 1) ui) $ S.indexOf ":" ui
passwordFromURI _ = ""

setURIPassword :: String -> Uri.AbsoluteURI -> Uri.AbsoluteURI
setURIPassword pass (Uri.AbsoluteURI s (Uri.HierarchicalPart (Just (Uri.Authority (Just ui) hs)) p) q) =
  let mbIx = S.indexOf ":" ui
      user = maybe ui (\ix -> S.take ix ui) mbIx
      pass' = if pass == "" then "" else ":" ++ pass
  in Uri.AbsoluteURI s (Uri.HierarchicalPart (Just (Uri.Authority (Just $ user ++ pass') hs)) p) q
setURIPassword _ uri = uri

propsFromURI :: Uri.AbsoluteURI -> Array MountPropRec
propsFromURI (Uri.AbsoluteURI _ _ (Just (Uri.Query qs))) =
  fromList <<< (go <$>) <<< toList $ qs
  where
  go :: Tuple String (Maybe String) -> MountPropRec
  go (Tuple k v) = { name: k, value: fromMaybe "" v }
propsFromURI _ = []

hidePassword :: String -> String
hidePassword s = S.joinWith "" $ replicate (S.length s) (S.fromChar $ fromCharCode 8226)

validate :: State -> Array MountHostRec -> Maybe String
validate d hosts =
  either Just (const Nothing) do
    case Tuple d.new d.name of
      Tuple true "" -> Left "Please enter a name for the mount"
      _ -> Right unit
    case hosts of
      [ ] -> Left "Please enter at least one host"
      _ -> Right unit
