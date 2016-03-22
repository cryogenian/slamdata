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

module SlamData.FileSystem.Dialog.Mount.MongoDB.Component.State where

import SlamData.Prelude

import Control.UI.Browser (decodeURIComponent, encodeURIComponent)

import Data.Array as A
import Data.Foldable (any)
import Data.Lens (LensP, lens)
import Data.List (fromList)
import Data.Path.Pathy (printPath)
import Data.String as S
import Data.String.Regex as Rx
import Data.StrMap (toList)
import Data.URI.Types as Uri

import Utils.URI (toURI)

type State =
  { hosts :: Array MountHost
  , path :: String
  , user :: String
  , password :: String
  , props :: Array MountProp
  }

type MountHost =
  { host :: String
  , port :: String
  }

type MountProp = Tuple String String

_hosts :: LensP State (Array MountHost)
_hosts = lens _.hosts (_ { hosts = _ })

_path :: LensP State String
_path = lens _.path (_ { path = _ })

_user :: LensP State String
_user =
  lens _.user (_{user = _})

_password :: LensP State String
_password =
  lens _.password (_{password = _})

_props :: LensP State (Array MountProp)
_props = lens _.props (_ { props = _ })

_host :: LensP MountHost String
_host = lens _.host (_ { host = _ })

_port :: LensP MountHost String
_port = lens _.port (_ { port = _ })

initialState :: State
initialState =
  { hosts: [initialMountHost]
  , path: ""
  , user: ""
  , password: ""
  , props: [initialMountProp]
  }

stateFromURI :: Uri.AbsoluteURI -> State
stateFromURI uri =
  processState
    { hosts: hostsFromURI uri
    , path: pathFromURI uri
    , user: userFromURI uri
    , password: passwordFromURI uri
    , props: propsFromURI uri
    }

initialMountHost :: MountHost
initialMountHost =
  { host: ""
  , port: ""
  }

initialMountProp :: MountProp
initialMountProp = Tuple "" ""

processState :: State -> State
processState s = s
  { hosts = A.filter (not isEmptyHost) s.hosts <> [initialMountHost]
  , props = A.filter (not isEmptyProp) s.props <> [initialMountProp]
  }

isEmptyHost :: MountHost -> Boolean
isEmptyHost { host, port } = Rx.test rxEmpty host && Rx.test rxEmpty port

isEmptyProp :: MountProp -> Boolean
isEmptyProp (Tuple name value) = Rx.test rxEmpty name && Rx.test rxEmpty value

hostsFromURI :: Uri.AbsoluteURI -> Array MountHost
hostsFromURI (Uri.AbsoluteURI _ (Uri.HierarchicalPart (Just (Uri.Authority _ hs)) _) _) =
  go <$> hs
  where
  go :: Tuple Uri.Host (Maybe Uri.Port) -> MountHost
  go (Tuple h p) = { host: getHost h, port: maybe "" show p }
  getHost :: Uri.Host -> String
  getHost (Uri.IPv6Address s) = s
  getHost (Uri.IPv4Address s) = s
  getHost (Uri.NameAddress s) = s
hostsFromURI _ = []

pathFromURI :: Uri.AbsoluteURI -> String
pathFromURI (Uri.AbsoluteURI _ (Uri.HierarchicalPart _ (Just p)) _) =
  let s = either printPath printPath p
  in if S.take 1 s == "/" then S.drop 1 s else s
pathFromURI _ = ""

userFromURI :: Uri.AbsoluteURI -> String
userFromURI (Uri.AbsoluteURI _ (Uri.HierarchicalPart (Just (Uri.Authority (Just ui) _)) _) _) =
  decodeURIComponent $ maybe ui (\ix -> S.take ix ui) $ S.indexOf ":" ui
userFromURI _ = ""

passwordFromURI :: Uri.AbsoluteURI -> String
passwordFromURI (Uri.AbsoluteURI _ (Uri.HierarchicalPart (Just (Uri.Authority (Just ui) _)) _) _) =
  decodeURIComponent $ maybe "" (\ix -> S.drop (ix + 1) ui) $ S.indexOf ":" ui
passwordFromURI _ = ""

propsFromURI :: Uri.AbsoluteURI -> Array MountProp
propsFromURI (Uri.AbsoluteURI _ _ (Just (Uri.Query qs))) =
  fromList <<< map (rmap (fromMaybe "")) <<< toList $ qs
propsFromURI _ = []

mkURI :: State -> String
mkURI { path, user, password, hosts, props } =
  if any isValidHost hosts
  then
    toURI
      { path: nonEmpty path
      , credentials:
          mkCredentials
          <$> (encodeURIComponent <$> nonEmpty user)
          <*> (encodeURIComponent <$> nonEmpty password)
      , hosts: prepareHost <$> A.filter (not isEmptyHost) hosts
      , props: prepareProp <$> A.filter (not isEmptyProp) props
      }
  else ""
  where

  mkCredentials = { user: _, password: _ }
  prepareHost h = h { port = nonEmpty h.port }
  prepareProp = uncurry { name: _, value: _ }

  isValidHost :: MountHost -> Boolean
  isValidHost = isJust <<< nonEmpty <<< _.host

  nonEmpty :: String -> Maybe String
  nonEmpty s = if Rx.test rxEmpty s then Nothing else Just s

rxEmpty :: Rx.Regex
rxEmpty = Rx.regex "^\\s*$" Rx.noFlags
