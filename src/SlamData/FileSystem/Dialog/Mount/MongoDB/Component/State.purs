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

module SlamData.FileSystem.Dialog.Mount.MongoDB.Component.State
  ( MountHost
  , MountProp
  , State
  , _host
  , _hosts
  , _password
  , _path
  , _port
  , _props
  , _user
  , initialState
  , processState
  , fromConfig
  , toConfig
  ) where

import SlamData.Prelude

import Data.Array as A
import Data.Int as Int
import Data.Lens (LensP, lens)
import Data.List (fromList, fromFoldable)
import Data.NonEmpty (NonEmpty(..), oneOf)
import Data.Path.Pathy (parsePath, rootDir, (</>))
import Data.Profunctor.Strong (first, second)
import Data.String.Regex as Rx
import Data.StrMap as SM
import Data.URI.Host as URI

import Text.Parsing.StringParser (runParser)

import Quasar.Mount.MongoDB (Config, Host)

import Utils.Path as PU

type State =
  { hosts ∷ Array MountHost
  , path ∷ String
  , user ∷ String
  , password ∷ String
  , props ∷ Array MountProp
  }

type MountHost = Tuple String String

type MountProp = Tuple String String

_hosts ∷ LensP State (Array MountHost)
_hosts = lens _.hosts (_ { hosts = _ })

_path ∷ LensP State String
_path = lens _.path (_ { path = _ })

_user ∷ LensP State String
_user = lens _.user (_{user = _})

_password ∷ LensP State String
_password = lens _.password (_{password = _})

_props ∷ LensP State (Array MountProp)
_props = lens _.props (_ { props = _ })

_host ∷ LensP MountHost String
_host = first

_port ∷ LensP MountHost String
_port = second

initialState ∷ State
initialState =
  { hosts: [initialTuple]
  , path: ""
  , user: ""
  , password: ""
  , props: [initialTuple]
  }

initialTuple ∷ Tuple String String
initialTuple = Tuple "" ""

processState ∷ State → State
processState s = s
  { hosts = A.filter (not isEmptyTuple) s.hosts ⊕ [initialTuple]
  , props = A.filter (not isEmptyTuple) s.props ⊕ [initialTuple]
  }

fromConfig ∷ Config → State
fromConfig { hosts, path, user, password, props } =
  processState
    { hosts: bimap URI.printHost show <$> oneOf hosts
    , path: ""
    , user: fromMaybe "" user
    , password: fromMaybe "" password
    , props: map (fromMaybe "") <$> fromList (SM.toList props)
    }

toConfig ∷ State → Either String Config
toConfig { hosts, path, user, password, props } = do
  hosts' ← nonEmptyHosts =<< traverse parseHost (A.filter (not isEmptyTuple) hosts)
  when (not isEmpty user || not isEmpty password) do
    when (isEmpty user) $ Left "Please enter user name"
    when (isEmpty password) $ Left "Please enter password"
    when (isEmpty path) $ Left "Please enter authentication database name"
  pure
    { hosts: hosts'
    , path: parsePath' =<< nonEmptyString path
    , user: nonEmptyString user
    , password: nonEmptyString password
    , props: SM.fromList $ fromFoldable $
        map nonEmptyString <$> A.filter (not isEmptyTuple) props
    }

parsePath' ∷ String → Maybe PU.AnyPath
parsePath' =
  bitraverse PU.sandbox PU.sandbox <<<
    parsePath (Right <<< (rootDir </> _)) Right (Left <<< (rootDir </> _)) Left

parseHost ∷ Tuple String String → Either String Host
parseHost (Tuple host port) = do
  host' ← lmap show $ runParser URI.parseHost host
  port' ← case nonEmptyString port of
    Nothing → pure Nothing
    Just p →
      maybe
        (Left $ "'" ⊕ port ⊕ "' is not a valid port number")
        (Right <<< Just) $
        (Int.fromString p)
  pure $ Tuple host' port'

nonEmptyString ∷ String → Maybe String
nonEmptyString s = if isEmpty s then Nothing else Just s

nonEmptyHosts ∷ Array Host → Either String (NonEmpty Array Host)
nonEmptyHosts hs = maybe err Right $ NonEmpty <$> A.head hs <*> A.tail hs
  where
  err = Left "Please enter at least one host"

isEmptyTuple ∷ Tuple String String → Boolean
isEmptyTuple (Tuple k v) = isEmpty k && isEmpty v

isEmpty ∷ String → Boolean
isEmpty = Rx.test rxEmpty

rxEmpty ∷ Rx.Regex
rxEmpty = Rx.regex "^\\s*$" Rx.noFlags
