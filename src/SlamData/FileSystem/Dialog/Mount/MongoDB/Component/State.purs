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
  ( State
  , initialState
  , processState
  , fromConfig
  , toConfig
  , module MCS
  ) where

import SlamData.Prelude

import Data.Array as A
import Data.NonEmpty (oneOf)
import Data.StrMap as SM
import Data.URI.Host as URI

import Global as Global

import Quasar.Mount.MongoDB (Config)

import SlamData.FileSystem.Dialog.Mount.Common.State as MCS

type State =
  { hosts ∷ Array MCS.MountHost
  , path ∷ String
  , user ∷ String
  , password ∷ String
  , props ∷ Array MCS.MountProp
  }

initialState ∷ State
initialState =
  { hosts: [MCS.initialTuple]
  , path: ""
  , user: ""
  , password: ""
  , props: [MCS.initialTuple]
  }

processState ∷ State → State
processState s = s
  { hosts = MCS.processDynMap s.hosts
  , props = MCS.processDynMap s.props
  }

fromConfig ∷ Config → State
fromConfig { hosts, path, user, password, props } =
  processState
    { hosts: bimap URI.printHost (maybe "" show) <$> oneOf hosts
    , path: ""
    , user: maybe "" Global.decodeURIComponent user
    , password: maybe "" Global.decodeURIComponent password
    , props: map (fromMaybe "") <$> SM.toUnfoldable props
    }

toConfig ∷ State → Either String Config
toConfig { hosts, path, user, password, props } = do
  hosts' ← MCS.nonEmptyHosts =<< traverse MCS.parseHost (A.filter (not MCS.isEmptyTuple) hosts)
  when (not MCS.isEmpty user || not MCS.isEmpty password) do
    when (MCS.isEmpty user) $ Left "Please enter user name"
    when (MCS.isEmpty password) $ Left "Please enter password"
    when (MCS.isEmpty path) $ Left "Please enter authentication database name"
  pure
    { hosts: hosts'
    , path: MCS.parsePath' =<< MCS.nonEmptyString path
    , user: Global.encodeURIComponent <$> MCS.nonEmptyString user
    , password: Global.encodeURIComponent <$> MCS.nonEmptyString password
    , props: SM.fromFoldable $ map MCS.nonEmptyString <$> A.filter (not MCS.isEmptyTuple) props
    }
