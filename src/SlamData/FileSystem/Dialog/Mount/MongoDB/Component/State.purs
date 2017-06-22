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
  , Field(..)
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
import Data.URI.Host (printHost) as URI
import Data.URI.Path (printPath) as URI
import Data.Validation.Semigroup as V
import Global as Global
import Quasar.Mount.MongoDB as MDB
import SlamData.FileSystem.Dialog.Mount.Common.State as MCS

data Field = Hosts | Auth

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

fromConfig ∷ MDB.Config → State
fromConfig { hosts, auth, props } =
  processState
    { hosts: bimap URI.printHost (maybe "" show) <$> oneOf hosts
    , path: maybe "" (URI.printPath <<< _.path <<< unwrap) auth
    , user: maybe "" (Global.decodeURIComponent <<< _.user <<< unwrap <<< _.credentials <<< unwrap) auth
    , password: maybe "" (Global.decodeURIComponent <<< _.password <<< unwrap <<< _.credentials <<< unwrap) auth
    , props: map (fromMaybe "") <$> SM.toUnfoldable props
    }

toConfig ∷ State → V.V (MCS.ValidationError Field) MDB.Config
toConfig { hosts, path, user, password, props } =
  { hosts: _, auth: _, props: _ } <$> hosts' <*> auth' <*> props'
  where
    hosts' = either (MCS.invalid Hosts) pure $
      MCS.nonEmptyHosts =<< traverse MCS.parseHost (A.filter (not MCS.isEmptyTuple) hosts)
    -- TODO: make better use of V, don't mash the fields into just "auth"
    auth' = either (MCS.invalid Auth) pure $ do
      let credentials = MCS.parseCredentials { user, password }
      when (not MCS.isEmpty user || not MCS.isEmpty password) do
        when (MCS.isEmpty user) $ Left "Please enter user name"
        when (MCS.isEmpty password) $ Left "Please enter password"
        when (MCS.isEmpty path) $ Left "Please enter authentication database name"
      case MCS.nonEmptyString path, credentials of
        Just p, Just c → do
          path' ← maybe (Left "Please enter a valid path") pure (MCS.parsePath' p)
          pure $ Just $ MDB.Auth { path: path', credentials: c }
        _, _ →
          -- The possible mixed cases ignored here will be caught by the `when`s above.
          pure Nothing
    props' = pure $ SM.fromFoldable $
      map MCS.nonEmptyString <$> A.filter (not MCS.isEmptyTuple) props
