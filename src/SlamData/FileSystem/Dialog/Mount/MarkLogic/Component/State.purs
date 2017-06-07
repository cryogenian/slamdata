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

module SlamData.FileSystem.Dialog.Mount.MarkLogic.Component.State
  ( State
  , Field(..)
  , initialState
  , fromConfig
  , toConfig
  , module MCS
  ) where

import SlamData.Prelude

import Data.URI.Host (printHost) as URI
import Data.URI.Path (printPath) as URI
import Data.Validation.Semigroup as V
import Global as Global
import Quasar.Mount.MarkLogic (Config, Format(..))
import SlamData.FileSystem.Dialog.Mount.Common.State as MCS

type State =
  { host ∷ MCS.MountHost
  , user ∷ String
  , password ∷ String
  , path ∷ String
  , format ∷ Format
  }

data Field = Host | Auth | Path

initialState ∷ State
initialState =
  { host: MCS.initialTuple
  , user: ""
  , password: ""
  , path: ""
  , format: XML
  }

fromConfig ∷ Config → State
fromConfig { host, credentials, path, format } =
  { host: bimap URI.printHost (maybe "" show) host
  , user: maybe "" (Global.decodeURIComponent <<< _.user <<< unwrap) credentials
  , password: maybe "" (Global.decodeURIComponent <<< _.password <<< unwrap) credentials
  , path: maybe "" URI.printPath path
  , format
  }

toConfig ∷ State → V.V (MCS.ValidationError Field) Config
toConfig { host, user, password, path, format } =
  { host: _
  , credentials: _
  , path: _
  , format
  } <$> host' <*> credentials <*> path'
  where
    host' = either (MCS.invalid Host) pure do
      when (MCS.isEmpty (fst host)) $ Left "Please enter host"
      h ← lmap ("Host: " <> _) $ MCS.parseHost host
      when (isNothing (snd h)) $ Left "Please enter port"
      pure h
    credentials = either (MCS.invalid Auth) pure do
      when (not MCS.isEmpty user || not MCS.isEmpty password) do
        when (MCS.isEmpty user) $ Left "Please enter user name"
        when (MCS.isEmpty password) $ Left "Please enter password"
      pure $ MCS.parseCredentials { user, password }
    path' = pure $ MCS.parsePath' =<< MCS.nonEmptyString path
