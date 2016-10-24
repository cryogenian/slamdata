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

module SlamData.FileSystem.Dialog.Mount.Spark.Component.State
  ( State
  , initialState
  , _sparkHost
  , _hdfsHost
  , fromConfig
  , toConfig
  , module MCS
  ) where

import SlamData.Prelude

import Data.Lens (LensP, lens)
import Data.URI.Path (printPath) as URI
import Data.URI.Host (printHost) as URI

import Quasar.Mount.Spark (Config)

import SlamData.FileSystem.Dialog.Mount.Common.State as MCS

type State =
  { sparkHost ∷ MCS.MountHost
  , hdfsHost ∷ MCS.MountHost
  , path ∷ String
  }

initialState ∷ State
initialState =
  { sparkHost: MCS.initialTuple
  , hdfsHost: MCS.initialTuple
  , path: ""
  }

_sparkHost ∷ ∀ r a. LensP { sparkHost ∷ a | r } a
_sparkHost = lens _.sparkHost (_ { sparkHost = _ })

_hdfsHost ∷ ∀ r a. LensP { hdfsHost ∷ a | r } a
_hdfsHost = lens _.hdfsHost (_ { hdfsHost = _ })

fromConfig ∷ Config → State
fromConfig { sparkHost, hdfsHost, path } =
  { sparkHost: bimap URI.printHost (maybe "" show) sparkHost
  , hdfsHost: bimap URI.printHost (maybe "" show) hdfsHost
  , path: maybe "" URI.printPath path
  }

toConfig ∷ State → Either String Config
toConfig { sparkHost, hdfsHost, path } = do
  when (MCS.isEmpty (fst sparkHost)) $ Left "Please enter a Spark host"
  sparkHost' ← lmap ("Spark host: " <> _) $ MCS.parseHost sparkHost
  when (MCS.isEmpty (fst hdfsHost)) $ Left "Please enter an HDFS host"
  hdfsHost' ← lmap ("HDFS host: " <> _) $ MCS.parseHost hdfsHost
  pure
    { sparkHost: sparkHost'
    , hdfsHost: hdfsHost'
    , path: MCS.parsePath' =<< MCS.nonEmptyString path
    }
