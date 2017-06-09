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

module SlamData.FileSystem.Dialog.Mount.Couchbase.Component.State
  ( State
  , Field(..)
  , initialState
  , _bucketName
  , _docTypeKey
  , fromConfig
  , toConfig
  , module MCS
  ) where

import SlamData.Prelude

import Data.Lens (Lens', lens)
import Data.Number as Num
import Data.Time.Duration (Seconds(..))
import Data.URI.Host as URI
import Data.Validation.Semigroup as V
import Global as Global
import Quasar.Mount.Couchbase (Config)
import SlamData.FileSystem.Dialog.Mount.Common.State as MCS
import Utils (showPrettyNumber)

data Field = Host | BucketName | Password | DocType | QueryTimeout

type State =
  { host ∷ MCS.MountHost
  , bucketName ∷ String
  , password ∷ String
  , docTypeKey ∷ String
  , queryTimeout ∷ Maybe String
  }

initialState ∷ State
initialState =
  { host: MCS.initialTuple
  , bucketName: ""
  , password: ""
  , docTypeKey: "type"
  , queryTimeout: Nothing
  }

_bucketName ∷ Lens' State String
_bucketName = lens _.bucketName (_{bucketName = _})

_docTypeKey ∷ Lens' State String
_docTypeKey = lens _.docTypeKey (_{docTypeKey = _})

fromConfig ∷ Config → State
fromConfig { host, bucketName, password, docTypeKey, queryTimeout } =
  { host: bimap URI.printHost (maybe "" show) host
  , bucketName
  , password
  , docTypeKey
  , queryTimeout: showPrettyNumber <<< unwrap <$> queryTimeout
  }

toConfig ∷ State → V.V (MCS.ValidationError Field) Config
toConfig st = do
  { host: _, bucketName: _, password: _, docTypeKey: _, queryTimeout: _ }
    <$> host
    <*> bucketName
    <*> password
    <*> docTypeKey
    <*> queryTimeout
  where

    host = either (MCS.invalid Host) pure do
      when (MCS.isEmpty (fst st.host)) $ Left "Please enter a host"
      lmap ("Host: " <> _) $ MCS.parseHost st.host

    bucketName =
      maybe (MCS.invalid BucketName "Please enter a bucket name") pure $
        MCS.nonEmptyString st.bucketName

    docTypeKey =
      maybe (MCS.invalid DocType "Please enter a document type") pure $
        MCS.nonEmptyString st.docTypeKey

    password = pure (Global.encodeURIComponent st.password)

    queryTimeout =
      either (MCS.invalid QueryTimeout) pure $
        for st.queryTimeout \s → do
          n ← maybe (Left "Please enter a valid timeout in seconds") Right $ Num.fromString s
          when (n < 0.0) $ Left "Please enter a timeout value greater than 0 seconds"
          pure $ Seconds n
