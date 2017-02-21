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

module SlamData.FileSystem.Dialog.Mount.Component.State where

import SlamData.Prelude

import Data.Lens (Lens', lens)

import SlamData.FileSystem.Dialog.Mount.Scheme as MS
import SlamData.FileSystem.Dialog.Mount.Couchbase.Component.State as Couchbase
import SlamData.FileSystem.Dialog.Mount.MarkLogic.Component.State as MarkLogic
import SlamData.FileSystem.Dialog.Mount.SparkHDFS.Component.State as SparkHDFS
import SlamData.FileSystem.Dialog.Mount.MongoDB.Component.State as MongoDB
import SlamData.FileSystem.Dialog.Mount.SQL2.Component.State as SQL2

import Utils.Path (DirPath)

type State =
  { new ∷ Boolean
  , parent ∷ DirPath
  , name ∷ String
  , message ∷ Maybe String
  , saving ∷ Boolean
  , settings ∷ Maybe MountSettings
  }

type Input =
  { parent ∷ DirPath
  , name ∷ String
  , settings ∷ Maybe MountSettings
  }

data MountSettings
  = MongoDB MongoDB.State
  | SQL2 SQL2.State
  | Couchbase Couchbase.State
  | MarkLogic MarkLogic.State
  | SparkHDFS SparkHDFS.State

initialSettings ∷ MS.Scheme → MountSettings
initialSettings = case _ of
  MS.MongoDB → MongoDB MongoDB.initialState
  MS.SQL2 → SQL2 SQL2.initialState
  MS.Couchbase → Couchbase Couchbase.initialState
  MS.MarkLogic → MarkLogic MarkLogic.initialState
  MS.SparkHDFS → SparkHDFS SparkHDFS.initialState

_new ∷ Lens' State Boolean
_new = lens _.new (_ { new = _ })

_parent ∷ Lens' State DirPath
_parent = lens _.parent (_ { parent = _ })

_name ∷ forall a. Lens' { name ∷ String | a } String
_name = lens _.name (_ { name = _ })

_message ∷ Lens' State (Maybe String)
_message = lens _.message (_ { message = _ })

_saving ∷ Lens' State Boolean
_saving = lens _.saving (_ { saving = _ })

_settings ∷ Lens' State (Maybe MountSettings)
_settings = lens _.settings _{settings = _}

initialState ∷ Input → State
initialState { parent, name, settings } =
  { new: isNothing settings
  , parent
  , settings
  , name
  , message: Nothing
  , saving: false
  }

scheme ∷ MountSettings → MS.Scheme
scheme = case _ of
  MongoDB _ → MS.MongoDB
  SQL2 _ → MS.SQL2
  Couchbase _ → MS.Couchbase
  MarkLogic _ → MS.MarkLogic
  SparkHDFS _ → MS.SparkHDFS

-- | Checks whether the state is saveable: no validation errors, and has a name
-- | entered/type selected.
canSave ∷ State → Boolean
canSave st
  = isNothing st.message
  && isJust st.settings
  && (not st.new || st.name /= "")

validate ∷ State → Maybe String
validate { new, name, settings } = either Just (const Nothing) $ runExcept do
  when (new && name == "") $ throwError "Please enter a mount name"
