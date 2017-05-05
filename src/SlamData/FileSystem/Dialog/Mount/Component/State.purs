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
import SlamData.FileSystem.Dialog.Mount.SparkLocal.Component.State as SparkLocal
import SlamData.FileSystem.Dialog.Mount.MongoDB.Component.State as MongoDB
import SlamData.FileSystem.Dialog.Mount.SQL2.Component.State as SQL2

import Utils.Path (DirPath)

type State =
  { new ∷ Boolean
  , parent ∷ Maybe DirPath
  , name ∷ Maybe String
  , message ∷ Maybe String
  , saving ∷ Boolean
  , settings ∷ Maybe MountSettings
  }

data Input
  = New { parent ∷ DirPath }
  | Edit { parent ∷ Maybe DirPath, name ∷ Maybe String, settings ∷  MountSettings }
  | Root

data MountSettings
  = MongoDB MongoDB.State
  | SQL2 SQL2.State
  | Couchbase Couchbase.State
  | MarkLogic MarkLogic.State
  | SparkHDFS SparkHDFS.State
  | SparkLocal SparkLocal.State

initialSettings ∷ MS.Scheme → MountSettings
initialSettings = case _ of
  MS.MongoDB → MongoDB MongoDB.initialState
  MS.SQL2 → SQL2 SQL2.initialState
  MS.Couchbase → Couchbase Couchbase.initialState
  MS.MarkLogic → MarkLogic MarkLogic.initialState
  MS.SparkHDFS → SparkHDFS SparkHDFS.initialState
  MS.SparkLocal → SparkLocal SparkLocal.initialState

_new ∷ Lens' State (Maybe DirPath)
_new = lens _.parent (_ { parent = _ })

_parent ∷ Lens' State (Maybe DirPath)
_parent = lens _.parent (_ { parent = _ })

_name ∷ forall a. Lens' { name ∷ Maybe String | a } (Maybe String)
_name = lens _.name (_ { name = _ })

_message ∷ Lens' State (Maybe String)
_message = lens _.message (_ { message = _ })

_saving ∷ Lens' State Boolean
_saving = lens _.saving (_ { saving = _ })

_settings ∷ Lens' State (Maybe MountSettings)
_settings = lens _.settings _{settings = _}

initialState ∷ Input → State
initialState = case _ of
  New { parent } →
    { new: true
    , parent: Just parent
    , settings: Nothing
    , name: Just ""
    , message: Nothing
    , saving: false
    }
  Edit { parent, name, settings } →
    { new: false
    , parent: parent
    , settings: Just settings
    , name: name
    , message: Nothing
    , saving: false
    }
  Root →
    { new: true
    , parent: Nothing
    , settings: Nothing
    , name: Nothing
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
  SparkLocal _ → MS.SparkLocal

-- | Checks whether the state is saveable: no validation errors, and has a name
-- | entered/type selected.
canSave ∷ State → Boolean
canSave st
  = isNothing st.message
  && isJust st.settings
  && (maybe true (_ /= "") st.name)

validate ∷ State → Maybe String
validate { name } = either Just (const Nothing) $ runExcept do
  when (maybe false (eq "") name) $ throwError "Please enter a mount name"
