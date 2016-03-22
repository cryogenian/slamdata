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

import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (runExcept)

import Data.Lens (LensP, lens)

import SlamData.FileSystem.Dialog.Mount.MongoDB.Component.State as MongoDB
import SlamData.FileSystem.Dialog.Mount.Scheme (Scheme(..))
import SlamData.FileSystem.Dialog.Mount.SQL2.Component.State as SQL2

import Utils.Path (DirPath)

type State =
  { new :: Boolean
  , parent :: DirPath
  , name :: String
  , message :: Maybe String
  , saving :: Boolean
  , settings :: Maybe MountSettings
  }

type MountSettings = Either MongoDB.State SQL2.State

initialSettings :: Scheme -> MountSettings
initialSettings MongoDB = Left MongoDB.initialState
initialSettings SQL2 = Right SQL2.initialState

_new :: LensP State Boolean
_new = lens _.new (_ { new = _ })

_parent :: LensP State DirPath
_parent = lens _.parent (_ { parent = _ })

_name :: forall a. LensP { name :: String | a } String
_name = lens _.name (_ { name = _ })

_message :: LensP State (Maybe String)
_message = lens _.message (_ { message = _ })

_saving :: LensP State Boolean
_saving = lens _.saving (_ { saving = _ })

_settings :: LensP State (Maybe MountSettings)
_settings = lens _.settings _{settings = _}

initialState :: DirPath -> String -> Maybe MountSettings -> State
initialState parent name settings =
  { new: isNothing settings
  , parent
  , settings
  , name
  , message: Nothing
  , saving: false
  }

scheme :: MountSettings -> Scheme
scheme s = if isLeft s then MongoDB else SQL2

-- | Checks whether the state is saveable: no validation errors, and has a name
-- | entered/type selected.
canSave :: State -> Boolean
canSave st = isNothing st.message && isJust st.settings && st.name /= ""

validate :: State -> Maybe String
validate { new, name, settings } = either Just (const Nothing) $ runExcept do
  when (new && name == "") $ throwError "Please enter a mount name"
