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

module SlamData.FileSystem.Dialog.Download.Component.State where

import SlamData.Prelude

import Data.Array (findIndex)
import Data.Lens (Lens', lens, (?~), (.~))

import Network.HTTP.RequestHeader (RequestHeader)

import SlamData.Download.Model (JSONOptions, CSVOptions, initialCSVOptions)
import SlamData.FileSystem.Resource (Resource, resourceName, getPath)

type State =
  { source ∷ Resource
  , targetName ∷ Either String String
  , compress ∷ Boolean
  , options ∷ Either CSVOptions JSONOptions
  , error ∷ Maybe String
  , authHeaders ∷ Array RequestHeader
  }

initialState ∷ Resource → State
initialState res =
  { source: res
  , targetName:
      let name = resourceName res
      in Right $ if name ≡ "" then "archive" else name
  , compress: false
  , options: Left initialCSVOptions
  , error: Nothing
  , authHeaders: []
  }

_source ∷ Lens' State Resource
_source = lens _.source (_ { source = _ })

_targetName ∷ Lens' State (Either String String)
_targetName = lens _.targetName (_ { targetName = _ })

_compress ∷ Lens' State Boolean
_compress = lens _.compress (_ { compress = _ })

_options ∷ Lens' State (Either CSVOptions JSONOptions)
_options = lens _.options (_ { options = _ })

_error ∷ Lens' State (Maybe String)
_error = lens _.error (_ { error = _ })

_authHeaders ∷ ∀ a r. Lens' {authHeaders ∷ a | r} a
_authHeaders = lens _.authHeaders (_{authHeaders = _})

validate ∷ State → State
validate r
  | isLeft (r.targetName) =
    r # _error ?~ "Please enter a valid target filename"
  | otherwise =
    r # _error .~ Nothing

checkExists ∷ Resource → Array Resource → Boolean
checkExists r rs =
  let path = getPath r
  in isJust $ findIndex (getPath ⋙ eq path) rs
