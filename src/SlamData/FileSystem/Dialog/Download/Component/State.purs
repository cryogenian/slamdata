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
import Data.Either.Unsafe as U
import Data.Lens (LensP, lens, (?~), (.~))

import SlamData.Download.Model (JSONOptions, CSVOptions, initialCSVOptions)
import SlamData.FileSystem.Resource (Resource, resourceName, root, getPath)

type State =
  { source :: Either String Resource
  , sources :: (Array Resource)
  , showSourcesList :: Boolean
  , targetName :: Either String String
  , compress :: Boolean
  , options :: Either CSVOptions JSONOptions
  , error :: Maybe String
  }

initialState :: Resource -> State
initialState res =
  { source: Right res
  , sources: [root, res]
  , showSourcesList: false
  , targetName:
      let name = resourceName res
      in Right $ if name == "" then "archive" else name
  , compress: false
  , options: Left initialCSVOptions
  , error: Nothing
  }

_source :: LensP State (Either String Resource)
_source = lens _.source (_ { source = _ })

_sources :: LensP State (Array Resource)
_sources = lens _.sources (_ { sources = _ })

_showSourcesList :: LensP State Boolean
_showSourcesList = lens _.showSourcesList (_ { showSourcesList = _ })

_targetName :: LensP State (Either String String)
_targetName = lens _.targetName (_ { targetName = _ })

_compress :: LensP State Boolean
_compress = lens _.compress (_ { compress = _ })

_options :: LensP State (Either CSVOptions JSONOptions)
_options = lens _.options (_ { options = _ })

_error :: LensP State (Maybe String)
_error = lens _.error (_ { error = _ })

validate :: State -> State
validate r
  | isLeft (r.source) =
    r # _error ?~ "Please enter a valid source path to download"
  | not $ checkExists (U.fromRight $ r.source) (r.sources) =
    r # _error ?~ "The source resource does not exists"
  | isLeft (r.targetName) =
    r # _error ?~ "Please enter a valid target filename"
  | otherwise =
    r # _error .~ Nothing

checkExists :: Resource -> Array Resource -> Boolean
checkExists r rs =
  let path = getPath r
  in isJust $ findIndex (getPath >>> eq path) rs
