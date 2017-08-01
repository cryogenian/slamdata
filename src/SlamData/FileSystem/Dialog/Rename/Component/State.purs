{-
Copyright 2017 SlamData, Inc.

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

module SlamData.FileSystem.Dialog.Rename.Component.State where

import SlamData.Prelude

import Data.Array as Array
import Data.Lens ((.~))
import Data.Path.Pathy as P
import Data.String as Str
import SlamData.Config as Config
import SlamData.FileSystem.Resource as R
import Utils.Path as UP

type State =
  { initial ∷ R.Resource
  , name ∷ String
  , dir ∷ String
  , showList ∷ Boolean
  , dirs ∷ Array R.Resource
  , value ∷ Either String R.Resource
  , error ∷ Maybe String
  , renaming ∷ Boolean
  }

initialState ∷ R.Resource → State
initialState resource =
  { initial: resource
  , name:
      if R.isWorkspace resource
        then UP.dropWorkspaceExt (R.resourceName resource)
        else R.resourceName resource
  , dir: P.printPath $ R.resourceDir resource
  , showList: false
  , dirs: Array.singleton R.root
  , value: Right resource
  , error: Nothing
  , renaming: false
  }

validate ∷ State → State
validate st = st { value = go st, error = Nothing }
  where
  go ∷ State → Either String R.Resource
  go { initial, name, dir } = do
    pathTip ← validateName name
    pathBase ← validateDirectory dir
    pure $ initial
      # (R._name .~ withWorkspaceExt initial pathTip)
      ∘ (R._root .~ pathBase)
  withWorkspaceExt ∷ R.Resource → String → String
  withWorkspaceExt r name
    | R.isWorkspace r = name <> "." <> Config.workspaceExtension
    | otherwise = name

validateName ∷ String → Either String String
validateName name = do
  when (Str.trim name == "")
    $ Left "Please enter a name"

  when (isJust $ Str.stripSuffix (Str.Pattern $ "." <> Config.workspaceExtension) name)
    $ Left $ "Please choose an alternative name, ."
    <> Config.workspaceExtension
    <> " is a reserved extension"

  when (Str.contains (Str.Pattern "/") name)
    $ Left "Please enter a valid name"

  Right name

validateDirectory ∷ String → Either String UP.DirPath
validateDirectory dir = do
  when (Str.trim dir == "")
    $ Left "Please enter a directory path"

  -- Allow the trailing "/" to be missing, add it as necessary
  let dir' = fromMaybe dir (Str.stripSuffix (Str.Pattern "/") dir) <> "/"

  case UP.parseDirPath dir' of
    Nothing → Left "Please enter a valid directory path"
    Just path → Right path
