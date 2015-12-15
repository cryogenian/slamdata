{-
Copyright 2015 SlamData, Inc.

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

module Model.Common where

import Prelude

import Config as Config
import Control.UI.Browser (encodeURIComponent)
import Data.Maybe (Maybe(), fromMaybe)
import Data.Path.Pathy (printPath, dir, (</>))
import Model.AccessType (AccessType(), printAccessType)
import Model.Salt (Salt(), runSalt)
import Model.Sort (Sort(), sort2string)
import Utils.Path (DirPath(), encodeURIPath, (<./>))

browseURL :: Maybe String -> Sort -> Salt -> DirPath -> String
browseURL search sort salt path =
  Config.browserUrl
  <> "#?q=" <> q
  <> "&sort=" <> sort2string sort
  <> "&salt=" <> runSalt salt
  where
  search' =
    (\s -> if s == ""
           then s
           else s <> " ")
    $ fromMaybe "" search

  q = encodeURIComponent
      $ search'
      <> "path:\""
      <> printPath path
      <> "\""

-- Currently the only place where modules from `Model.Notebook` are used
-- is `Controller.File`. I think that it would be better if url will be constructed
-- from things that are already in `FileSystem` (In fact that using of
-- `notebookURL` is redundant, because (state ^. _path) is `DirPath`
-- `theseRight $ That Config.newNotebookName` ≣ `Just Config.newNotebookName`
mkNotebookURL :: String -> DirPath -> AccessType -> String
mkNotebookURL name path access =
  Config.notebookUrl
  <> "#"
  <> encodeURIPath (printPath $ path </> dir name <./> Config.notebookExtension)
  <> printAccessType access
