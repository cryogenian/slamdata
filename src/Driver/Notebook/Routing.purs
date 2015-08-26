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

module Driver.Notebook.Routing where

import Prelude
import Config (notebookExtension)
import Control.Alt ((<|>))
import Control.Apply ((*>), (<*))
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Maybe.Unsafe (fromJust)
import Data.List (List(..), last, init)
import Data.Path.Pathy ((</>), rootDir, dir, file)
import Data.String (indexOf, length)
import Data.String.Regex (noFlags, regex, test, Regex())
import Data.Tuple (Tuple(..))
import Model.Action (Action(..), string2action)
import Model.Notebook.Cell (CellId(), string2cellId)
import Model.Resource (Resource(..), newNotebook, newFile, _filePath)
import Optic.Core
import Routing.Match (Match(), list, eitherMatch)
import Routing.Match.Class (lit, str)


data Routes
  = CellRoute Resource CellId Action
  | ExploreRoute Resource
  | NotebookRoute Resource Action

routing :: Match Routes
routing = ExploreRoute <$> (oneSlash *> lit "explore" *> (fileFromParts <$> list str))
      <|> CellRoute <$> notebook <*> (lit "cells" *> cellId) <*> action
      <|> NotebookRoute <$> notebook <*> action
  where

  partsAndName :: Match (Tuple (List String) String)
  partsAndName = Tuple <$> (oneSlash *> (list notName)) <*> name

  notebookFromParts :: Tuple (List String) String -> Resource
  notebookFromParts (Tuple ps name) =
    Notebook $ foldl (</>) rootDir (dir <$> ps) </> dir name

  fileFromParts :: List String -> Resource
  fileFromParts (Cons name Nil) = newFile # _filePath .~ rootDir </> file name
  fileFromParts ps =
    let name = fromJust $ last ps
        ps' = fromJust $ init ps
    in newFile # _filePath .~ foldl (</>) rootDir (dir <$> ps') </> file name

  notebook :: Match Resource
  notebook = notebookFromParts <$> partsAndName

  oneSlash :: Match Unit
  oneSlash = lit ""

  name :: Match String
  name = eitherMatch (notebookName <$> str)

  notName :: Match String
  notName = eitherMatch (pathPart <$> str)

  action :: Match Action
  action = (eitherMatch (string2action <$> str)) <|> pure View

  cellId :: Match CellId
  cellId = eitherMatch (string2cellId <$> str)


  notebookName :: String -> Either String String
  notebookName input | checkExtension input = Right input
                     | otherwise = Left input

  pathPart :: String -> Either String String
  pathPart input | input == "" || checkExtension input = Left "incorrect path part"
                 | otherwise = Right input


  extensionRegex :: Regex
  extensionRegex = regex ("\\." <> notebookExtension <> "$") noFlags

  checkExtension :: String -> Boolean
  checkExtension = test extensionRegex

