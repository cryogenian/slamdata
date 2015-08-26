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

module Driver.File.Path (renderPath) where

import Prelude
import Data.Array (intersect, length, (:), null)
import Data.Either (either)
import Data.Maybe (maybe)
import Data.Path.Pathy ((</>), printPath, canonicalize, sandbox, rootDir, file)
import Model.Path (AnyPath())
import Text.SlamSearch.Parser.Tokens (keyChars)

import qualified Data.String as Str
import qualified Data.Char as Ch

renderPath :: AnyPath -> String
renderPath ap =
  if null $ intersect (Str.toCharArray rendered) ((Ch.fromCharCode 32):keyChars)
  then rendered
  else "\"" <> rendered <> "\""
  where
  rendered = either renderFile renderDir ap
  renderDir path =
    printPath $ canonicalize $ maybe rootDir (rootDir </>) (sandbox rootDir path)

  renderFile path =
    printPath $ canonicalize $
    maybe (rootDir </> file "") (rootDir </>) (sandbox rootDir path)
