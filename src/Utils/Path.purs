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

module Utils.Path where

import Prelude

import Control.Alt ((<|>))
import Control.Bind ((=<<))
import Control.UI.Browser (encodeURIComponent, decodeURIComponent)

import Data.Array (intersect, null, (:))
import Data.Char as Ch
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(), maybe, fromMaybe)
import Data.Path.Pathy
import Data.String (split, joinWith, trim, replace, drop, take, lastIndexOf, length, toCharArray)
import Data.String.Regex as Rgx

import Text.SlamSearch.Parser.Tokens (keyChars)

import Config (notebookExtension)

type FilePath = AbsFile Sandboxed
type DirPath = AbsDir Sandboxed
type AnyPath = Either FilePath DirPath

infixl 6 <./>
(<./>) :: forall a s. Path a Dir s -> String -> Path a Dir s
(<./>) p ext = renameDir (changeDirExt $ const ext) p

rootify :: DirPath -> Path Rel Dir Sandboxed
rootify p = fromMaybe (dir "/") $ relativeTo p rootDir

rootifyFile :: FilePath -> Path Rel File Sandboxed
rootifyFile p = fromMaybe (file "") $ relativeTo p rootDir

-- | Setted by default to support cells without
-- | `pathToNotebook` field. After first save `pathToNotebook` is
-- | setted to correct notebook path.
phantomNotebookPath :: DirPath
phantomNotebookPath = rootDir </> dir "phantom" <./> notebookExtension

parseAnyPath :: String -> Maybe AnyPath
parseAnyPath s = Left <<< (rootDir </>) <$> (sandbox rootDir =<< parseAbsFile s)
             <|> Right <<< (rootDir </>) <$> (sandbox rootDir =<< parseAbsDir s)

changeDirExt :: (String -> String) -> DirName -> DirName
changeDirExt f (DirName name) =
  DirName ((if ext == "" then name else n) <> "." <> f ext)
  where
  mbIdx = lastIndexOf "." name
  n = maybe name (\idx -> take idx name) mbIdx
  ext = maybe "" (\idx -> drop (idx + 1) name) mbIdx


dropDirExt :: DirName -> DirName
dropDirExt (DirName d) =
  DirName $ maybe d (\idx -> take idx d) $ lastIndexOf "." d

takeDirExt :: DirName -> String
takeDirExt (DirName d) =
  maybe "" (\idx -> drop (idx + 1) d) $ lastIndexOf "." d


dropNotebookExt :: String -> String
dropNotebookExt name = take (length name - length Config.notebookExtension - 1) name

decodeURIPath :: String -> String
decodeURIPath uri =
  decodeURIComponent $
  Rgx.replace (Rgx.regex "\\+" Rgx.noFlags{global=true}) " " uri

encodeURIPath :: String -> String
encodeURIPath path =
  joinWith "/" $
  joinWith "+" <$>
  (encodeURIComponent <$>) <$>
  split " " <$>
  split "/" path

hidePath :: String -> String -> String
hidePath path input =
  trim $
  replace ("+path:\"" <> path <> "\"") "" $
  replace ("+path:" <> path) "" input

renderPath :: AnyPath -> String
renderPath ap =
  if null $ intersect (toCharArray rendered) ((Ch.fromCharCode 32):keyChars)
  then rendered
  else "\"" <> rendered <> "\""
  where
  rendered = either renderFile renderDir ap
  renderDir path =
    printPath $ canonicalize $ maybe rootDir (rootDir </>) (sandbox rootDir path)

  renderFile path =
    printPath $ canonicalize $
    maybe (rootDir </> file "") (rootDir </>) (sandbox rootDir path)
