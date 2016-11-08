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

module Utils.Path
  ( module Utils.Path
  , module Quasar.Types
  ) where

import Prelude

import Control.Alt ((<|>))

import Data.Array (intersect, null, (:))
import Data.Char as Ch
import Data.Either (Either(..), either, fromRight)
import Data.Maybe (Maybe, maybe, fromMaybe)
import Data.Path.Pathy (Sandboxed, Unsandboxed, Abs, Path, File, Rel, Dir, DirName(..), FileName(..), peel, rootDir, (</>), file, canonicalize, printPath, parseAbsDir, parseAbsFile, dir, relativeTo, renameDir)
import Data.Path.Pathy as P
import Data.String as S
import Data.String.Regex as Rgx
import Data.String.Regex.Flags as RXF
import Data.Tuple (snd, fst)

import Global as Global

import Quasar.Types (AnyPath, DirPath, FilePath)

import Partial.Unsafe (unsafePartial)

import SlamData.Config as Config

import Text.SlamSearch.Parser.Tokens (keyChars)

infixl 6 renameDirExt as <./>

renameDirExt :: forall a s. Path a Dir s -> String -> Path a Dir s
renameDirExt p ext = renameDir (changeDirExt $ const ext) p

rootify :: DirPath -> Path Rel Dir Sandboxed
rootify p = fromMaybe (dir "/") $ relativeTo p rootDir

rootifyFile :: FilePath -> Path Rel File Sandboxed
rootifyFile p = fromMaybe (file "") $ relativeTo p rootDir

sandbox ∷ ∀ b. Path Abs b Unsandboxed → Maybe (Path Abs b Sandboxed)
sandbox = map (rootDir </> _) <<< P.sandbox rootDir

-- | Set by default to support cells without
-- | `pathToWorkspace` field. After first save `pathToWorkspace` is
-- | setted to correct workspace path.
phantomWorkspacePath :: DirPath
phantomWorkspacePath = rootDir </> dir "phantom" <./> Config.workspaceExtension

parseAnyPath :: String -> Maybe AnyPath
parseAnyPath s
  = Right <$> parseFilePath s
  <|> Left <$> parseDirPath s

parseFilePath :: String -> Maybe FilePath
parseFilePath s = (rootDir </> _) <$> (P.sandbox rootDir =<< parseAbsFile s)

parseDirPath :: String -> Maybe DirPath
parseDirPath s = (rootDir </> _) <$> (P.sandbox rootDir =<< parseAbsDir s)

changeDirExt :: (String -> String) -> DirName -> DirName
changeDirExt f (DirName name) =
  DirName ((if ext == "" then name else n) <> "." <> f ext)
  where
  mbIdx = S.lastIndexOf (S.Pattern ".") name
  n = maybe name (\idx -> S.take idx name) mbIdx
  ext = maybe "" (\idx -> S.drop (idx + 1) name) mbIdx

dropDirExt :: DirName -> DirName
dropDirExt (DirName d) =
  DirName $ maybe d (\idx -> S.take idx d) $ S.lastIndexOf (S.Pattern ".") d

takeDirExt :: DirName -> String
takeDirExt (DirName d) =
  maybe "" (\idx -> S.drop (idx + 1) d) $ S.lastIndexOf (S.Pattern ".") d

dropWorkspaceExt :: String -> String
dropWorkspaceExt name = S.take (S.length name - S.length Config.workspaceExtension - 1) name

decodeURIPath :: String -> String
decodeURIPath uri =
  Global.decodeURIComponent $
    Rgx.replace (unsafePartial fromRight $ Rgx.regex "\\+" RXF.global) " " uri

encodeURIPath :: String -> String
encodeURIPath path =
  S.joinWith "/" $
  S.joinWith "+" <$>
  (Global.encodeURIComponent <$> _) <$>
  S.split (S.Pattern " ") <$>
  S.split (S.Pattern "/") path

hidePath :: String -> String -> String
hidePath path input =
  S.trim $
  S.replace (S.Pattern $ "+path:\"" <> path <> "\"") (S.Replacement "") $
  S.replace (S.Pattern $ "+path:" <> path) (S.Replacement "") input

renderPath :: AnyPath -> String
renderPath ap =
  if null $ intersect (S.toCharArray rendered) ((Ch.fromCharCode 32):keyChars)
  then rendered
  else "\"" <> rendered <> "\""
  where
  rendered = either renderDir renderFile ap
  renderDir path =
    printPath $ canonicalize $ maybe rootDir (rootDir </> _) (P.sandbox rootDir path)

  renderFile path =
    printPath $ canonicalize $
    maybe (rootDir </> file "") (rootDir </> _) (P.sandbox rootDir path)

getNameStr :: AnyPath -> String
getNameStr ap = either getNameStr' getNameStr' ap
  where
  getNameStr' :: forall b a s. Path a b s -> String
  getNameStr' p = maybe "" (snd >>> nameOfFileOrDir) $ peel p

nameOfFileOrDir :: Either DirName FileName -> String
nameOfFileOrDir (Left (DirName name)) = name
nameOfFileOrDir (Right (FileName name)) = name

getDir :: AnyPath -> DirPath
getDir ap = either getDir' getDir' ap
  where
  getDir' :: forall b. Path Abs b Sandboxed -> DirPath
  getDir' = maybe rootDir fst <<< peel

rootFile ∷ FilePath
rootFile = rootDir </> file ""
