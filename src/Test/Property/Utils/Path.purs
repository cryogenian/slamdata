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

module Test.Property.Utils.Path
  ( ArbFilePath
  , runArbFilePath
  , ArbDirPath
  , runArbDirPath
  ) where

import SlamData.Prelude

import Data.Path.Pathy (Path, Rel, Dir, Sandboxed, (</>), dir, file, rootDir)
import Data.String (null)

import Test.StrongCheck.Arbitrary (class Arbitrary, arbitrary)
import Test.StrongCheck.Gen (Gen, suchThat, chooseInt, vectorOf)

import Utils.Path (FilePath, DirPath)

newtype ArbFilePath = ArbFilePath FilePath

runArbFilePath ∷ ArbFilePath → FilePath
runArbFilePath (ArbFilePath p) = p

instance arbitraryArbFilePath ∷ Arbitrary ArbFilePath where
  arbitrary = do
    numDirs ← chooseInt 1.0 10.0
    dirs ← map dir <$> vectorOf numDirs pathPart
    filename ← file <$> pathPart
    pure $ ArbFilePath $ rootDir </> foldl (flip (</>)) filename (dirs ∷ Array (Path Rel Dir Sandboxed))

newtype ArbDirPath = ArbDirPath DirPath

runArbDirPath ∷ ArbDirPath → DirPath
runArbDirPath (ArbDirPath p) = p

instance arbitraryArbDirPath ∷ Arbitrary ArbDirPath where
  arbitrary = do
    numDirs ← chooseInt 1.0 10.0
    dirs ← map dir <$> vectorOf numDirs pathPart
    last ← dir <$> pathPart
    pure $ ArbDirPath $ rootDir </> foldl (flip (</>)) last (dirs ∷ Array (Path Rel Dir Sandboxed))

pathPart ∷ Gen String
pathPart = suchThat arbitrary (not ∘ null)
