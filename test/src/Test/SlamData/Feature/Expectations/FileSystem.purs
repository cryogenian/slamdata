module Test.SlamData.Feature.Expectations.FileSystem where

import SlamData.Prelude
import Data.Map as Map
import Test.SlamData.Feature.XPaths as XPaths
import XPath as XPath
import Test.Feature (expectDownloadedTextFileToMatchFile, expectNotPresented, expectPresented, expectPresentedNotRepeatedly, expectPresentedWithProperties)
import Test.SlamData.Feature.Monad (SlamFeature)

downloadedTextFileToMatchFile ∷ String → String → String → SlamFeature Unit
downloadedTextFileToMatchFile = expectDownloadedTextFileToMatchFile

file ∷ String → SlamFeature Unit
file =
  expectPresented ∘ XPath.anywhere ∘ XPaths.selectFile

fileNotRepeatedly ∷ String → SlamFeature Unit
fileNotRepeatedly =
  expectPresentedNotRepeatedly ∘ XPath.anywhere ∘ XPaths.selectFile

fileSearchString ∷ String → SlamFeature Unit
fileSearchString string =
  expectPresentedWithProperties
  (Map.singleton "value" $ Just string)
  (XPath.anywhere XPaths.fileSearchInput)

noFile ∷ String → SlamFeature Unit
noFile =
  expectNotPresented ∘ XPath.anywhere ∘ XPaths.selectFile

numberOfFiles ∷ Int → SlamFeature Unit
numberOfFiles i = do
  expectPresented $ XPath.index (XPath.anywhere $ XPaths.nthFile) i
  expectNotPresented $ XPath.index (XPath.anywhere $ XPaths.nthFile) $ i + 1
