module Test.SlamData.Feature.Expectations where

import SlamData.Prelude

import Data.Map as Map
import Selenium.Monad (tryRepeatedlyTo)
import Test.Feature (expectPresented, expectNotPresented, expectPresentedWithProperties, expectDownloadedTextFileToMatchFile, expectScreenshotToMatchAny, expectSelectValue)
import Test.SlamData.Feature.Monad (SlamFeature)
import Test.SlamData.Feature.XPaths as XPaths

cellsInTableColumnInLastCardToEq
  ∷ Int → String → String → SlamFeature Unit
cellsInTableColumnInLastCardToEq =
  cellsInTableColumnInLastCard XPath.tdWithThAndTextEq

cellsInTableColumnInLastCardToContain
  ∷ Int → String → String → SlamFeature Unit
cellsInTableColumnInLastCardToContain =
  cellsInTableColumnInLastCard XPath.tdWithThAndTextContaining

cellsInTableColumnInLastCardToNotEq
  ∷ Int → String → String → SlamFeature Unit
cellsInTableColumnInLastCardToNotEq =
  cellsInTableColumnInLastCard XPath.tdWithThAndTextNotEq

cellsInTableColumnInLastCardToBeGT
  ∷ Int → String → String → SlamFeature Unit
cellsInTableColumnInLastCardToBeGT =
  cellsInTableColumnInLastCard XPath.tdWithThAndTextGT

cellsInTableColumnInLastCardToBeLT
  ∷ Int → String → String → SlamFeature Unit
cellsInTableColumnInLastCardToBeLT =
  cellsInTableColumnInLastCard XPath.tdWithThAndTextLT

cellsInTableColumnInLastCardToEqOneOf
  ∷ Int → String → Array String → SlamFeature Unit
cellsInTableColumnInLastCardToEqOneOf =
  cellsInTableColumnInLastCard XPath.tdWithThAndTextEqOneOf

cellsInTableColumnInLastCardToNotEqOneOf
  ∷ Int → String → Array String → SlamFeature Unit
cellsInTableColumnInLastCardToNotEqOneOf =
  cellsInTableColumnInLastCard XPath.tdWithThAndTextNotEqOneOf

cellsInTableColumnInLastCard
  ∷ forall a
  . (String → String → a → String)
  → Int
  → String
  → a
  → SlamFeature Unit
cellsInTableColumnInLastCard f i headerText xs = do
  expectPresented $ XPath.index tdXPath i
  expectNotPresented $ XPath.index trXPath (i + 1)
  where
  trXPath =
    tableXPath ++ "/tbody/tr"
  thXPath =
    XPath.thWithExactText headerText
  tdXPath =
    f tableXPath thXPath xs
  tableXPath =
    XPath.last (XPath.anywhere XPaths.jtableHeading) `XPath.following` "table"

labelInLastMdCard ∷ String → SlamFeature Unit
labelInLastMdCard label =
  expectPresented
    $ (XPath.anywhere $ XPaths.formCardTitle)
    `XPath.following` ("label" `XPath.nodeWithExactText` label)

fieldInLastMdCard ∷ String → String → String → SlamFeature Unit
fieldInLastMdCard labelText inputType value =
  expectPresentedWithProperties valueProperty
    $ (XPath.anywhere $ XPaths.formCardTitle)
    `XPath.following` inputXPath
  where
  valueProperty = Map.singleton "value" $ Just value
  inputXPath = XPaths.inputWithLabelAndType labelText inputType

checkableFieldInLastMdCard ∷ String → String → Boolean → SlamFeature Unit
checkableFieldInLastMdCard labelText inputType checked =
  expectPresentedWithProperties checkedProperty
    $ (XPath.anywhere $ XPaths.formCardTitle)
    `XPath.following` inputXPath
  where
  propertyValue = if checked then Just "true" else Nothing
  checkedProperty = Map.singleton "checked" propertyValue
  inputXPath = XPaths.inputWithLabelAndType labelText inputType

dropdownInLastMdCard ∷ String → Array String → SlamFeature Unit
dropdownInLastMdCard value values =
  expectPresentedWithProperties
    (Map.singleton "value" $ Just value)
    $ (XPath.anywhere $ XPaths.formCardTitle)
    `XPath.following` XPath.selectWithOptionsWithExactTexts values

lastCardToBeFinished ∷ SlamFeature Unit
lastCardToBeFinished =
  expectPresented
    $ (XPath.last $ XPath.anywhere $ XPaths.cardHeading)
    `XPath.following` XPath.anyWithText "Finished"

exploreFileInLastCard ∷ String → SlamFeature Unit
exploreFileInLastCard fileName =
  expectPresentedWithProperties
    (Map.singleton "value" $ Just fileName)
    ((XPath.last $ XPath.anywhere $ XPaths.cardHeading) `XPath.following` XPaths.exploreInput)

file ∷ String → SlamFeature Unit
file =
  expectPresented ∘ XPath.anywhere ∘ XPaths.selectFile

noFile ∷ String → SlamFeature Unit
noFile =
  expectNotPresented ∘ XPath.anywhere ∘ XPaths.selectFile

numberOfFiles ∷ Int → SlamFeature Unit
numberOfFiles i = do
  expectPresented $ XPath.index (XPath.anywhere $ XPaths.nthFile) i
  expectNotPresented $ XPath.index (XPath.anywhere $ XPaths.nthFile) $ i + 1

notebookName ∷ String → SlamFeature Unit
notebookName name =
  expectPresentedWithProperties
    (Map.singleton "value" $ Just name)
    (XPath.anywhere "input")

text ∷ String → SlamFeature Unit
text = expectPresented ∘ XPath.anywhere ∘ XPath.anyWithText

textEventually ∷ String → SlamFeature Unit
textEventually = tryRepeatedlyTo ∘ text

downloadedTextFileToMatchFile ∷ String → String → String → SlamFeature Unit
downloadedTextFileToMatchFile = expectDownloadedTextFileToMatchFile

measureDisabledInLastVisualizeCard
  ∷ SlamFeature Unit
measureDisabledInLastVisualizeCard =
  expectPresentedWithProperties
    (Map.singleton "disabled" (Just "true"))
    (XPath.last $ XPath.anywhere $ XPaths.chartMeasureOneSelector)

measureInLastVisualizeCard
  ∷ String
  → SlamFeature Unit
measureInLastVisualizeCard value =
  expectSelectValue
    value
    (XPath.last $ XPath.anywhere $ XPaths.chartMeasureOneSelector)

lastChartScreenshotToMatchAny
  ∷ Array String
  → SlamFeature Unit
lastChartScreenshotToMatchAny =
  expectScreenshotToMatchAny
    "test/image/diff.png"
    0.2
    (XPath.last $ XPath.anywhere $ XPaths.chartContainer)
    "test/image/actual.png"

fileSearchString ∷ String → SlamFeature Unit
fileSearchString string =
  expectPresentedWithProperties
  (Map.singleton "value" $ Just string)
  (XPath.anywhere XPaths.fileSearchInput)

textInFormCell ∷ String → SlamFeature Unit
textInFormCell =
  tryRepeatedlyTo
    ∘ expectPresented
    ∘ XPath.anywhere
    ∘ XPath.following XPaths.formCellHeader
    ∘ XPath.anyWithText
