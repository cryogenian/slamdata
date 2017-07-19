module Test.SlamData.Feature.Expectations.Cards where

import SlamData.Prelude

import Data.Map as Map
import Test.Feature (expectNotPresentedWithProperties, expectPresented, expectNotPresented, expectPresentedWithProperties)
import Test.Feature.Log (annotate)
import Test.SlamData.Feature.Monad (SlamFeature)
import Test.SlamData.Feature.XPaths as XPaths
import XPath as XPath

cellsInTableColumnInLastCardToEq
  ∷ Int → String → String → SlamFeature Unit
cellsInTableColumnInLastCardToEq number s1 s2=
  annotate ("Found "
            <> show number <> " expected to equal, in columns: "
            <> s1 <> " "
            <> s2)
    $ (cellsInTableColumnInLastCard XPath.tdWithThAndTextEq) number s1 s2

cellsInTableColumnInLastCardToContain
  ∷ Int → String → String → SlamFeature Unit
cellsInTableColumnInLastCardToContain number s1 s2 =
  annotate ("Found expected column")
    $ (cellsInTableColumnInLastCard XPath.tdWithThAndTextContaining) number s1 s2

cellsInTableColumnInLastCardToNotEq
  ∷ Int → String → String → SlamFeature Unit
cellsInTableColumnInLastCardToNotEq =
  cellsInTableColumnInLastCard XPath.tdWithThAndTextNotEq

cellsInTableColumnInLastCardToBeGT
  ∷ Int → String → String → SlamFeature Unit
cellsInTableColumnInLastCardToBeGT number s1 s2 =
  annotate ("Found "
            <> show number <> " expected to be greater, in columns: "
            <> s1 <> " "
            <> s2)
    $ (cellsInTableColumnInLastCard XPath.tdWithThAndTextGT) number s1 s2

cellsInTableColumnInLastCardToBeLT
  ∷ Int → String → String → SlamFeature Unit
cellsInTableColumnInLastCardToBeLT number s1 s2 =
  annotate "Found cell in table column to be of lower value"
    $ (cellsInTableColumnInLastCard XPath.tdWithThAndTextLT) number s1 s2

cellsInTableColumnInLastCardToEqOneOf
  ∷ Int → String → Array String → SlamFeature Unit
cellsInTableColumnInLastCardToEqOneOf number s arrayS =
  annotate ("Found "
            <> show number <> " expected to equal, in column: "
            <> s)
    $ (cellsInTableColumnInLastCard XPath.tdWithThAndTextEqOneOf) number s arrayS

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
--  expectNotPresented $ XPath.index trXPath (i + 1)
  where
  trXPath =
    tableXPath <> "/tr"
  thXPath =
    XPath.thWithExactText headerText
  tdXPath =
    f tableXPath thXPath xs
  tableXPath =
    XPath.last $ XPath.anywhere "table"

categoryEnabledInLastBuildChartCard ∷ SlamFeature Unit
categoryEnabledInLastBuildChartCard =
  annotate "Enabled category in Build Chart card" do
    let
      catXPath = XPath.last $ XPath.anywhere XPaths.chartCategorySelector
    expectPresented catXPath
    expectNotPresentedWithProperties
      (Map.singleton "disabled" (Just "true"))
      catXPath

addColumnIsPresent ∷ SlamFeature Unit
addColumnIsPresent =
  expectPresented
    $ XPath.anywhere
    $ XPaths.addColumnAction

checkableFieldInLastMdCard ∷ String → String → Boolean → SlamFeature Unit
checkableFieldInLastMdCard labelText inputType checked =
  annotate ("Found expected checkable fields "
            <> labelText <> " "
            <> inputType <> " "
            <> show checked)
    $ expectPresentedWithProperties checkedProperty
        $ XPath.anywhere inputXPath
      where
      propertyValue = if checked then Just "true" else Nothing
      checkedProperty = Map.singleton "checked" propertyValue
      inputXPath = XPaths.inputWithLabelAndType labelText inputType

columnHeaderInSetupPivotTableCard ∷ String → SlamFeature Unit
columnHeaderInSetupPivotTableCard string =
  annotate ("Found expected column header " <> string)
    $ (expectPresented <<< XPath.anywhere <<< XPath.nodeWithExactAttribute "placeholder" XPath.any) string

displayMarkdownCardPresented ∷ SlamFeature Unit
displayMarkdownCardPresented =
  annotate "Found expected Markdown card"
    $ expectPresented
      $ XPath.anywhere
      $ XPaths.displayMarkdownCardHeader

dropdownInLastMdCard ∷ String → Array String → SlamFeature Unit
dropdownInLastMdCard value values =
  annotate ("Found expected value " <> value <> " in dropdown")
    $ expectPresentedWithProperties
        (Map.singleton "value" $ Just value)
        $ XPath.anywhere $ XPath.selectWithOptionsWithExactTexts values

fieldInLastMdCard ∷ String → String → String → SlamFeature Unit
fieldInLastMdCard labelText inputType value =
  annotate ("Found expected fields "
            <> labelText <> " "
            <> inputType <> " " <> value)
    $ expectPresentedWithProperties valueProperty
      $ XPath.anywhere inputXPath
    where
    valueProperty = Map.singleton "value" $ Just value
    inputXPath = XPaths.inputWithLabelAndType labelText inputType

labelInLastMdCard ∷ String → SlamFeature Unit
labelInLastMdCard label =
  annotate ("Found expected label " <> label)
    $ expectPresented
      $ XPath.anywhere "label" `XPath.nodeWithExactText` label

measureDisabledInLastChartCard ∷ SlamFeature Unit
measureDisabledInLastChartCard =
  expectPresentedWithProperties
    (Map.singleton "disabled" (Just "true"))
    (XPath.last $ XPath.anywhere $ XPaths.chartMeasureSelector)

measureInLastChartCard ∷ String → SlamFeature Unit
measureInLastChartCard value =
  expectPresented
    $ XPath.last
    $ XPath.anywhere
    $ XPath.nodeWithText XPaths.chartMeasureSelector value

noTablesPresented ∷ SlamFeature Unit
noTablesPresented =
  annotate "Found no table presented as expected"
    $ expectNotPresented
        $ XPath.anywhere
        $ XPaths.tableHeading

resourceOpenedInLastOpenCard ∷ String → SlamFeature Unit
resourceOpenedInLastOpenCard fileName =
  expectPresented (XPath.last $ XPath.anywhere $ XPaths.resourceOpened fileName)

tableCardPresented ∷ SlamFeature Unit
tableCardPresented =
  expectPresented
    $ XPath.anywhere
    $ XPaths.tableCardHeader

tableColumnsAre ∷ ∀ f. Foldable f ⇒ f String → SlamFeature Unit
tableColumnsAre expectedTexts =
  for_ expectedTexts \et →
    annotate ("Found expected table column " <> et)
     $ expectPresented (XPath.anywhere $ XPath.nodeWithExactText "th" et)

textInDisplayMarkdownCard ∷ String → SlamFeature Unit
textInDisplayMarkdownCard text =
  annotate ("Found expected text: " <> text)
    $ (expectPresented
       ∘ XPath.anywhere
       ∘ XPath.nodeWithExactText "p") text

troubleshootCardPresented ∷ SlamFeature Unit
troubleshootCardPresented =
  annotate "Found expected Troubleshoot card"
    $ expectPresented
      $ XPath.anywhere
      $ XPaths.troubleshootCardHeader
