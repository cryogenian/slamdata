module Test.SlamData.Feature.Expectations.Cards where

import SlamData.Prelude
import Data.Map as Map
import Test.SlamData.Feature.XPaths as XPaths
import XPath as XPath
import Test.Feature ( expectNotPresentedWithProperties
                    , expectPresented
                    , expectNotPresented
                    , expectPresentedWithProperties
                    )
import Test.SlamData.Feature.Monad (SlamFeature)

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
categoryEnabledInLastBuildChartCard = do
  let
    catXPath = XPath.last $ XPath.anywhere XPaths.chartCategorySelector
  expectPresented catXPath
  expectNotPresentedWithProperties
    (Map.singleton "disabled" (Just "true"))
    catXPath

checkableFieldInLastMdCard ∷ String → String → Boolean → SlamFeature Unit
checkableFieldInLastMdCard labelText inputType checked =
  expectPresentedWithProperties checkedProperty
    $ XPath.anywhere inputXPath
  where
  propertyValue = if checked then Just "true" else Nothing
  checkedProperty = Map.singleton "checked" propertyValue
  inputXPath = XPaths.inputWithLabelAndType labelText inputType

columnHeaderInSetupPivotTableCard ∷ String → SlamFeature Unit
columnHeaderInSetupPivotTableCard =
  expectPresented <<< XPath.anywhere <<< XPath.nodeWithExactAttribute "placeholder" XPath.any

displayMarkdownCardPresented ∷ SlamFeature Unit
displayMarkdownCardPresented =
  expectPresented
    $ XPath.anywhere
    $ XPaths.displayMarkdownCardHeader

dropdownInLastMdCard ∷ String → Array String → SlamFeature Unit
dropdownInLastMdCard value values =
  expectPresentedWithProperties
    (Map.singleton "value" $ Just value)
    $ XPath.anywhere $ XPath.selectWithOptionsWithExactTexts values

fieldInLastMdCard ∷ String → String → String → SlamFeature Unit
fieldInLastMdCard labelText inputType value =
  expectPresentedWithProperties valueProperty
    $ XPath.anywhere inputXPath
  where
  valueProperty = Map.singleton "value" $ Just value
  inputXPath = XPaths.inputWithLabelAndType labelText inputType

labelInLastMdCard ∷ String → SlamFeature Unit
labelInLastMdCard label =
  expectPresented
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
  expectNotPresented
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
    expectPresented (XPath.anywhere $ XPath.nodeWithExactText "th" et)

textInDisplayMarkdownCard ∷ String → SlamFeature Unit
textInDisplayMarkdownCard =
  expectPresented
    ∘ XPath.anywhere
    ∘ XPath.nodeWithExactText "p"

troubleshootCardPresented ∷ SlamFeature Unit
troubleshootCardPresented =
  expectPresented
    $ XPath.anywhere
    $ XPaths.troubleshootCardHeader
