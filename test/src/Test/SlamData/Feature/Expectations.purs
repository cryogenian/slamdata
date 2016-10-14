module Test.SlamData.Feature.Expectations where

import SlamData.Prelude

import Data.Map as Map
import Selenium.Monad (tryRepeatedlyTo)
import Test.Feature (expectPresented, expectNotPresented, expectPresentedWithProperties, expectDownloadedTextFileToMatchFile, expectSelectValue, expectPresentedNotRepeatedly)
import Test.SlamData.Feature.Monad (SlamFeature)
import Test.SlamData.Feature.XPaths as XPaths
import XPath as XPath

cardsInTableColumnInLastCardToEq
  ∷ Int → String → String → SlamFeature Unit
cardsInTableColumnInLastCardToEq =
  cardsInTableColumnInLastCard XPath.tdWithThAndTextEq

cardsInTableColumnInLastCardToContain
  ∷ Int → String → String → SlamFeature Unit
cardsInTableColumnInLastCardToContain =
  cardsInTableColumnInLastCard XPath.tdWithThAndTextContaining

cardsInTableColumnInLastCardToNotEq
  ∷ Int → String → String → SlamFeature Unit
cardsInTableColumnInLastCardToNotEq =
  cardsInTableColumnInLastCard XPath.tdWithThAndTextNotEq

cardsInTableColumnInLastCardToBeGT
  ∷ Int → String → String → SlamFeature Unit
cardsInTableColumnInLastCardToBeGT =
  cardsInTableColumnInLastCard XPath.tdWithThAndTextGT

cardsInTableColumnInLastCardToBeLT
  ∷ Int → String → String → SlamFeature Unit
cardsInTableColumnInLastCardToBeLT =
  cardsInTableColumnInLastCard XPath.tdWithThAndTextLT

cardsInTableColumnInLastCardToEqOneOf
  ∷ Int → String → Array String → SlamFeature Unit
cardsInTableColumnInLastCardToEqOneOf =
  cardsInTableColumnInLastCard XPath.tdWithThAndTextEqOneOf

cardsInTableColumnInLastCardToNotEqOneOf
  ∷ Int → String → Array String → SlamFeature Unit
cardsInTableColumnInLastCardToNotEqOneOf =
  cardsInTableColumnInLastCard XPath.tdWithThAndTextNotEqOneOf

cardsInTableColumnInLastCard
  ∷ forall a
  . (String → String → a → String)
  → Int
  → String
  → a
  → SlamFeature Unit
cardsInTableColumnInLastCard f i headerText xs = do
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

tableColumnsAre ∷ ∀ f. Foldable f ⇒ f String → SlamFeature Unit
tableColumnsAre expectedTexts =
  for_ expectedTexts \et →
    expectPresented (XPath.anywhere $ XPath.nodeWithExactText "th" et)

labelInLastMdCard ∷ String → SlamFeature Unit
labelInLastMdCard label =
  expectPresented
    $ XPath.anywhere "label" `XPath.nodeWithExactText` label

fieldInLastMdCard ∷ String → String → String → SlamFeature Unit
fieldInLastMdCard labelText inputType value =
  expectPresentedWithProperties valueProperty
    $ XPath.anywhere inputXPath
  where
  valueProperty = Map.singleton "value" $ Just value
  inputXPath = XPaths.inputWithLabelAndType labelText inputType

checkableFieldInLastMdCard ∷ String → String → Boolean → SlamFeature Unit
checkableFieldInLastMdCard labelText inputType checked =
  expectPresentedWithProperties checkedProperty
    $ XPath.anywhere inputXPath
  where
  propertyValue = if checked then Just "true" else Nothing
  checkedProperty = Map.singleton "checked" propertyValue
  inputXPath = XPaths.inputWithLabelAndType labelText inputType

dropdownInLastMdCard ∷ String → Array String → SlamFeature Unit
dropdownInLastMdCard value values =
  expectPresentedWithProperties
    (Map.singleton "value" $ Just value)
    $ XPath.anywhere $ XPath.selectWithOptionsWithExactTexts values

resourceOpenedInLastOpenCard ∷ String → SlamFeature Unit
resourceOpenedInLastOpenCard fileName =
  expectPresented (XPath.last $ XPath.anywhere $ XPaths.resourceOpened fileName)


file ∷ String → SlamFeature Unit
file =
  expectPresented ∘ XPath.anywhere ∘ XPaths.selectFile

fileNotRepeatedly ∷ String → SlamFeature Unit
fileNotRepeatedly =
  expectPresentedNotRepeatedly ∘ XPath.anywhere ∘ XPaths.selectFile

noFile ∷ String → SlamFeature Unit
noFile =
  expectNotPresented ∘ XPath.anywhere ∘ XPaths.selectFile

numberOfFiles ∷ Int → SlamFeature Unit
numberOfFiles i = do
  expectPresented $ XPath.index (XPath.anywhere $ XPaths.nthFile) i
  expectNotPresented $ XPath.index (XPath.anywhere $ XPaths.nthFile) $ i + 1

workspaceName ∷ String → SlamFeature Unit
workspaceName name =
  expectPresentedWithProperties
    (Map.singleton "value" $ Just name)
    (XPath.anywhere "input")

text ∷ String → SlamFeature Unit
text = expectPresented ∘ XPath.anywhere ∘ XPath.anyWithText

textEventually ∷ String → SlamFeature Unit
textEventually = tryRepeatedlyTo ∘ text

downloadedTextFileToMatchFile ∷ String → String → String → SlamFeature Unit
downloadedTextFileToMatchFile = expectDownloadedTextFileToMatchFile

measureDisabledInLastChartCard
  ∷ SlamFeature Unit
measureDisabledInLastChartCard =
  expectPresentedWithProperties
    (Map.singleton "disabled" (Just "true"))
    (XPath.last $ XPath.anywhere $ XPaths.chartMeasureOneSelector)

measureInLastChartCard
  ∷ String
  → SlamFeature Unit
measureInLastChartCard value =
  expectSelectValue
    value
    (XPath.last $ XPath.anywhere $ XPaths.chartMeasureOneSelector)

fileSearchString ∷ String → SlamFeature Unit
fileSearchString string =
  expectPresentedWithProperties
  (Map.singleton "value" $ Just string)
  (XPath.anywhere XPaths.fileSearchInput)

lastEChart ∷ String → SlamFeature Unit
lastEChart =
  expectPresented
    ∘ XPath.last
    ∘ XPath.anywhere
    ∘ XPaths.eChart

displayMarkdownCardPresented ∷ SlamFeature Unit
displayMarkdownCardPresented =
  tryRepeatedlyTo
    $ expectPresented
    $ XPath.anywhere
    $ XPaths.displayMarkdownCardHeader

troubleshootCardPresented ∷ SlamFeature Unit
troubleshootCardPresented =
  tryRepeatedlyTo
    $ expectPresented
    $ XPath.anywhere
    $ XPaths.troubleshootCardHeader

tableCardPresented ∷ SlamFeature Unit
tableCardPresented =
  tryRepeatedlyTo
    $ expectPresented
    $ XPath.anywhere
    $ XPaths.tableCardHeader

textInDisplayMarkdownCard ∷ String → SlamFeature Unit
textInDisplayMarkdownCard =
  tryRepeatedlyTo
    ∘ expectPresented
    ∘ XPath.anywhere
    ∘ XPath.nodeWithExactText "p"

backsideActionNotPresented ∷ String → SlamFeature Unit
backsideActionNotPresented =
  tryRepeatedlyTo
    ∘ expectNotPresented
    ∘ XPath.anywhere

trashButtonPresented ∷ SlamFeature Unit
trashButtonPresented =
  tryRepeatedlyTo
    $ expectPresented
    $ XPath.anywhere
    $ XPaths.trashCardAction

shareButtonPresented ∷ SlamFeature Unit
shareButtonPresented =
  tryRepeatedlyTo
    $ expectPresented
    $ XPath.anywhere
    $ XPaths.shareDeckAction

publishButtonPresented ∷ SlamFeature Unit
publishButtonPresented =
  tryRepeatedlyTo
    $ expectPresented
    $ XPath.anywhere
    $ XPaths.publishDeckAction

embedButtonPresented ∷ SlamFeature Unit
embedButtonPresented =
  tryRepeatedlyTo
    $ expectPresented
    $ XPath.anywhere
    $ XPaths.embedDeckAction

backsideMenuPresented ∷ SlamFeature Unit
backsideMenuPresented = do
  trashButtonPresented
  publishButtonPresented
  embedButtonPresented

backsideMenuNotPresented ∷ SlamFeature Unit
backsideMenuNotPresented = do
  for_
    [ XPaths.trashCardAction
    , XPaths.publishDeckAction
    , XPaths.embedDeckAction
    ]
    backsideActionNotPresented

noTablesPresented ∷ SlamFeature Unit
noTablesPresented =
  tryRepeatedlyTo
    $ expectNotPresented
    $ XPath.anywhere
    $ XPaths.tableHeading

onlyTrashActionPresented ∷ SlamFeature Unit
onlyTrashActionPresented = do
  trashButtonPresented
  for_
    [ XPaths.publishDeckAction
    , XPaths.embedDeckAction
    ]
    backsideActionNotPresented

onlyEmbedActionPresented ∷ SlamFeature Unit
onlyEmbedActionPresented = do
  embedButtonPresented
  for_
    [ XPaths.shareDeckAction
    , XPaths.publishDeckAction
    , XPaths.trashCardAction
    ]
    backsideActionNotPresented

onlyPublishActionPresented ∷ SlamFeature Unit
onlyPublishActionPresented = do
  publishButtonPresented
  for_
    [ XPaths.embedDeckAction
    , XPaths.trashCardAction
    ]
    backsideActionNotPresented
