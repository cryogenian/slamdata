module Test.SlamData.Feature.Expectations where

import SlamData.Prelude

import Data.Map as Map
import Selenium.Monad (tryRepeatedlyTo)
import Test.Feature (expectPresented, expectNotPresented, expectPresentedWithProperties, expectDownloadedTextFileToMatchFile, expectSelectValue, expectPresentedNotRepeatedly)
import Test.SlamData.Feature.Monad (SlamFeature)
import Test.SlamData.Feature.XPaths as XPaths

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

tableColumnsAre ∷ ∀ f. Foldable f ⇒ f String → SlamFeature Unit
tableColumnsAre expectedTexts =
  for_ expectedTexts \et →
    expectPresented (XPath.anywhere $ XPath.nodeWithExactText "th" et)

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

resourceOpenedInLastExploreCard ∷ String → SlamFeature Unit
resourceOpenedInLastExploreCard fileName =
  expectPresented (XPath.last $ XPath.anywhere $ XPaths.resourceOpened fileName)


exploreFileInLastCard ∷ String → SlamFeature Unit
exploreFileInLastCard fileName =
  expectPresentedWithProperties
    (Map.singleton "value" $ Just fileName)
    ((XPath.last $ XPath.anywhere $ XPaths.cardHeading) `XPath.following` XPaths.exploreInput)

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

fileSearchString ∷ String → SlamFeature Unit
fileSearchString string =
  expectPresentedWithProperties
  (Map.singleton "value" $ Just string)
  (XPath.anywhere XPaths.fileSearchInput)

lastEChartOptions ∷ String → SlamFeature Unit
lastEChartOptions =
  expectPresented
    ∘ XPath.last
    ∘ XPath.anywhere
    ∘ XPaths.eChartOptions

textInFormCard ∷ String → SlamFeature Unit
textInFormCard =
  tryRepeatedlyTo
    ∘ expectPresented
    ∘ XPath.anywhere
    ∘ XPath.following XPaths.formCardHeader
    ∘ XPath.anyWithText

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
  shareButtonPresented
  publishButtonPresented
  embedButtonPresented

backsideMenuNotPresented ∷ SlamFeature Unit
backsideMenuNotPresented = do
  for_
    [ XPaths.trashCardAction
    , XPaths.shareDeckAction
    , XPaths.publishDeckAction
    , XPaths.embedDeckAction
    ]
    backsideActionNotPresented

noJTablesPresented ∷ SlamFeature Unit
noJTablesPresented =
  tryRepeatedlyTo
    $ expectNotPresented
    $ XPath.anywhere
    $ XPaths.jtableHeading

onlyTrashActionPresented ∷ SlamFeature Unit
onlyTrashActionPresented = do
  trashButtonPresented
  for_
    [ XPaths.shareDeckAction
    , XPaths.publishDeckAction
    , XPaths.embedDeckAction
    ]
    backsideActionNotPresented

onlyShareActionPresented ∷ SlamFeature Unit
onlyShareActionPresented = do
  shareButtonPresented
  for_
    [ XPaths.trashCardAction
    , XPaths.publishDeckAction
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
    [ XPaths.shareDeckAction
    , XPaths.embedDeckAction
    , XPaths.trashCardAction
    ]
    backsideActionNotPresented
