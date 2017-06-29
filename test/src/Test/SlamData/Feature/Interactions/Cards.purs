module Test.SlamData.Feature.Interactions.Cards where

import SlamData.Prelude

import Data.Array as Arr
import Data.String as S

import Test.Feature as Feature
import Test.SlamData.Feature.Monad (SlamFeature)
import Test.SlamData.Feature.XPaths as XPaths

import XPath as XPath

import Utils.Array (enumerate)

type ApiVarName = String
type ApiVarType = String
type ApiVarValue = String

addColumn ∷ String → SlamFeature Unit
addColumn str = do
  Feature.click $ XPath.last $ XPath.anywhere $ XPath.anyWithExactAriaLabel "Add column"
  Feature.click $ XPath.last $ XPath.anywhere $ XPath.anyWithExactAriaLabel $ "Select " ⊕ str
  Feature.click $ XPath.last $ XPath.anywhere $ XPath.anyWithExactText "Confirm"

checkFieldInLastDeck ∷ String → SlamFeature Unit
checkFieldInLastDeck labelText =
  Feature.check
    $ XPaths.followingLastPreviousCardGripper
    $ "input" `XPath.withLabelWithExactText` labelText

doSaveInLastCacheCard ∷ SlamFeature Unit
doSaveInLastCacheCard =
  Feature.click (XPath.last $ XPath.anywhere XPaths.saveSubmitButton)

expandNewCardMenu ∷ SlamFeature Unit
expandNewCardMenu = Feature.click (XPath.anywhere XPaths.insertCard)

filterNextActions ∷ String → SlamFeature Unit
filterNextActions =
  Feature.provideFieldValue (XPath.anywhere $ XPath.anyWithExactAriaLabel "Filter next actions")

provideApiVariableBindingsForVariablesCard
  ∷ ApiVarName
  → ApiVarType
  → ApiVarValue
  → SlamFeature Unit
provideApiVariableBindingsForVariablesCard name ty val = do
  provideValueForVariablesCard
  provideTypeForVariablesCard
  provideDefaultValueForVariablesCard
  where
  provideValueForVariablesCard ∷ SlamFeature Unit
  provideValueForVariablesCard = do
    Feature.provideFieldValue
      (XPath.first $ XPath.anywhere $ XPaths.variablesCardVariableName)
      name
    Feature.pressEnter
  provideTypeForVariablesCard ∷ SlamFeature Unit
  provideTypeForVariablesCard = do
    Feature.selectFromDropdown
      (XPath.first $ XPath.anywhere $ XPaths.variablesCardVariableTypeFor name)
      ty
    Feature.pressEnter

  provideDefaultValueForVariablesCard ∷ SlamFeature Unit
  provideDefaultValueForVariablesCard = do
    Feature.provideFieldValue
      (XPath.first $ XPath.anywhere $ XPaths.variablesCardDefaultValueFor name)
      val
    Feature.pressEnter

provideFieldValueInLastDeck ∷ String → String → SlamFeature Unit
provideFieldValueInLastDeck labelText =
  Feature.provideFieldValue
    $ XPaths.followingLastPreviousCardGripper
    $ "input" `XPath.withLabelWithExactText` labelText

provideMdInLastMdCard ∷ String → SlamFeature Unit
provideMdInLastMdCard =
  Feature.provideFieldValue
    $ XPath.last $ XPath.anywhere XPaths.aceEditor

provideQueryInLastQueryCard ∷ String → SlamFeature Unit
provideQueryInLastQueryCard =
  Feature.provideFieldValue
    $ XPath.last $ XPath.anywhere XPaths.aceEditor

provideSaveDestinationInLastCacheCard ∷ String → SlamFeature Unit
provideSaveDestinationInLastCacheCard =
  Feature.provideFieldValue (XPath.last $ XPath.anywhere XPaths.saveDestinationInput)

provideSearchStringInLastSearchCard ∷ String → SlamFeature Unit
provideSearchStringInLastSearchCard =
  Feature.provideFieldValue $ XPath.last $ XPath.anywhere XPaths.searchStringInput

pushRadioButtonInLastDeck ∷ String → SlamFeature Unit
pushRadioButtonInLastDeck labelText =
  Feature.pushRadioButton
    $ XPaths.followingLastPreviousCardGripper
    $ "input" `XPath.withLabelWithExactText` labelText

runQuery ∷ SlamFeature Unit
runQuery =
  Feature.click $ XPath.last $ XPath.anywhere $ XPath.anyWithExactAriaLabel "Run query"

selectFileForLastOpenCard ∷ String → SlamFeature Unit
selectFileForLastOpenCard s =
  selectInMillerColumns $ pure "Filesystem" <> S.split (S.Pattern "/") s

selectFromDropdownInLastDeck ∷ String → String → SlamFeature Unit
selectFromDropdownInLastDeck labelText =
  Feature.selectFromDropdown
    $ XPaths.followingLastPreviousCardGripper
    $ "select" `XPath.withLabelWithExactText` labelText

selectInMillerColumns ∷ Array String → SlamFeature Unit
selectInMillerColumns ps = do
  for_ (enumerate $ Arr.filter (\s → S.length s > 0) ps) \(ix × path) →
    Feature.click $ resourceXPath (ix + one) path
  where
  ariaLabel ∷ String → String
  ariaLabel rPath = "Select " ⊕ rPath

  resourceXPath ∷ Int → String → String
  resourceXPath ix rPath =
    XPath.last
      $ XPath.anywhere
      $ (XPath.nodeAtPosition ix $ XPath.anyWithExactAriaLabel "Column")
      ⊕ XPath.descendantString
      ⊕ (XPath.anyWithExactAriaLabel $ ariaLabel rPath)

trashActiveOrLastCard ∷ SlamFeature Unit
trashActiveOrLastCard =
  Feature.click $ XPath.anywhere $ XPath.anyWithExactAriaLabel "Delete card"

uncheckFieldInLastDeck ∷ String → SlamFeature Unit
uncheckFieldInLastDeck labelText =
  Feature.uncheck
    $ XPaths.followingLastPreviousCardGripper
    $ "input" `XPath.withLabelWithExactText` labelText
