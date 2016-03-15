module Test.SlamData.Feature.Expectations where

import Data.Maybe (Maybe(..))
import Data.Map as Map
import Data.Tuple (Tuple(..))
import Prelude
import Test.Feature (expectPresented, expectNotPresented, expectPresentedWithProperties)
import Test.SlamData.Feature.Monad (SlamFeature())
import Test.SlamData.Feature.XPaths as XPaths

cellsInTableColumnInLastCardToEq
  :: Int -> String -> String -> SlamFeature Unit
cellsInTableColumnInLastCardToEq =
  cellsInTableColumnInLastCard XPath.tdWithThAndTextEq

cellsInTableColumnInLastCardToContain
  :: Int -> String -> String -> SlamFeature Unit
cellsInTableColumnInLastCardToContain =
  cellsInTableColumnInLastCard XPath.tdWithThAndTextContaining

cellsInTableColumnInLastCardToNotEq
  :: Int -> String -> String -> SlamFeature Unit
cellsInTableColumnInLastCardToNotEq =
  cellsInTableColumnInLastCard XPath.tdWithThAndTextNotEq

cellsInTableColumnInLastCardToBeGT
  :: Int -> String -> String -> SlamFeature Unit
cellsInTableColumnInLastCardToBeGT =
  cellsInTableColumnInLastCard XPath.tdWithThAndTextGT

cellsInTableColumnInLastCardToBeLT
  :: Int -> String -> String -> SlamFeature Unit
cellsInTableColumnInLastCardToBeLT =
  cellsInTableColumnInLastCard XPath.tdWithThAndTextLT

cellsInTableColumnInLastCardToEqOneOf
  :: Int -> String -> Array String -> SlamFeature Unit
cellsInTableColumnInLastCardToEqOneOf =
  cellsInTableColumnInLastCard XPath.tdWithThAndTextEqOneOf

cellsInTableColumnInLastCardToNotEqOneOf
  :: Int -> String -> Array String -> SlamFeature Unit
cellsInTableColumnInLastCardToNotEqOneOf =
  cellsInTableColumnInLastCard XPath.tdWithThAndTextNotEqOneOf

cellsInTableColumnInLastCard
  :: forall a
  .  (String -> String -> a -> String)
  -> Int
  -> String
  -> a
  -> SlamFeature Unit
cellsInTableColumnInLastCard f i headerText xs = do
  expectPresented $ XPath.index tdXPath i
  expectNotPresented $ XPath.index trXPath (i + 1)
  where
  trXPath = tableXPath ++ "/tbody/tr"
  thXPath = XPath.thWithExactText headerText
  tdXPath = f tableXPath thXPath xs
  tableXPath = XPath.last (XPath.anywhere XPaths.playButton) `XPath.following` "table"

labelInLastMdCard :: String -> SlamFeature Unit
labelInLastMdCard label =
  expectPresented
    $ (XPath.last $ XPath.anywhere $ XPaths.mdCardTitle)
    `XPath.following` ("label" `XPath.nodeWithExactText` label)

fieldInLastMdCard :: String -> String -> String -> SlamFeature Unit
fieldInLastMdCard labelText inputType value =
  expectPresentedWithProperties valueProperty
    $ (XPath.last $ XPath.anywhere $ XPaths.mdCardTitle)
    `XPath.following` inputXPath
  where
  valueProperty = Map.singleton "value" $ Just value
  inputXPath = XPaths.inputWithLabelAndType labelText inputType

checkableFieldInLastMdCard :: String -> String -> Boolean -> SlamFeature Unit
checkableFieldInLastMdCard labelText inputType checked =
  expectPresentedWithProperties checkedProperty
    $ (XPath.last $ XPath.anywhere $ XPaths.mdCardTitle)
    `XPath.following` inputXPath
  where
  propertyValue = if checked then Just "true" else Nothing
  checkedProperty = Map.singleton "checked" propertyValue
  inputXPath = XPaths.inputWithLabelAndType labelText inputType

dropdownInLastMdCard :: String -> Array String -> SlamFeature Unit
dropdownInLastMdCard value values =
  expectPresentedWithProperties
    (Map.singleton "value" $ Just value)
    $ (XPath.last $ XPath.anywhere $ XPaths.mdCardTitle)
    `XPath.following` XPath.selectWithOptionsWithExactTexts values

lastCardToBeFinished :: SlamFeature Unit
lastCardToBeFinished =
  expectPresented
    $ (XPath.last $ XPath.anywhere $ XPaths.cardHeading)
    `XPath.following` XPath.anyWithText "Finished"
