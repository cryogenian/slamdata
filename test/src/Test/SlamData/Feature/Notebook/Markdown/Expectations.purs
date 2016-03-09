module Test.SlamData.Feature.Notebook.Markdown.Expectations where

import Prelude

--import Test.SlamData.Feature.Expect (Expectation(), expect, toEq, toNotEq, toBeGreaterThan, toMatch)
--import Data.String.Regex (regex, noFlags)
--import Data.String as String
--import Data.Int (toNumber)
--import Selenium.Combinators (tryToFind)
--import Selenium.Monad (byXPath, getText)
import Test.SlamData.Feature.Monad (SlamFeature())
import Test.Feature (expectPresented, expectPresentedWithProperties)
import XPath as XPath
import Test.SlamData.Feature.XPaths as XPaths
import Test.SlamData.Feature.Properties as Properties
import Data.Maybe (Maybe(..))
--import Test.SlamData.Feature.Notebook.Expectations (expectFinishedMessage)
--
--import Data.Foldable (traverse_)

--expectMdQueryResultsToBeFilteredByDefaultFormValues :: SlamFeature Unit
--expectMdQueryResultsToBeFilteredByDefaultFormValues = do
--  expectMdQueryRows 10
--  expectMdQueryColumn "discipline" toEq "Figure skating"
--  expectMdQueryColumn "country" toEq "AUT"
--  expectMdQueryColumn "gender" toEq "W"
--  expectMdQueryColumn "year" toBeGreaterThan $ toNumber 1924
--  expectMdQueryColumn "type" toNotEq "Gold"
--
--expectMdQueryRows :: Int -> SlamFeature Unit
--expectMdQueryRows = void <<< findByXPath <<< XPath.index (XPath.anywhere xPath)
--  where
--  xPath = "*[text()='Markdown']/following::*[text()='Query']/following::tbody/tr"
--
--expectMdQueryResultsToBeFilteredByChangedFormValues :: SlamFeature Unit
--expectMdQueryResultsToBeFilteredByChangedFormValues = do
--  expectMdQueryRows 8
--  expectMdQueryColumn "discipline" toEq "Luge"
--  expectMdQueryColumn "year" toBeGreaterThan $ toNumber 1950
--  expectMdQueryColumn "country" toEq "GDR"
--  expectMdQueryColumn "gender" toMatch $ regex "^(M|X)$" noFlags
--  expectMdQueryColumn "type" toNotEq "Gold"
--
--expectMdQueryColumn :: String -> String -> SlamFeature Unit
--expectMdQueryColumn heading preducate =
--  expectPresented $ XPath.anywhere $ tdWithTh tableXPath thXPath tdXPath
--  where
--  tableXPath =
--    XPaths.mdCellTitleXPath `XPath.following` XPaths.queryCellTitleXPath `XPath.following` "table"
--  thXPath = XPath.thWithText heading
--  tdXPath = "td" ++ predicate
--
--expectMdQueryColumnToEq :: String -> String -> SlamFeature Unit
--expectMdQueryColumnToEq heading = expectMdQueryColumn heading <<< XPath.withText
--
--expectMdQueryColumnToNotEq :: String -> String -> SlamFeature Unit
--expectMdQueryColumnToNotEq heading = expectMdQueryColumn heading <<< XPath.withoutText
--
--expectMdQueryColumnToBeGT :: String -> String -> SlamFeature Unit
--expectMdQueryColumnToBeGT heading = expectMdQueryColumn heading <<< XPath.withTextGreaterThan
--
--expectMdQueryColumnToBeLT :: String -> String -> SlamFeature Unit
--expectMdQueryColumnToBeLT heading = expectMdQueryColumn heading <<< XPath.withTextLessThan
--
--expectMdQueryColumnToEqOneOf :: String -> Array String -> SlamFeature Unit
--expectMdQueryColumnToEqOneOf heading = expectMdQueryColumn heading <<< eqAnyOfThese
--  where
--  joinOr = String.joinWith " or "
--  anyOfThesePredicates = predicate <<< joinOr
--  eqAnyOfThese = anyOfThesePredicates <<< map withText

--
expectMdFinishedMessage :: SlamFeature Unit
expectMdFinishedMessage = expectPresented XPaths.mdFinishedMessage
-- where
-- xPath = "//*[text()='Markdown']/following::*[contains(text(), 'Finished')]"

expectToBePresentedWithFormWithAllInputTypes :: SlamFeature Unit
expectToBePresentedWithFormWithAllInputTypes = do
  expectPresentedWithProperties
    [Properties.value (Just "")]
    $ XPath.anywhere $ XPath.textInput `XPath.withLabelWithExactText` "discipline"

  expectPresentedWithProperties
    [Properties.value (Just "Bobsleigh")]
    $ XPath.anywhere $ XPath.textInput `XPath.withLabelWithExactText` "sport"

--  expectPresented "age" "number" ""
--  expectPresented "year" "number" "2002"
--
--  expectPresented "startDate" "text" ""
--  expectPresented "finishDate" "text" "2002-06-06"
--
--  expectPresented "startTime" "text" ""
--  expectPresented "finishTime" "text" "20:39"

--  expectDropdownWithLabelOptionsAndValue "event" ["1000m", "1500m", "3000m"] "1500m"
--
--  expectLabel "gender"
--  expectInputWithLabelTypeAndSlamFeatureed "X" "checkbox" false
--  expectInputWithLabelTypeAndSlamFeatureed "W" "checkbox" false
--  expectInputWithLabelTypeAndSlamFeatureed "M" "checkbox" false
--
--  expectLabel "color"
--  expectInputWithLabelTypeAndSlamFeatureed "Red" "checkbox" true
--  expectInputWithLabelTypeAndSlamFeatureed "Green" "checkbox" false
--  expectInputWithLabelTypeAndSlamFeatureed "Blue" "checkbox" true
--
--  expectLabel "type"
--  expectInputWithLabelTypeAndSlamFeatureed "Gold" "radio" true
--  expectInputWithLabelTypeAndSlamFeatureed "Silver" "radio" false
--  expectInputWithLabelTypeAndSlamFeatureed "Bronze" "radio" false
--
--expectToBePresentedWithFormWithEvaluatedContent :: SlamFeature Unit
--expectToBePresentedWithFormWithEvaluatedContent = do
--  expectInputWithLabelTypeAndValue "discipline" "text" "Figure skating"
--
--  expectInputWithLabelTypeAndValue "year" "text" "1924"
--
--  expectDropdownWithLabelOptionsAndValue "country"
--    [ "LAT"
--    , "CZE"
--    , "UKR"
--    , "SLO"
--    , "RUS"
--    , "SVK"
--    , "KAZ"
--    , "AUS"
--    , "LUX"
--    , "UZB"
--    , "EUN"
--    , "DEN"
--    , "CHN"
--    , "ROU"
--    , "GDR"
--    , "PRK"
--    , "CRO"
--    , "URS"
--    , "BLR"
--    , "BUL"
--    , "POL"
--    , "EUA"
--    , "KOR"
--    , "NED"
--    , "ITA"
--    , "FRG"
--    , "EST"
--    , "SWE"
--    , "GBR"
--    , "TCH"
--    , "BEL"
--    , "FIN"
--    , "USA"
--    , "YUG"
--    , "SUI"
--    , "LIE"
--    , "CAN"
--    , "JPN"
--    , "HUN"
--    , "GER"
--    , "NOR"
--    , "NZL"
--    , "FRA"
--    , "AUT"
--    , "ESP"
--    ] "AUT"
--
--  expectLabel "gender"
--  expectInputWithLabelTypeAndSlamFeatureed "X" "checkbox" false
--  expectInputWithLabelTypeAndSlamFeatureed "W" "checkbox" true
--  expectInputWithLabelTypeAndSlamFeatureed "M" "checkbox" false
--
--  expectLabel "type"
--  expectInputWithLabelTypeAndSlamFeatureed "Gold" "radio" false
--  expectInputWithLabelTypeAndSlamFeatureed "Silver" "radio" true
--  expectInputWithLabelTypeAndSlamFeatureed "Bronze" "radio" false
