module Test.Selenium.Notebook.Markdown.Expectations where

import Prelude

import Test.Selenium.Expect (Expectation(), expect, toEq, toNotEq, toBeGreaterThan, toMatch)
import Data.String.Regex (regex, noFlags)
import Data.String as String
import Data.Int (toNumber)
import Selenium.Combinators (tryToFind)
import Selenium.Monad (byXPath, getText)
import Test.Selenium.Monad (Check())
import Test.Selenium.Finders (findByXPath)
import Test.XPath as XPath
import Test.Selenium.Notebook.Expectations (expectFinishedMessage)

import Data.Foldable (traverse_)

--expectMdQueryResultsToBeFilteredByDefaultFormValues :: Check Unit
--expectMdQueryResultsToBeFilteredByDefaultFormValues = do
--  expectMdQueryRows 10
--  expectMdQueryColumn "discipline" toEq "Figure skating"
--  expectMdQueryColumn "country" toEq "AUT"
--  expectMdQueryColumn "gender" toEq "W"
--  expectMdQueryColumn "year" toBeGreaterThan $ toNumber 1924
--  expectMdQueryColumn "type" toNotEq "Gold"
--
--expectMdQueryRows :: Int -> Check Unit
--expectMdQueryRows = void <<< findByXPath <<< XPath.index (XPath.anywhere xPath)
--  where
--  xPath = "*[text()='Markdown']/following::*[text()='Query']/following::tbody/tr"
--
--expectMdQueryResultsToBeFilteredByChangedFormValues :: Check Unit
--expectMdQueryResultsToBeFilteredByChangedFormValues = do
--  expectMdQueryRows 8
--  expectMdQueryColumn "discipline" toEq "Luge"
--  expectMdQueryColumn "year" toBeGreaterThan $ toNumber 1950
--  expectMdQueryColumn "country" toEq "GDR"
--  expectMdQueryColumn "gender" toMatch $ regex "^(M|X)$" noFlags
--  expectMdQueryColumn "type" toNotEq "Gold"
--
--expectMdQueryColumn :: String -> String -> Check Unit
--expectMdQueryColumn heading preducate =
--  expectPresented $ XPath.anywhere $ tdWithTh tableXPath thXPath tdXPath
--  where
--  tableXPath =
--    XPaths.mdCellTitleXPath `XPath.following` XPaths.queryCellTitleXPath `XPath.following` "table"
--  thXPath = XPath.thWithText heading
--  tdXPath = "td" ++ predicate
--
--expectMdQueryColumnToEq :: String -> String -> Check Unit
--expectMdQueryColumnToEq heading = expectMdQueryColumn heading <<< XPath.withText
--
--expectMdQueryColumnToNotEq :: String -> String -> Check Unit
--expectMdQueryColumnToNotEq heading = expectMdQueryColumn heading <<< XPath.withoutText
--
--expectMdQueryColumnToBeGT :: String -> String -> Check Unit
--expectMdQueryColumnToBeGT heading = expectMdQueryColumn heading <<< XPath.withTextGreaterThan
--
--expectMdQueryColumnToBeLT :: String -> String -> Check Unit
--expectMdQueryColumnToBeLT heading = expectMdQueryColumn heading <<< XPath.withTextLessThan
--
--expectMdQueryColumnToEqOneOf :: String -> Array String -> Check Unit
--expectMdQueryColumnToEqOneOf heading = expectMdQueryColumn heading <<< eqAnyOfThese
--  where
--  joinOr = String.joinWith " or "
--  anyOfThesePredicates = predicate <<< joinOr
--  eqAnyOfThese = anyOfThesePredicates <<< map withText

--
--expectMdFinishedMessage :: Check Unit
--expectMdFinishedMessage = expectFinishedMessage xPath
-- where
-- xPath = "//*[text()='Markdown']/following::*[contains(text(), 'Finished')]"
--
--expectToBePresentedWithFormWithAllInputTypes :: Check Unit
--expectToBePresentedWithFormWithAllInputTypes = do
--  expectInputWithLabelTypeAndValue "discipline" "text" ""
--  expectInputWithLabelTypeAndValue "sport" "text" "Bobsleigh"
--
--  expectInputWithLabelTypeAndValue "age" "number" ""
--  expectInputWithLabelTypeAndValue "year" "number" "2002"
--
--  expectInputWithLabelTypeAndValue "startDate" "text" ""
--  expectInputWithLabelTypeAndValue "finishDate" "text" "2002-06-06"
--
--  expectInputWithLabelTypeAndValue "startTime" "text" ""
--  expectInputWithLabelTypeAndValue "finishTime" "text" "20:39"
--
--  expectDropdownWithLabelOptionsAndValue "event" ["1000m", "1500m", "3000m"] "1500m"
--
--  expectLabel "gender"
--  expectInputWithLabelTypeAndChecked "X" "checkbox" false
--  expectInputWithLabelTypeAndChecked "W" "checkbox" false
--  expectInputWithLabelTypeAndChecked "M" "checkbox" false
--
--  expectLabel "color"
--  expectInputWithLabelTypeAndChecked "Red" "checkbox" true
--  expectInputWithLabelTypeAndChecked "Green" "checkbox" false
--  expectInputWithLabelTypeAndChecked "Blue" "checkbox" true
--
--  expectLabel "type"
--  expectInputWithLabelTypeAndChecked "Gold" "radio" true
--  expectInputWithLabelTypeAndChecked "Silver" "radio" false
--  expectInputWithLabelTypeAndChecked "Bronze" "radio" false
--
--expectToBePresentedWithFormWithEvaluatedContent :: Check Unit
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
--  expectInputWithLabelTypeAndChecked "X" "checkbox" false
--  expectInputWithLabelTypeAndChecked "W" "checkbox" true
--  expectInputWithLabelTypeAndChecked "M" "checkbox" false
--
--  expectLabel "type"
--  expectInputWithLabelTypeAndChecked "Gold" "radio" false
--  expectInputWithLabelTypeAndChecked "Silver" "radio" true
--  expectInputWithLabelTypeAndChecked "Bronze" "radio" false
