module Test.Selenium.Notebook.Markdown.Expectations where

import Prelude

import Test.Selenium.Expect (Expectation(), expect, toEq, toNotEq, toBeGreaterThan, toMatch)
import Data.String.Regex (regex, noFlags)
import Data.Int (toNumber)
import Selenium.Combinators (tryToFind)
import Selenium.Monad (byXPath, getText)
import Test.Selenium.Monad (Check(), findAtLeast)
import Test.Selenium.Expectations (expectLabel, expectInputWithLabelTypeAndChecked, expectInputWithLabelTypeAndValue, expectDropdownWithLabelOptionsAndValue)
import Test.Selenium.Notebook.Markdown.Finders (findMdQueryColumnCellsTextByHeading)

import qualified Data.Foldable (traverse_) as F

expectMdQueryResultsToBeFilteredByDefaultFormValues :: Check Unit
expectMdQueryResultsToBeFilteredByDefaultFormValues = do
  expectMdQueryRows 10
  expectMdQueryColumn "discipline" toEq "Figure skating"
  expectMdQueryColumn "country" toEq "AUT"
  expectMdQueryColumn "gender" toEq "W"
  expectMdQueryColumn "year" toBeGreaterThan $ toNumber 1924
  expectMdQueryColumn "type" toNotEq "Gold"

expectMdQueryRows :: Int -> Check Unit
expectMdQueryRows i = void $ locator >>= findAtLeast i
  where
  locator = byXPath "//*[text()='Markdown']/following::*[text()='Query']/following::tbody/tr"

expectMdQueryResultsToBeFilteredByChangedFormValues :: Check Unit
expectMdQueryResultsToBeFilteredByChangedFormValues = do
  expectMdQueryRows 10
  expectMdQueryColumn "discipline" toEq "Luge"
  expectMdQueryColumn "year" toBeGreaterThan $ toNumber 1950
  expectMdQueryColumn "country" toEq "GDR"
  expectMdQueryColumn "gender" toMatch $ regex "^(M|X)$" noFlags
  expectMdQueryColumn "type" toNotEq "Gold"

expectMdQueryColumn :: forall a. (Show a) => String -> Expectation String a -> a -> Check Unit
expectMdQueryColumn heading expectation expected = do
  cellTexts <- findMdQueryColumnCellsTextByHeading heading
  F.traverse_ (\cellText -> expect cellText expectation expected) cellTexts

expectMdFinishedMessage :: Check Unit
expectMdFinishedMessage = do
  element <- tryToFind $ byXPath xPath
  message <- getText element
  expect message toMatch (regex "Finished: took ([0-9]*)ms." noFlags)
    where
    xPath = "//*[text()='Markdown']/following::*[contains(text(), 'Finished')]"

expectToBePresentedWithFormWithAllInputTypes = do
  expectInputWithLabelTypeAndValue "discipline" "text" ""
  expectInputWithLabelTypeAndValue "sport" "text" "Bobsleigh"

  expectInputWithLabelTypeAndValue "age" "number" ""
  expectInputWithLabelTypeAndValue "year" "number" "2002"

  expectInputWithLabelTypeAndValue "startDate" "text" ""
  expectInputWithLabelTypeAndValue "finishDate" "text" "2002-06-06"

  expectInputWithLabelTypeAndValue "startTime" "text" ""
  expectInputWithLabelTypeAndValue "finishTime" "text" "20:39"

  expectDropdownWithLabelOptionsAndValue "event" ["1000m", "1500m", "3000m"] "1500m"

  expectLabel "gender"
  expectInputWithLabelTypeAndChecked "X" "checkbox" false
  expectInputWithLabelTypeAndChecked "W" "checkbox" false
  expectInputWithLabelTypeAndChecked "M" "checkbox" false

  expectLabel "color"
  expectInputWithLabelTypeAndChecked "Red" "checkbox" true
  expectInputWithLabelTypeAndChecked "Green" "checkbox" false
  expectInputWithLabelTypeAndChecked "Blue" "checkbox" true

  expectLabel "type"
  expectInputWithLabelTypeAndChecked "Gold" "radio" true
  expectInputWithLabelTypeAndChecked "Silver" "radio" false
  expectInputWithLabelTypeAndChecked "Bronze" "radio" false

expectToBePresentedWithFormWithEvaluatedContent = do
  expectInputWithLabelTypeAndValue "discipline" "text" "Figure skating"

  expectInputWithLabelTypeAndValue "year" "text" "1924"

  expectDropdownWithLabelOptionsAndValue "country" [ "LAT"
                                                   , "CZE"
                                                   , "UKR"
                                                   , "SLO"
                                                   , "RUS"
                                                   , "SVK"
                                                   , "KAZ"
                                                   , "AUS"
                                                   , "LUX"
                                                   , "UZB"
                                                   , "EUN"
                                                   , "DEN"
                                                   , "CHN"
                                                   , "ROU"
                                                   , "GDR"
                                                   , "PRK"
                                                   , "CRO"
                                                   , "URS"
                                                   , "BLR"
                                                   , "BUL"
                                                   , "POL"
                                                   , "EUA"
                                                   , "KOR"
                                                   , "NED"
                                                   , "ITA"
                                                   , "FRG"
                                                   , "EST"
                                                   , "SWE"
                                                   , "GBR"
                                                   , "TCH"
                                                   , "BEL"
                                                   , "FIN"
                                                   , "USA"
                                                   , "YUG"
                                                   , "SUI"
                                                   , "LIE"
                                                   , "CAN"
                                                   , "JPN"
                                                   , "HUN"
                                                   , "GER"
                                                   , "NOR"
                                                   , "NZL"
                                                   , "FRA"
                                                   , "AUT"
                                                   , "ESP"
                                                   ] "AUT"

  expectLabel "gender"
  expectInputWithLabelTypeAndChecked "X" "checkbox" false
  expectInputWithLabelTypeAndChecked "W" "checkbox" true
  expectInputWithLabelTypeAndChecked "M" "checkbox" false

  expectLabel "type"
  expectInputWithLabelTypeAndChecked "Gold" "radio" false
  expectInputWithLabelTypeAndChecked "Silver" "radio" true
  expectInputWithLabelTypeAndChecked "Bronze" "radio" false
