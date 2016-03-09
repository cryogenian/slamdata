module Test.SlamData.Feature.Notebook.Markdown.Expectations where

import Prelude

--import Test.SlamData.Feature.Expect (Expectation(), expect, toEq, toNotEq, toBeGreaterThan, toMatch)
--import Data.String.Regex (regex, noFlags)
--import Data.String as String
--import Data.Int (toNumber)
--import Selenium.Combinators (tryToFind)
--import Selenium.Monad (byXPath, getText)
import Test.SlamData.Feature.Monad (SlamFeature())
import Test.Feature (expectPresented, expectNotPresented, expectPresentedWithProperties)
import XPath as XPath
import Test.SlamData.Feature.XPaths as XPaths
import Test.SlamData.Feature.Properties as Properties
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))

expectMdQueryResultsToBeFilteredByDefaultFormValues :: SlamFeature Unit
expectMdQueryResultsToBeFilteredByDefaultFormValues = do
  expectPresented $ XPath.index disciplineTdXPath 2
  expectNotPresented $ XPath.index disciplineTdXPath 3

  expectPresented $ XPath.index countryTdXPath 2
  expectNotPresented $ XPath.index countryTdXPath 3

  expectPresented $ XPath.index genderTdXPath 2
  expectNotPresented $ XPath.index genderTdXPath 3

  expectPresented $ XPath.index yearTdXPath 2
  expectNotPresented $ XPath.index yearTdXPath 3

  expectPresented $ XPath.index typeTdXPath 2
  expectNotPresented $ XPath.index typeTdXPath 3
  where
  disciplineTdXPath =
    XPath.tdWithThAndTextEq
      (XPath.anywhere XPaths.mdQueryTable)
      (XPath.thWithExactText "discipline")
      "Figure skating"
  countryTdXPath =
    XPath.tdWithThAndTextEq
      (XPath.anywhere XPaths.mdQueryTable)
      (XPath.thWithExactText "country")
      "AUT"
  genderTdXPath =
    XPath.tdWithThAndTextEq
      (XPath.anywhere XPaths.mdQueryTable)
      (XPath.thWithExactText "gender")
      "W"
  yearTdXPath =
    XPath.tdWithThAndTextGT
      (XPath.anywhere XPaths.mdQueryTable)
      (XPath.thWithExactText "year")
      "1924"
  typeTdXPath =
    XPath.tdWithThAndTextNotEq
      (XPath.anywhere XPaths.mdQueryTable)
      (XPath.thWithExactText "type")
      "Gold"

expectMdQueryResultsToBeFilteredByChangedFormValues :: SlamFeature Unit
expectMdQueryResultsToBeFilteredByChangedFormValues = do
  expectPresented $ XPath.index disciplineTdXPath 8
  expectNotPresented $ XPath.index disciplineTdXPath 9

  expectPresented $ XPath.index countryTdXPath 8
  expectNotPresented $ XPath.index countryTdXPath 9

  expectPresented $ XPath.index genderTdXPath 8
  expectNotPresented $ XPath.index genderTdXPath 9

  expectPresented $ XPath.index yearTdXPath 8
  expectNotPresented $ XPath.index yearTdXPath 9

  expectPresented $ XPath.index typeTdXPath 8
  expectNotPresented $ XPath.index typeTdXPath 9
  where
  disciplineTdXPath =
    XPath.tdWithThAndTextEq
      (XPath.anywhere XPaths.mdQueryTable)
      (XPath.thWithExactText "discipline")
      "Luge"
  countryTdXPath =
    XPath.tdWithThAndTextEq
      (XPath.anywhere XPaths.mdQueryTable)
      (XPath.thWithExactText "country")
      "GDR"
  genderTdXPath =
    XPath.tdWithThAndTextEqOneOf
      (XPath.anywhere XPaths.mdQueryTable)
      (XPath.thWithExactText "gender")
      ["M", "X"]
  yearTdXPath =
    XPath.tdWithThAndTextGT
      (XPath.anywhere XPaths.mdQueryTable)
      (XPath.thWithExactText "year")
      "1950"
  typeTdXPath =
    XPath.tdWithThAndTextNotEq
      (XPath.anywhere XPaths.mdQueryTable)
      (XPath.thWithExactText "type")
      "Gold"

expectToBePresentedWithMdField :: String -> String -> String -> SlamFeature Unit
expectToBePresentedWithMdField labelText inputType value =
  expectPresentedWithProperties [valueProperty] $ XPath.anywhere inputXPath
  where
  valueProperty = Tuple "value" $ Just value
  inputXPath = XPaths.mdInput labelText inputType

expectToBePresentedWithCheckableMdField :: String -> String -> Boolean -> SlamFeature Unit
expectToBePresentedWithCheckableMdField labelText inputType checked =
  expectPresentedWithProperties [checkedProperty] $ XPath.anywhere inputXPath
  where
  propertyValue = if checked then Just "true" else Nothing
  checkedProperty = Tuple "checked" propertyValue
  inputXPath = XPaths.mdInput labelText inputType

expectToBePresentedWithFormWithAllInputTypes :: SlamFeature Unit
expectToBePresentedWithFormWithAllInputTypes = do
  expectToBePresentedWithMdField "discipline" "text" ""

  expectToBePresentedWithMdField "sport" "text" "Bobsleigh"

  expectToBePresentedWithMdField "age" "number" ""
  expectToBePresentedWithMdField "year" "number" "2002"

  expectToBePresentedWithMdField "startDate" "text" ""
  expectToBePresentedWithMdField "finishDate" "text" "2002-06-06"

  expectToBePresentedWithMdField "startTime" "text" ""
  expectToBePresentedWithMdField "finishTime" "text" "20:39"

  expectPresented
    $ XPath.anywhere
    $ XPaths.mdCellTitle `XPath.following`
      ("label" `XPath.nodeWithExactText` "event")
  expectPresentedWithProperties
    [ Tuple "value" $ Just "1500m" ]
    $ XPath.anywhere
    $ XPaths.mdCellTitle `XPath.following`
      (XPath.selectWithOptionsWithExactTexts ["1000m", "1500m", "3000m"])

  expectPresented
    $ XPath.anywhere
    $ XPaths.mdCellTitle `XPath.following`
      ("label" `XPath.nodeWithExactText` "gender")
  expectToBePresentedWithCheckableMdField "X" "checkbox" false
  expectToBePresentedWithCheckableMdField "W" "checkbox" false
  expectToBePresentedWithCheckableMdField "M" "checkbox" false

  expectPresented
    $ XPath.anywhere
    $ XPaths.mdCellTitle `XPath.following`
      ("label" `XPath.nodeWithExactText` "color")
  expectToBePresentedWithCheckableMdField "Red" "checkbox" true
  expectToBePresentedWithCheckableMdField "Green" "checkbox" false
  expectToBePresentedWithCheckableMdField "Blue" "checkbox" true

  expectPresented
    $ XPath.anywhere
    $ XPaths.mdCellTitle `XPath.following`
      ("label" `XPath.nodeWithExactText` "type")
  expectToBePresentedWithCheckableMdField "Gold" "radio" true
  expectToBePresentedWithCheckableMdField "Silver" "radio" false
  expectToBePresentedWithCheckableMdField "Bronze" "radio" false

expectMdFinishedMessage :: SlamFeature Unit
expectMdFinishedMessage = expectPresented $ XPath.anywhere XPaths.mdFinishedMessage

expectToBePresentedWithFormWithEvaluatedContent :: SlamFeature Unit
expectToBePresentedWithFormWithEvaluatedContent = do
  expectToBePresentedWithMdField "discipline" "text" "Figure skating"

  expectToBePresentedWithMdField "year" "text" "1924"

  expectPresented
    $ XPath.anywhere
    $ XPaths.mdCellTitle `XPath.following`
      ("label" `XPath.nodeWithExactText` "country")
  expectPresentedWithProperties
    [ Tuple "value" $ Just "AUT" ]
    $ XPath.anywhere
    $ XPaths.mdCellTitle `XPath.following`
      (XPath.selectWithOptionsWithExactTexts
        [ "LAT"
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
        ]
      )

  expectPresented
    $ XPath.anywhere
    $ XPaths.mdCellTitle `XPath.following`
      ("label" `XPath.nodeWithExactText` "gender")
  expectToBePresentedWithCheckableMdField "X" "checkbox" false
  expectToBePresentedWithCheckableMdField "W" "checkbox" true
  expectToBePresentedWithCheckableMdField "M" "checkbox" false

  expectPresented
    $ XPath.anywhere
    $ XPaths.mdCellTitle `XPath.following`
      ("label" `XPath.nodeWithExactText` "type")
  expectToBePresentedWithCheckableMdField "Gold" "radio" false
  expectToBePresentedWithCheckableMdField "Silver" "radio" true
  expectToBePresentedWithCheckableMdField "Bronze" "radio" false
