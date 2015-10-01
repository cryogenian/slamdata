module Test.Selenium.Expectations where

import Prelude

import Selenium.Combinators (tryToFind)
import Selenium.Monad (getAttribute, childExact, byXPath, byId, tryRepeatedlyTo)
import Data.Maybe (Maybe(..))
import Test.Selenium.Monad (Check(), findSingle)
import Test.Selenium.Finders (findElementIdByLabelText)
import Test.Selenium.Expect (expect, toEq)
import Test.Selenium.Locators (checkableLocator)

import qualified Data.Traversable (traverse) as T

expectDropdownWithLabelOptionsAndValue :: String -> Array String -> String -> Check Unit
expectDropdownWithLabelOptionsAndValue expectedLabel expectedOptions expectedValue = do
  selectId <- findElementIdByLabelText expectedLabel
  select <- tryToFind $ byId selectId
  value <- getAttribute select "value"
  expect value toEq $ Just expectedValue
  void $ T.traverse (findOption select) expectedOptions
    where
    optionXPath text = "//option[text()=\"" ++ text ++ "\"]"
    findOption select text = byXPath (optionXPath text) >>= childExact select

expectInputWithLabelTypeAndValue :: String -> String -> String -> Check Unit
expectInputWithLabelTypeAndValue expectedLabel expectedInputType expectedValue = do
  inputId <- findElementIdByLabelText expectedLabel
  input <- tryToFind $ byId inputId
  value <- getAttribute input "value"
  inputType <- getAttribute input "type"
  expect value toEq $ Just expectedValue
  expect inputType toEq $ Just expectedInputType

expectLabel :: String -> Check Unit
expectLabel expected = void $ tryRepeatedlyTo $ byXPath labelXPath >>= findSingle
  where
  labelXPath = "//label[text()=\"" ++ expected ++ "\"]"

expectInputWithLabelTypeAndChecked :: String -> String -> Boolean -> Check Unit
expectInputWithLabelTypeAndChecked expectedLabel expectedType expectedChecked = do
  inputId <- findElementIdByLabelText expectedLabel
  void $ tryToFind $ checkableLocator expectedType expectedChecked inputId
