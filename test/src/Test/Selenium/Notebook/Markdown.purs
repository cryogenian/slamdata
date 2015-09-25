{-
Copyright 2015 SlamData, Inc.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
-}

module Test.Selenium.Notebook.Markdown (test) where

import Prelude

import Control.Apply ((*>))
import Control.Alt ((<|>))
import Control.Bind ((=<<))
import Data.Array ((..), (:))
import Data.String (joinWith)
import Data.String.Regex (regex, noFlags)
import Data.List (List(..), fromList)

import Selenium.Types (Element(), Locator())
import Selenium.Monad (sequence, getAttribute, getText, findExact, childExact, byXPath, byId, byCss, tryRepeatedlyTo, findElements, navigateTo)
import Selenium.ActionSequence (Sequence(), sendKeys, mouseDown, mouseUp, leftClick)
import Selenium.Key (shiftKey)
import Selenium.MouseButton (leftButton)
import Selenium.Combinators (tryToFind)

import Test.Config
import Test.Selenium.ActionSequence (selectAll, keys)
import Test.Selenium.Expect (expectEq, expectMatch)
import Test.Selenium.Monad (Check(), getConfig, getModifierKey, byAriaLabel, byText, byExactText, findSingle)
import Test.Selenium.Log (sectionMsg, successMsg, warnMsg)
import Test.Selenium.Notebook.Getters (findPlayButton)
import Test.Selenium.Notebook.Contexts (deleteAllCells, insertMdCell)

import Utils (s2i)

import qualified Data.Traversable (traverse, sequence) as T
import qualified Data.Foldable (sequence_) as F

provideMd :: String -> Check Unit
provideMd md = focusMdField *> sequence (keys $ md ++ " ")

focusMdField :: Check Unit
focusMdField = do
  config <- getConfig
  visibleMdField <- tryToFind $ byCss config.markdown.focusEditorCssSelector
  sequence do
    mouseDown leftButton visibleMdField
    mouseUp leftButton visibleMdField

changeMd :: String -> Check Unit
changeMd md = do
  focusMdField
  modifierKey <- getModifierKey
  sequence $ selectAll modifierKey *> keys md

playMd :: Check Unit
playMd = findPlayButton >>= sequence <<< leftClick

expectFinishedMessage :: Check Unit
expectFinishedMessage = do
  element <- tryToFind $ byText "Finished"
  message <- getText element
  expectMatch (regex "Finished: took ([0-9]*)ms." noFlags) message

findElementIdByLabelText text =
  tryRepeatedlyTo $ byExactText text >>= findSingle >>= findElementIdByLabel
    where
    findElementIdByLabel labelElement = tryRepeatedlyTo $ getAttribute labelElement "for"

expectDropdownWithLabelOptionsAndValue :: String -> Array String -> String -> Check Unit
expectDropdownWithLabelOptionsAndValue expectedLabel expectedOptions expectedValue = do
  selectId <- findElementIdByLabelText expectedLabel
  select <- tryToFind $ byId selectId
  getAttribute select "value" >>= expectEq expectedValue
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
  expectEq expectedValue value
  expectEq expectedInputType inputType

expectLabel :: String -> Check Unit
expectLabel expected = void $ tryRepeatedlyTo $ byXPath labelXPath >>= findSingle
  where
  labelXPath = "//label[text()=\"" ++ expected ++ "\"]"

expectInputWithLabelTypeAndChecked :: String -> String -> Boolean -> Check Unit
expectInputWithLabelTypeAndChecked expectedLabel expectedType expectedChecked = do
  id <- findElementIdByLabelText expectedLabel
  void $ tryToFind $ byCss (inputSelector id expectedChecked)
    where
    baseSelector id = "input#" ++ id ++ "[type=\"" ++ expectedType ++ "\"]"
    inputSelector id true = baseSelector id ++ ":checked"
    inputSelector id false = baseSelector id ++ ":not(:checked)"

provideMdForFormWithAllInputTypes =
    T.traverse provideMd [ "discipline = __"
                         , "sport = __ (Bobsleigh)"
                         , "age = #__"
                         , "year = #__ (2002)"
                         , "startDate = __ - __ - __"
                         , "finishDate = __ - __ - __ (2002-06-06)"
                         , "startTime = __ : __"
                         , "finishTime = __ : __ (20:39)"
                         , "event = {1000m, 1500m, 3000m} (1500m)"
                         , "gender = []M []W []X"
                         , "color = [x]Red []Green [x]Blue"
                         , "type = (x)Gold ()Silver ()Bronze"
                         ]

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

provideMdForFormWithEvaluatedContent =
    T.traverse provideMd [ "discipline = __ (!`SELECT discipline FROM \"/test-mount/testDb/olympics\" LIMIT 1`)"
                         , "year = #__ (!`SELECT year FROM \"/test-mount/testDb/olympics\" LIMIT 1`)"
                         , "city = {!`SELECT DISTINCT city FROM \"/test-mount/testDb/olympics\"`} (!`SELECT city FROM \"/test-mount/testDb/olympics\" LIMIT 1`)"
                         , "type = (!`SELECT DISTINCT type FROM \"/test-mount/testDb/olympics\" LIMIT 1`) !`SELECT DISTINCT type FROM \"/test-mount/testDb/olympics\" OFFSET 1`"
                         , "gender = [!`SELECT gender FROM \"/test-mount/testDb/olympics\" LIMIT 1`] !`SELECT DISTINCT gender FROM \"/test-mount/testDb/olympics\"`"
                         ]

expectToBePresentedWithFormWithEvaluatedContent = do
  expectInputWithLabelTypeAndValue "discipline" "text" "Figure skating"

  expectInputWithLabelTypeAndValue "year" "number" "1924"

  expectDropdownWithLabelOptionsAndValue "city" [ "Turin"
                                                , "Lake Placid"
                                                , "Salt Lake City"
                                                , "Nagano"
                                                , "Squaw Valley"
                                                , "Lillehammer"
                                                , "Albertville"
                                                , "Sarajevo"
                                                , "Grenoble"
                                                , "Sapporo"
                                                , "Innsbruck"
                                                , "Calgary"
                                                , "Cortina d'Ampezzo"
                                                , "Chamonix"
                                                , "Garmisch-Partenkirchen"
                                                , "St. Moritz"
                                                , "Oslo"
                                                ] "Chamonix"

  expectLabel "gender"
  expectInputWithLabelTypeAndChecked "X" "checkbox" false
  expectInputWithLabelTypeAndChecked "W" "checkbox" true
  expectInputWithLabelTypeAndChecked "M" "checkbox" false

  expectLabel "type"
  expectInputWithLabelTypeAndChecked "Gold" "radio" false
  expectInputWithLabelTypeAndChecked "Silver" "radio" true
  expectInputWithLabelTypeAndChecked "Bronze" "radio" false

test :: Check Unit
test = do
  sectionMsg "Markdown: Provide and play markdown" *> do
    deleteAllCells

    insertMdCell
    provideMdForFormWithAllInputTypes
    playMd

    expectToBePresentedWithFormWithAllInputTypes
    expectFinishedMessage
    successMsg "Ok, succesfully provided and played markdown."

    deleteAllCells

  sectionMsg "Markdown: Change and play markdown" *> do
    deleteAllCells

    insertMdCell
    provideMd "discipline = __"
    playMd
    changeMd "sport = __ (Bobsleigh)"
    playMd

    expectInputWithLabelTypeAndValue "sport" "text" "Bobsleigh"
    successMsg "Ok, successfully changed and played markdown."

    deleteAllCells

  sectionMsg "Markdown: Provide and play markdown with evaluated content" *> do
    deleteAllCells

    insertMdCell
    provideMdForFormWithEvaluatedContent
    playMd

    expectToBePresentedWithFormWithEvaluatedContent
    successMsg "Ok, successfully provided and played markdown with evaluated content"

    deleteAllCells
