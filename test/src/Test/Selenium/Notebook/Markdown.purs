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
import Data.String (joinWith)
import Data.String.Regex (regex, noFlags)
import Data.Traversable (traverse)
import Data.Foldable (traverse_)

import Selenium.Types (Element())
import Selenium.Monad (sequence, getAttribute, getText, findExact, childExact, byXPath, byId, byCss, tryRepeatedlyTo)
import Selenium.ActionSequence (Sequence(), sendKeys, mouseDown, mouseUp, leftClick)
import Selenium.Key (shiftKey)
import Selenium.MouseButton (leftButton)
import Selenium.Combinators (tryToFind)

import Test.Config
import Test.Selenium.ActionSequence (selectAll, keys)
import Test.Selenium.Expect (expectEq, expectMatch)
import Test.Selenium.Monad (Check(), getConfig, getModifierKey)
import Test.Selenium.Log (sectionMsg, successMsg, errorMsg)
import Test.Selenium.Notebook.Getters (findPlayButton)
import Test.Selenium.Notebook.Contexts (deleteAllCells, insertMdCell)

import Utils (s2i)

provideMd :: String -> Check Unit
provideMd md = focusMdField *> sequence (keys $ md ++ " ")

focusMdField :: Check Unit
focusMdField = do
  config <- getConfig
  -- ".ace-editor" can be considered an HTML tag name like "ace-editor". The "user" is asked
  -- to "focus the ace editor".
  visibleMdField <- tryToFind $ byCss ".ace_editor"
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
  element <- tryToFind $ byXPath "//*[contains(., 'Finished')]"
  message <- getText element
  expectMatch (regex "Finished: took ([0-9]*)ms." noFlags) message

findElementIdByLabelText text = tryToFind (byXPath labelXPath) >>= findElementIdByLabel
  where
  labelXPath = "//label[text()='" ++ text ++ "']"
  findElementIdByLabel labelElement = tryRepeatedlyTo $ getAttribute labelElement "for"

expectDropdownWithLabelOptionsAndValue :: String -> Array String -> String -> Check Unit
expectDropdownWithLabelOptionsAndValue expectedLabel expectedOptions expectedValue = do
  selectId <- findElementIdByLabelText expectedLabel
  select <- tryToFind $ byId selectId
  getAttribute select "value">>= expectEq expectedValue
  void $ traverse (findOption select) expectedOptions
    where
    optionXPath text = "//option[text()='" ++ text ++ "']"
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
expectLabel expected = void $ tryToFind $ byXPath labelXPath
  where
  labelXPath = "//label[text()='" ++ expected ++ "']"

expectInputWithLabelTypeAndChecked :: String -> String -> Boolean -> Check Unit
expectInputWithLabelTypeAndChecked expectedLabel expectedType expectedChecked = do
  id <- findElementIdByLabelText expectedLabel
  void $ tryToFind $ byCss (inputSelector id expectedChecked)
    where
    baseSelector id = "input#" ++ id ++ "[type='" ++ expectedType ++ "']"
    inputSelector id true = baseSelector id ++ ":checked"
    inputSelector id false = baseSelector id ++ ":not(:checked)"

provideMdForFormWithAllInputTypes = traverse provideMd [ "dicipline = __"
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
  expectInputWithLabelTypeAndValue "dicipline" "text" ""
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

test :: Check Unit
test = do
  sectionMsg "Markdown cell: Provide and play markdown" *> do
    deleteAllCells

    insertMdCell
    provideMdForFormWithAllInputTypes
    playMd

    expectToBePresentedWithFormWithAllInputTypes
    expectFinishedMessage

    successMsg "Ok, succesfully provided and played markdown."

    deleteAllCells

  sectionMsg "Markdown cell: Change and play markdown" *> do
    deleteAllCells

    insertMdCell
    provideMd "dicipline = __"
    playMd
    changeMd "sport = __ (Bobsleigh)"
    playMd

    expectInputWithLabelTypeAndValue "sport" "text" "Bobsleigh"

    successMsg "Ok, successfully changed and played markdown."

    deleteAllCells
