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
import Control.Monad.Eff.Random (randomInt)
import Control.Monad.Eff.Class (liftEff)
import Data.List (length, List(..), fromList)
import qualified Data.String.Regex (Regex(), test, regex, noFlags) as R
import Data.Maybe (isJust, isNothing, Maybe(..), maybe)
import Data.Maybe.Unsafe (fromJust)
import Data.Traversable (traverse)

import Selenium.Types (Element(), ControlKey())
import Selenium.Monad (sequence, getText, getAttribute, clearEl)
import Selenium.ActionSequence hiding (sequence)
import Selenium.Key (shiftKey)
import Selenium.MouseButton (leftButton)

import Test.Config
import Test.Selenium.ActionSequence (selectAll, keys)
import Test.Selenium.Monad (Check(), getConfig, getModifierKey)
import Test.Selenium.Log (sectionMsg, successMsg, errorMsg)
import Test.Selenium.Common (await, waitTime, waitExistentCss)
import Test.Selenium.Notebook.Getters (getPlayButton, waitTextField, getStatus)
import Test.Selenium.Notebook.Contexts (Context(), deleteAllCells, insertMdCell)
import qualified Test.Selenium.Notebook.Common as C

import Utils (s2i)

-- We use a Sequence instead of a String as sendKey "(" doesn't show up in the
-- markdown cell.
provideMd :: Sequence Unit -> Check Unit
provideMd mdSequence = focusMdField *> sequence mdSequence

focusMdField :: Check Unit
focusMdField = do
  config <- getConfig
  let visibleMdFieldSelector = config.markdown.visibleMdFieldSelector
  visibleMdField <- waitExistentCss visibleMdFieldSelector "Error: Couldn't find markdown field."
  sequence do
    mouseDown leftButton visibleMdField
    mouseUp leftButton visibleMdField

mdForTextField :: String -> Sequence Unit
mdForTextField name = keys $ name ++ " = _____"

mdForTextFieldWithPlaceholder :: String -> String -> Sequence Unit
mdForTextFieldWithPlaceholder placeholder name =
  keys $ name ++ " = _____(" <> placeholder <> ")"


changeMd :: Sequence Unit -> Check Unit
changeMd mdSequence = do
  focusMdField
  modifierKey <- getModifierKey
  sequence $ selectAll modifierKey *> mdSequence

playMd :: Check Unit
playMd = getPlayButton >>= sequence <<< leftClick

expectElValue :: String -> String -> Element -> Check Unit
expectElValue error expected element =
  await error $ (eq expected) <$> getAttribute element "value"

expectElTextMatches :: String -> R.Regex -> Element -> Check Unit
expectElTextMatches error expected element =
  await error $ (R.test expected) <$> getText element

expectTextFieldValue :: String -> String -> String -> Check Unit
expectTextFieldValue error expected name =
  waitTextField name >>= expectElValue error expected

expectFinishedStatus :: String -> Check Unit
expectFinishedStatus error =
  getStatus >>= expectElTextMatches error (R.regex "Finished: took ([0-9]*)ms." R.noFlags)

test :: Check Unit
test = do
  config <- getConfig
  let fieldValue = config.markdown.fieldValue
  let fieldName = config.markdown.fieldName
  let altFieldValue = config.markdown.altFieldValue
  let altFieldName = config.markdown.altFieldName

  sectionMsg "Markdown cell: Provide and play markdown" *> do
    deleteAllCells

    insertMdCell
    provideMd $ mdForTextField fieldName
    playMd
    expectFinishedStatus "Error: Creating text field didn't finish."
    expectTextFieldValue "Error: Text field wasn't empty." "" fieldName
    deleteAllCells

    insertMdCell
    provideMd $ mdForTextFieldWithPlaceholder fieldValue fieldName
    playMd
    expectFinishedStatus "Error: Creating text field with placeholder didn't finish."
    expectTextFieldValue "Error: Text field didn't contain placeholder." fieldValue fieldName
    deleteAllCells

    successMsg "Ok, succesfully provided and played markdown."

  sectionMsg "Markdown cell: Change and play markdown" *> do
    deleteAllCells

    insertMdCell
    provideMd $ mdForTextField fieldName
    playMd
    changeMd $ mdForTextFieldWithPlaceholder altFieldValue altFieldName
    playMd
    expectTextFieldValue "Error: Playing changed markdown didn't work." altFieldValue altFieldName

    successMsg "Ok, successfully changed and played markdown."
    deleteAllCells
