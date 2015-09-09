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
import Data.String (split)
import Data.Maybe (isJust, isNothing, Maybe(..), maybe)
import Data.Maybe.Unsafe (fromJust)
import Data.Traversable (traverse)

import Selenium.Types (Element())
import Selenium.Monad (sequence, getAttribute, clearEl)
import Selenium.ActionSequence hiding (sequence)
import Selenium.Key (shiftKey)

import Test.Config
import Test.Platform
import Test.Selenium.Monad (Check())
import Test.Selenium.Log (sectionMsg, successMsg, errorMsg)
import Test.Selenium.Common (await, waitTime)
import Test.Selenium.Notebook.Getters (getPlayButton, waitTextField)
import Test.Selenium.Notebook.Contexts (Context(), deleteAllCells, withMarkdownCell, makeQueryCell)
import qualified Test.Selenium.Notebook.Common as C

import Utils (s2i)

-- We to use a Sequence instead of a String as sendKey "(" doesn't show up in the
-- markdown cell.
provideAndPlayMarkdown :: Sequence Unit -> Check Unit
provideAndPlayMarkdown markdownSequence = do
  playMarkdownButton <- getPlayButton
  sequence do
    markdownSequence
    leftClick playMarkdownButton

checkValue :: String -> String -> Element -> Check Unit
checkValue expected error element =
  await error $ (eq expected) <$> getAttribute element "value"

createTextField :: String -> Check Unit
createTextField name = withMarkdownCell do
  provideAndPlayMarkdown $ sendKeys $ name ++ " = _____"
  waitTextField name >>= checkValue "" "Text field wasn't empty"

createTextFieldWithPlaceholder :: String -> String -> Check Unit
createTextFieldWithPlaceholder name placeholder = withMarkdownCell do
  provideAndPlayMarkdown do
    sendKeys $ name ++ " = _____"
    keyDown shiftKey
    sendKeys "9"
    keyUp shiftKey
    sendKeys placeholder
    keyDown shiftKey
    sendKeys "0"
  waitTextField name >>= checkValue placeholder "Text field didn't contain placeholder"

test :: Check Unit
test = do
  sectionMsg "Markdown cell: Create text field"
  createTextField "sport"
  successMsg "Ok, text field created"

  sectionMsg "Markdown cell: Create text field with placeholder"
  createTextFieldWithPlaceholder "sport" "Bobsleigh"
  successMsg "Ok, text field with placeholder created"
