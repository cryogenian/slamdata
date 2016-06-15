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

module Test.Feature.ActionSequence
  ( selectAll
  , copy
  , paste
  , undo
  , sendDelete
  , sendEnter
  , shifted
  , keys
  , sendBackspaces
  , close
  , focusAddressBar
  ) where

import Prelude

import Data.Foldable (traverse_)
import Data.String (split)
import Data.Array (replicate)
import Selenium.ActionSequence (Sequence, sendKeys, keyDown, keyUp)
import Selenium.Types (ControlKey)
import Selenium.Key (shiftKey)

selectAll :: ControlKey -> Sequence Unit
selectAll modifierKey = sendKeyCombo [modifierKey] "a"

copy :: ControlKey -> Sequence Unit
copy modifierKey = sendKeyCombo [modifierKey] "c"

focusAddressBar :: ControlKey -> Sequence Unit
focusAddressBar modifierKey = sendKeyCombo [modifierKey] "l"

paste :: ControlKey -> Sequence Unit
paste modifierKey = sendKeyCombo [modifierKey] "v"

undo :: ControlKey -> Sequence Unit
undo modifierKey = sendKeyCombo [modifierKey] "z"

close :: ControlKey -> Sequence Unit
close modifierKey = sendKeyCombo [modifierKey] "w"

shifted :: String -> Sequence Unit
shifted str = do
  keyDown shiftKey
  sendKeys str
  keyUp shiftKey

sendDelete :: Sequence Unit
sendDelete = sendKeys "\xE017"

sendBackspaces :: Int -> Sequence Unit
sendBackspaces n = traverse_ sendKeys $ replicate n "\xE003"

sendEnter :: Sequence Unit
sendEnter =
  sendKeys "\xE007"
--  sendKeys "\xE006"
--  sendKeys $ fromChar $ fromCharCode 13

sendKeyCombo :: Array ControlKey -> String -> Sequence Unit
sendKeyCombo ctrlKeys str = do
  traverse_ keyDown ctrlKeys
  sendKeys str
  traverse_ keyUp ctrlKeys

-- Send keys one by one and replace if they can't be processed
-- by selenium driver
keys :: String -> Sequence Unit
keys str = traverse_ sendKey $ split "" str
  where
  sendKey :: String -> Sequence Unit
  sendKey "!" = shifted "1"
  sendKey "(" = shifted "9"
  sendKey ")" = shifted "0"
  sendKey "#" = shifted "3"
  sendKey "-" = sendKeys "\xE027"
  sendKey a = sendKeys a
