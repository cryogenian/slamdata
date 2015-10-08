module Test.Selenium.Interactions where

import Prelude

import Test.Selenium.Monad (Check(), findSingle, getModifierKey)
import Control.Alt ((<|>))
import Test.Selenium.ActionSequence (keys, sendEnter, selectAll)
import Test.Selenium.Finders (findElementIdByLabelText, findElementByLabelText)
import Test.Selenium.Locators (checkableLocator)
import Selenium.Types (Element())
import Selenium.Monad (sequence)
import Selenium.MouseButton (leftButton)
import Selenium.ActionSequence (mouseDown, mouseUp, leftClick)
import Control.Apply ((*>))

provideCheckboxValue :: String -> Boolean -> Check Unit
provideCheckboxValue labelText value = void find <|> (findOpposite >>= toggle)
  where
  find' :: Boolean -> Check Element
  find' checked = findElementIdByLabelText labelText >>= checkableLocator "checkbox" checked
                                                     >>= findSingle
  find = find' value
  findOpposite = find' $ not value
  toggle :: Element -> Check Unit
  toggle element = sequence $ leftClick element

checkBox :: String -> Check Unit
checkBox = flip provideCheckboxValue true

uncheckBox :: String -> Check Unit
uncheckBox = flip provideCheckboxValue false

pushRadioButton :: String -> Check Unit
pushRadioButton labelText = findElementByLabelText labelText >>= leftClick >>> sequence

provideFieldValue :: String -> String -> Check Unit
provideFieldValue labelText value = do
  field <- findElementByLabelText labelText
  sequence $ leftClick field *> keys value

selectFromDropdown :: String -> String -> Check Unit
selectFromDropdown labelText value = do
  dropdown <- findElementByLabelText labelText
  sequence $ leftClick dropdown *> keys value *> sendEnter

changeFieldValue :: String -> String -> Check Unit
changeFieldValue labelText value = do
  field <- findElementByLabelText labelText
  modifierKey <- getModifierKey
  sequence $ leftClick field *> selectAll modifierKey *> keys value
