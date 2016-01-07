module Test.Selenium.Interactions where

import Prelude

import Control.Alt ((<|>))
import Control.Apply ((*>))
import Control.Bind ((=<<))
import Selenium.Monad (sequence, clickEl, later, tryRepeatedlyTo, isDisplayed, getInnerHtml)
import Selenium.Types (ControlKey(), Element())
import Test.Selenium.ActionSequence as SlamSequence
import Selenium.ActionSequence as Sequence
import Selenium.MouseButton (leftButton)
import Test.Selenium.Common (waitTime)
import Test.Selenium.Finders (findElementIdByLabelText, findByLabelText)
import Test.Selenium.Locators (checkableLocator)
import Test.Selenium.Monad (Check(), getModifierKey)
import Test.Selenium.Finders (findSingle)
import Test.Selenium.Log (errorMsg, warnMsg)

provideCheckboxValue :: String -> Boolean -> Check Unit
provideCheckboxValue labelText value =
  tryRepeatedlyTo $ void find <|> (findOpposite >>= toggle)
  where
  find' :: Boolean -> Check Element
  find' checked =
    findElementIdByLabelText labelText
      >>= checkableLocator "checkbox" checked
      >>= findSingle
  find = find' value
  findOpposite = find' $ not value
  toggle :: Element -> Check Unit
  toggle element = click element

checkBox :: String -> Check Unit
checkBox = flip provideCheckboxValue true

uncheckBox :: String -> Check Unit
uncheckBox = flip provideCheckboxValue false

pushRadioButton :: String -> Check Unit
pushRadioButton labelText = tryRepeatedlyTo $ findByLabelText labelText >>= click

provideFieldValue :: String -> String -> Check Unit
provideFieldValue labelText value = tryRepeatedlyTo do
  field <- findByLabelText labelText
  click field
  typeString value

selectFromDropdown :: String -> String -> Check Unit
selectFromDropdown labelText value = tryRepeatedlyTo $ do
  dropdown <- findByLabelText labelText
  click dropdown
  typeString value
  pressEnter

changeFieldValue :: String -> String -> Check Unit
changeFieldValue labelText value = tryRepeatedlyTo $ do
  field <- findByLabelText labelText
  modifierKey <- getModifierKey
  click field
  selectAll modifierKey
  typeString value

typeString :: String -> Check Unit
typeString string = tryRepeatedlyTo $ sequence $ SlamSequence.keys string

pressEnter :: Check Unit
pressEnter = tryRepeatedlyTo $ sequence $ SlamSequence.sendEnter

selectAll :: ControlKey -> Check Unit
selectAll modifierKey = tryRepeatedlyTo $ sequence $ SlamSequence.selectAll modifierKey

click :: Element -> Check Unit
click element = tryRepeatedlyTo do
  sequence $ Sequence.mouseDown leftButton element
  sequence $ Sequence.mouseUp leftButton element

hover :: Element -> Check Unit
hover = tryRepeatedlyTo <<< sequence <<< Sequence.hover

