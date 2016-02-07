module Test.Selenium.Interactions where
  ( click
  , hover
  , check
  , uncheck
  , pushRadioButton
  , provideFieldValue
  , selectFromDropdown
  ) where

import Prelude

import Control.Alt ((<|>))
import Control.Apply ((*>))
import Control.Bind ((=<<))
import Selenium.ActionSequence as Sequence
import Selenium.Monad (sequence, clickEl, later, tryRepeatedlyTo, isDisplayed, getInnerHtml)
import Selenium.MouseButton (leftButton)
import Selenium.Types (ControlKey(), Element())
import Test.Selenium.ActionSequence as SlamSequence
import Test.Selenium.Common (waitTime)
import Test.Selenium.Finders (findByXPath, findAllByXPath)
import Test.Selenium.Locators (checkableLocator)
import Test.Selenium.Log (errorMsg, warnMsg)
import Test.Selenium.Monad (Check(), getModifierKey)
import Test.Utils (ifTrue, ifFalse, orIfItFails, passover)

-- XPath dependent interactions
check' :: (Maybe String) -> String -> Check Unit
check' checked xPath = tryRepeatedlyTo $ clickElement =<< findAnyByXPathAndProperty "checked" xPath checked

check :: String -> Check Unit
check xPath = check' Nothing

uncheck :: String -> Check Unit
uncheck = check' (Just "true")

pushRadioButton :: String -> Check Unit
pushRadioButton xPath = check' Nothing

provideFieldValue :: String -> String -> Check Unit
provideFieldValue xPath value =
  tryRepeatedlyTo $ (click =<< findByXPath xPath) *> selectAll *> typeString value

selectFromDropdown :: String -> String -> Check Unit
selectFromDropdown xPath text =
  tryRepeatedlyTo $ click xPath *> typeString text *> pressEnter

click :: String -> Check Unit
click = tryRepeatedlyTo <<< clickElement <=< findByXPath

clickAll :: String -> Check Unit
clickAll = tryRepeatedlyTo <<< traverse clickElement <=< findAllByXPath

hover :: String -> Check Unit
hover = tryRepeatedlyTo <<< clickElement <=< findByXPath

-- Independent interactions
typeString :: String -> Check Unit
typeString string = tryRepeatedlyTo $ sequence $ SlamSequence.keys string

pressEnter :: Check Unit
pressEnter = tryRepeatedlyTo $ sequence $ SlamSequence.sendEnter

selectAll :: Check Unit
selectAll = tryRepeatedlyTo $ sequence $ (SlamSequence.selectAll =<< getModifierKey)

-- Element dependent interactions
clickElement :: Element -> Check Unit
clickElement element =
  tryRepeatedlyTo
    $ sequence $ Sequence.mouseDown leftButton element
    *> sequence $ Sequence.mouseUp leftButton element

hoverElement :: Element -> Check Unit
hoverElement = tryRepeatedlyTo <<< sequence <<< Sequence.hover

