module Test.Selenium.Finders where

import Prelude

import Control.Apply ((*>))
import Control.Bind ((<=<), (=<<))
import Control.Monad.Eff.Exception (error)
import Control.Monad.Error.Class (throwError)
import Data.List (List(), uncons, length, elemIndex, zip, filter, head)
import Data.Tuple (fst, snd)
import Data.Maybe (Maybe(..), maybe, maybe')
import Data.String (take)
import Selenium (showLocator)
import Selenium.Monad (getAttribute, later, sequence, getText, byXPath, byId, tryRepeatedlyTo, findExact, findElements, loseElement, isDisplayed, childExact, getInnerHtml)
import Selenium.ActionSequence (hover)
import Selenium.Types (Element(), Locator())
import Test.Selenium.Common (attrFail)
import Test.Selenium.Monad (Check())
import Test.XPath as XPath
import Test.Utils (ifTrue, ifFalse, orIfItFails, passover)

import Data.Traversable (traverse)
import Data.Foldable (traverse_)

-- Expectations
expectPresented :: Locator -> Element -> Check Unit
expectPresented locator element =
  expectPresentedVisual *> expectPresentedAria
  where
  expectPresentedVisual = isDisplayed element >>= ifFalse (throwLocatorError visualError locator)
  expectPresentedAria = getAttribute element "aria-hidden" >>= validateAriaHiddenAttribute
  validateAriaHiddenAttribute (Just "true") = throwNoElementWithAttributeOrPropertyError locator
  validateAriaHiddenAttribute _ = pure unit
  visualError = "Expected to find a visible element"

expectHidden :: Locator -> Element -> Check Unit
expectHidden locator element =
  expectHiddenVisual *> expectHiddenAria
  where
  expectHiddenVisual = isDisplayed element >>= ifTrue (throwLocatorError visualError locator)
  expectHiddenAria = thisOrItsParents expectHiddenAria' element
  expectHiddenAria' element' = getAttribute element' "aria-hidden" >>= validateAriaHiddenAttribute
  validateAriaHiddenAttribute (Just "false") = throwLocatorError ariaError locator
  validateAriaHiddenAttribute Nothing = throwLocatorError ariaError locator
  validateAriaHiddenAttribute _ = pure unit
  ariaError = "Expected attribute aria-hidden to be \"true\" for the element (or one of its parents) located by"
  visualError = "Expected not to be able to mouse over the element located by"

-- Locator dependent finders
findSingle :: Locator -> Check Element
findSingle locator = do
  elements <- findElements locator
  case uncons elements of
    Nothing ->
      throwLocatorError "Couldn't find an element" locator
    Just o -> case length o.tail of
      0 -> pure $ o.head
      _ -> throwLocatorError "Found more than one element" locator

findAtLeast :: Int -> Locator -> Check (List Element)
findAtLeast n locator = do
  elements <- findElements locator
  if length elements >= n
    then pure $ elements
    else throwLocatorError ("Couldn't find at least " ++ show n ++ " elements") locator

-- XPath dependent finders which don't check presentation
findByXPath' :: String -> Check Element
findByXPath' = findSingle <=< byXPath

findFirstByXPath' :: String -> Check Element
findFirstByXPath' =
  findExact <=< byXPath

findAnyByXPath' :: String -> Check (List Element)
findAnyByXPath' =
  findElements <=< byXPath

findAllByXPath' :: String -> Check (List Element)
findAllByXPath' =
  findAtLeast 1 <=< byXPath

-- Basic XPath dependent finders
loseByXPath :: String -> Check Unit
loseByXPath =
  (loseDOM `orIfItFails` losePresented) <=< byXPath
  where
  loseDOM = loseElement
  losePresented locator = traverse_ (expectHidden locator) =<< findElements locator

findByXPath :: String -> Check Element
findByXPath xPath = do
  locator <- byXPath xPath
  element <- findByXPath' xPath
  expectPresented locator element
  pure element

findFirstByXPath :: String -> Check Element
findFirstByXPath xPath = do
  locator <- byXPath xPath
  element <- findFirstByXPath' xPath
  expectPresented locator element
  pure element

findAllByXPath :: String -> Check (List Element)
findAllByXPath xPath = do
  locator <- byXPath xPath
  elements <- findAllByXPath' xPath
  traverse_ (expectPresented locator) elements
  pure elements

findAnyByXPath :: String -> Check (List Element)
findAnyByXPath xPath = do
  locator <- byXPath xPath
  elements <- findAnyByXPath' xPath
  traverse_ (expectPresented locator) elements
  pure elements

-- Property and XPath dependent finders
findAnyByXPathAndProperty :: String -> String -> (Maybe String) -> Check (List Element)
findAnyByXPathAndProperty xPath property expectedValue = tryRepeatedlyTo do
  xPath
  values <- traverse (flip getAttribute property) elements
  let matches = map (eq expectedValue) values
  let elementMatchTuples = zip elements matches
  let matchingElements = map fst $ filter snd elementMatchTuples
  pure matchingElements

findAllByXPathAndAttributeOrProperty :: String -> String -> (Maybe String) -> Check (List Element)
findAllByXPathAndAttributeOrProperty xPath property expectedValue =
  passover throwIfEmpty =<< findAnyByXPathAndProperty xPath property expectedValue
  where
  throwIfEmpty xs | length xs > 0 = pure unit
  throwIfEmpty _ = throwNoElementWithAttributeOrPropertyError xPath property shownExpectedValue

findByXPathAndAttributeOrProperty :: String -> String -> (Maybe String) -> Check Element
findByXPathAndAttributeOrProperty xPath property expectedValue =
  headOrThrow =<< findAnyByXPathAndProperty xPath property expectedValue
  where
  headOrThrow = maybe' (const throw) pure <<< head
  throw = throwNoElementWithPropertyError property expectedValue xPath

findByXPathAndValue :: String -> (Maybe String) -> Check Element
findByXPathAndValue xPath value =
  findByXPathAndAttributeOrProperty (XPath.anywhere $ xPath) "value" value

