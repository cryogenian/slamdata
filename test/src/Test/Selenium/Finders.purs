module Test.Selenium.Finders where

import Prelude

import Control.Apply ((<*))
import Control.Alt ((<|>))
import Control.Bind ((<=<), (=<<))
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Exception (EXCEPTION(), throw)
import Control.Monad.Error.Class (throwError)
import Data.List (List(), uncons, length, elemIndex, zip, filter, head)
import Data.Tuple (fst, snd)
import Data.Maybe (Maybe(..), maybe, maybe')
import Data.Either (Either(), either)
import Data.String (take)
import Data.Monoid (Monoid, mempty)
import Selenium (showLocator)
import Selenium.Monad (getAttribute, attempt, later, sequence, getText, byXPath, byId, tryRepeatedlyTo, findExact, findElements, loseElement, isDisplayed, childExact, getInnerHtml)
import Selenium.ActionSequence (hover)
import Selenium.Types (Element(), Locator())
import Test.Selenium.Common (attrFail)
import Test.Selenium.Monad (Check())
import Test.XPath as XPath
import Test.Utils (ifTrue, ifFalse, orIfItFails, passover)
import Data.Traversable (traverse)
import Data.Foldable (traverse_)

-- Errors
noElementWithPropertyError :: String -> (Maybe String) -> String -> String
noElementWithPropertyError name value =
  XPath.errorMessage message
  where
  stringValue = maybe "null" show value
  message =
    "Unable to find element with "
      ++ show name
      ++ " attribute or property of "
      ++ show stringValue

elementWithPropertyError :: String -> (Maybe String) -> String -> String
elementWithPropertyError name value =
  XPath.errorMessage message
  where
  stringValue = maybe "null" show value
  message =
    "Expected not to find element with "
      ++ show name
      ++ " attribute or property of "
      ++ show stringValue

-- Expectations
validateHidden :: (String -> String) -> String -> Maybe String -> Check Unit
validateHidden _ _ (Just "true") = pure unit
validateHidden error xPath _ = liftEff $ throw $ error xPath

expectHiddenAria :: (String -> Check (List Element)) -> String -> Check Unit
expectHiddenAria findAny xPath = XPath.thisOrItsParents expectSingleHiddenAria xPath
  where
  expectSingleHiddenAria = traverse_ validate <=< findAny
  validate = validateHidden ariaError xPath <=< attributeOrProperty
  ariaError = noElementWithPropertyError "aria-hidden" (Just "true")
  attributeOrProperty = flip getAttribute "aria-hidden"

expectHiddenHtml :: (String -> Check (List Element)) -> String -> Check Unit
expectHiddenHtml findAny xPath = XPath.thisOrItsParents expectSingleHiddenHtml xPath
  where
  expectSingleHiddenHtml = traverse_ validate <=< findAny
  validate = validateHidden htmlError xPath <=< attributeOrProperty
  htmlError = noElementWithPropertyError "hidden" (Just "true")
  attributeOrProperty = flip getAttribute "hidden"

expectHiddenVisual :: (String -> Check (List Element)) -> String -> Check Unit
expectHiddenVisual findAny xPath = traverse_ validate =<< findAny xPath
  where
  validate = ifTrue throwVisualError <=< isDisplayed
  throwVisualError = liftEff $ throw $ visualError xPath
  visualError = XPath.errorMessage "Expected to find no visually displayed elements"

expectHidden :: (String -> Check (List Element)) -> String -> Check Unit
expectHidden findAny xPath =
  expectHiddenVisual findAny xPath <* expectHiddenDom
  where
  expectHiddenDom = expectHiddenAria findAny xPath <|> expectHiddenHtml findAny xPath

expectHiddenByXPath :: String -> Check Unit
expectHiddenByXPath = expectHidden findAnyByXPath

expectHiddenByXPathAndProperty :: String -> Maybe String -> String-> Check Unit
expectHiddenByXPathAndProperty name value = expectHidden $ findAnyByXPathAndProperty name value

expectPresented :: (String -> Check (List Element)) -> String -> Check Unit
expectPresented findAny xPath =
  expectPresentedVisual <* expectPresentedAria <* expectPresentedHtml <* expectNode
  where
  verify = either (const $ pure unit) <<< const <<< throwExpectation
  throwExpectation = liftEff <<< throw
  expectNode = (liftEff <<< throwIfEmpty nodeError) =<< findAny xPath
  expectPresentedVisual = verify visualError =<< (attempt $ expectHiddenVisual findAny xPath)
  expectPresentedAria = verify ariaError =<< (attempt $ expectHiddenAria findAny xPath)
  expectPresentedHtml = verify htmlError =<< (attempt $ expectHiddenHtml findAny xPath)
  nodeError = XPath.errorMessage nodeMessage xPath
  visualError = XPath.errorMessage visualMessage xPath
  ariaError = XPath.errorMessage ariaMessage xPath
  htmlError = XPath.errorMessage htmlMessage xPath
  nodeMessage = "Expected to find at least one elements"
  visualMessage = "Expected to find only visually presented elements"
  ariaMessage = "Expected no true values for \"aria-hidden\" on elements or their parents found"
  htmlMessage = "Expected no true values for \"hidden\" on elements or their parents found"

expectPresentedByXPath :: String -> Check Unit
expectPresentedByXPath = expectPresented findAnyByXPath

expectPresentedByXPathAndProperty :: String -> Maybe String -> String-> Check Unit
expectPresentedByXPathAndProperty name value =
  expectPresented $ findAnyByXPathAndProperty name value

-- Locator dependent finders
findAtLeast :: Int -> String -> Check (List Element)
findAtLeast n xPath = do
  elements <- findAnyByXPath xPath
  if length elements >= n
    then pure $ elements
    else liftEff $ throw $ message
  where
  message = XPath.errorMessage ("Couldn't find at least " ++ show n ++ " elements") xPath

-- Basic XPath dependent finders
findByXPath :: String -> Check Element
findByXPath xPath = do
  elements <- findAnyByXPath xPath
  case uncons elements of
    Nothing ->
      liftEff $ throw $ XPath.errorMessage "Couldn't find an element" xPath
    Just o -> case length o.tail of
      0 -> pure $ o.head
      _ -> liftEff $ throw $ XPath.errorMessage "Found more than one element" xPath

findFirstByXPath :: String -> Check Element
findFirstByXPath = findExact <=< byXPath

findAnyByXPath :: String -> Check (List Element)
findAnyByXPath = findElements <=< byXPath

findAllByXPath :: String -> Check (List Element)
findAllByXPath = findAtLeast 1

-- Property and XPath dependent finders
findAnyByXPathAndProperty :: String -> (Maybe String) -> String -> Check (List Element)
findAnyByXPathAndProperty property expectedValue xPath =
  tryRepeatedlyTo do
    elements <- findAnyByXPath xPath
    values <- traverse (flip getAttribute property) elements
    let matches = map (eq expectedValue) values
    let elementMatchTuples = zip elements matches
    let matchingElements = map fst $ filter snd elementMatchTuples
    pure matchingElements

findAllByXPathAndProperty :: String -> (Maybe String) -> String -> Check (List Element)
findAllByXPathAndProperty property expectedValue xPath =
  passover (liftEff <<< throwIfEmpty emptyMessage)
    =<< findAnyByXPathAndProperty property expectedValue xPath
  where
  emptyMessage = noElementWithPropertyError property expectedValue xPath

findByXPathAndProperty :: String -> (Maybe String) -> String -> Check Element
findByXPathAndProperty property expectedValue xPath =
  headOrThrow =<< findAnyByXPathAndProperty property expectedValue xPath
  where
  headOrThrow = maybe' (const throw') pure <<< head
  throw' = liftEff $ throw $ noElementWithPropertyError property expectedValue xPath

findByXPathAndValue :: (Maybe String) -> String -> Check Element
findByXPathAndValue value xPath =
  findByXPathAndProperty "value" value xPath

throwIfEmpty :: forall a eff. String -> List a -> Eff (err :: EXCEPTION | eff) Unit
throwIfEmpty _ xs | length xs == 0 = pure unit
throwIfEmpty message _ = throw message
