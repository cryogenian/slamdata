module Test.Feature where
--  ( click
--  , hover
--  , check
--  , uncheck
--  , pushRadioButton
--  , provideFieldValue
--  , selectFromDropdown
--  , clickWithProperty
--  , hoverWithProperty
--  , checkWithProperty
--  , uncheckWithProperty
--  , pushRadioButtonWithProperty
--  , provideFieldValueWithProperty
--  , selectFromDropdownWithProperty
--  , expectPresented
--  , expectHidden
--  , expectPresentedWithProperty
--  , expectHiddenWithProperty
--  ) where

import Control.Alt ((<|>))
import Control.Apply ((<*))
import Control.Bind ((=<<), (<=<))
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (EXCEPTION(), throw, message)
import Control.Monad.Error.Class (throwError)
import Data.Either (Either(), either)
import Data.Foldable (foldMap, foldr, traverse_)
import Data.Array as Array
import Data.List (List(..), (..), index, uncons, length, singleton, elemIndex, zip, filter, head, intersectBy)
import Data.Maybe (Maybe(..), maybe, maybe', isJust)
import Data.Monoid (Monoid, mempty)
import Data.String (joinWith, take)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), fst, snd, uncurry)
import Prelude
import Selenium (showLocator)
import Selenium.ActionSequence as Sequence
import Selenium.Monad (getAttribute, clickEl, attempt, later, sequence, getText, byXPath, byId, tryRepeatedlyTo, findExact, findElements, loseElement, isDisplayed, childExact, getInnerHtml)
import Selenium.MouseButton (leftButton)
import Selenium.Types (Element(), Locator(), ControlKey())
import Test.Feature.ActionSequence as FeatureSequence
import Test.Selenium.Log (sectionMsg, warnMsg, errorMsg)
import Test.Selenium.Monad (Check(), getModifierKey)
import Test.Utils (ifTrue, ifFalse, orIfItFails, passover, throwIfEmpty)
import Test.XPath as XPath

type Property = Tuple String (Maybe String)
type Indexed a = Tuple Int a

-- Basic XPath dependent finders
find' :: String -> Check Element
find' xPath =
  (maybe throwNoElementsError validate <<< uncons) =<< findAll' xPath
  where
  throwNoElementsError =
    liftEff $ throw $ XPath.errorMessage "Couldn't find' an element" xPath
  throwMoreThanOneElementError =
    liftEff $ throw $ XPath.errorMessage "Found more than one element" xPath
  validate o | length o.tail == 0 = pure o.head
  validate _ = throwMoreThanOneElementError

findAll' :: String -> Check (List Element)
findAll' = findElements <=< byXPath

find :: String -> Check Element
find xPath = find' xPath <* expectPresented xPath

findAll :: String -> Check (List Element)
findAll xPath = findAll' xPath <* expectPresented xPath

-- Property and XPath dependent finders
findAllWithProperties' :: Array Property -> String -> Check (List Element)
findAllWithProperties' properties = elementsWithProperties properties <=< findAll

findWithProperties' :: Array Property -> String -> Check Element
findWithProperties' properties xPath =
  headOrThrow =<< findAllWithProperties' properties xPath
  where
  headOrThrow = maybe' (const throw') pure <<< head
  throw' = liftEff $ throw $ noElementWithPropertiesError properties xPath

findAllWithProperties :: Array Property -> String -> Check (List Element)
findAllWithProperties properties xPath =
  findAllWithProperties' properties xPath <* expectPresentedWithProperties properties xPath

findWithProperties :: Array Property -> String -> Check Element
findWithProperties properties xPath =
  findWithProperties' properties xPath <* expectPresentedWithProperties properties xPath

-- Errors
printPropertyValue :: Maybe String -> String
printPropertyValue = maybe "null" show

printProperty :: String -> Maybe String -> String
printProperty name value = name ++ "=" ++ printPropertyValue value

printProperties :: Array Property -> String
printProperties = joinWith " " <<< map (uncurry printProperty)

noElementWithPropertiesError :: Array Property -> String -> String
noElementWithPropertiesError properties =
  XPath.errorMessage $ withPropertiesMessage properties "Unable to find' element with "

elementWithPropertiesError :: Array Property -> String -> String
elementWithPropertiesError properties =
  XPath.errorMessage $ withPropertiesMessage properties "Expected not to find' element with "

withPropertiesMessage :: Array Property -> String -> String
withPropertiesMessage xs s | Array.length xs == 0 = ""
withPropertiesMessage xs s = s ++ " with the attributes or properties " ++ printProperties xs

-- Expectations
expectHidden :: String -> Check Unit
expectHidden xPath = expectHiddenWithProperties [] xPath

expectPresented :: String -> Check Unit
expectPresented xPath = expectPresentedWithProperties [] xPath

validateHidden :: (String -> String) -> String -> Maybe String -> Check Unit
validateHidden _ _ (Just "true") = pure unit
validateHidden error xPath _ = liftEff $ throw $ error xPath

expectHiddenAria :: Array Property -> String -> Check Unit
expectHiddenAria properties xPath = XPath.thisOrItsParents expectSingleHiddenAria xPath
  where
  expectSingleHiddenAria = traverse_ validate <=< findAllWithProperties' properties
  validate = validateHidden ariaError xPath <=< attributeOrProperty
  ariaError = noElementWithPropertiesError $ [(Tuple "aria-hidden" (Just "true"))] ++ properties
  attributeOrProperty = flip getAttribute "aria-hidden"

expectHiddenHtml :: Array Property -> String -> Check Unit
expectHiddenHtml properties xPath = XPath.thisOrItsParents expectSingleHiddenHtml xPath
  where
  expectSingleHiddenHtml = traverse_ validate <=< findAllWithProperties' properties
  validate = validateHidden htmlError xPath <=< attributeOrProperty
  htmlError = noElementWithPropertiesError $ [(Tuple "hidden" (Just "true"))] ++ properties
  attributeOrProperty = flip getAttribute "hidden"

expectHiddenVisual :: Array Property -> String -> Check Unit
expectHiddenVisual properties xPath = traverse_ validate =<< findAllWithProperties' properties xPath
  where
  validate = ifTrue throwVisualError <=< isDisplayed
  throwVisualError = liftEff $ throw $ XPath.errorMessage message xPath
  message = withPropertiesMessage properties "Expected to find' no visually displayed elements"

expectHiddenWithProperties :: Array Property -> String -> Check Unit
expectHiddenWithProperties properties xPath =
  expectHiddenVisual properties xPath <* expectHiddenDom
  where
  expectHiddenDom = expectHiddenAria properties xPath <|> expectHiddenHtml properties xPath

expectPresentedWithProperties :: Array Property -> String -> Check Unit
expectPresentedWithProperties properties xPath =
  expectPresentedVisual <* expectPresentedAria <* expectPresentedHtml <* expectNode
  where
  verify = either (const $ pure unit) <<< const <<< throwExpectation
  throwExpectation = liftEff <<< throw
  expectNode = (liftEff <<< throwIfEmpty nodeError) =<< findAllWithProperties' properties xPath
  expectPresentedVisual = verify visualError =<< (attempt $ expectHiddenVisual properties xPath)
  expectPresentedAria = verify ariaError =<< (attempt $ expectHiddenAria properties xPath)
  expectPresentedHtml = verify htmlError =<< (attempt $ expectHiddenHtml properties xPath)
  nodeError = XPath.errorMessage (withPropertiesMessage properties nodeMessage) xPath
  visualError = XPath.errorMessage (withPropertiesMessage properties visualMessage) xPath
  ariaError = XPath.errorMessage (withPropertiesMessage properties ariaMessage) xPath
  htmlError = XPath.errorMessage (withPropertiesMessage properties htmlMessage) xPath
  nodeMessage = "Expected to find' at least one elements"
  visualMessage = "Expected to find' only visually presented elements"
  ariaMessage = "Expected no true values for \"aria-hidden\" on elements or their parents found"
  htmlMessage = "Expected no true values for \"hidden\" on elements or their parents found"

-- Interaction utilities
checkedProperty :: Maybe String -> Property
checkedProperty checked = Tuple "checked" checked

propertiesAndChecked :: Maybe String -> Array Property -> Array Property
propertiesAndChecked checked = append [checkedProperty checked]

-- XPath dependent interactions
check' :: (Maybe String) -> String -> Check Unit
check' checked xPath =
  tryRepeatedlyTo
    $ (traverse_ clickElement) =<< findAllWithProperties [checkedProperty checked] xPath

check :: String -> Check Unit
check = check' Nothing

uncheck :: String -> Check Unit
uncheck = check' (Just "true")

pushRadioButton :: String -> Check Unit
pushRadioButton = check' Nothing

provideFieldValue :: String -> String -> Check Unit
provideFieldValue xPath value =
  tryRepeatedlyTo $ provideFieldValueElement value =<< find xPath

selectFromDropdown :: String -> String -> Check Unit
selectFromDropdown xPath text =
  tryRepeatedlyTo $ selectFromDropdownElement text =<< find xPath

click :: String -> Check Unit
click = tryRepeatedlyTo <<< clickElement <=< find

clickAll :: String -> Check Unit
clickAll = tryRepeatedlyTo <<< (traverse_ clickElement) <=< findAll

hover :: String -> Check Unit
hover = tryRepeatedlyTo <<< hoverElement <=< find

-- XPath and property dependent interactions
checkWithProperties' :: (Maybe String) -> Array Property -> String -> Check Unit
checkWithProperties' checked properties xPath =
  tryRepeatedlyTo $ (traverse_ clickElement) =<< findAllWithProperties properties' xPath
  where
  properties' = propertiesAndChecked checked properties

checkWithProperties :: Array Property -> String -> Check Unit
checkWithProperties = checkWithProperties' Nothing

uncheckWithProperties :: Array Property -> String -> Check Unit
uncheckWithProperties = checkWithProperties' (Just "true")

pushRadioButtonWithProperties :: Array Property -> String -> Check Unit
pushRadioButtonWithProperties = checkWithProperties' Nothing

provideFieldValueWithProperties :: Array Property -> String -> String -> Check Unit
provideFieldValueWithProperties properties xPath value =
  provideFieldValueElement value =<< findWithProperties properties xPath

selectFromDropdownWithProperties :: Array Property -> String -> String -> Check Unit
selectFromDropdownWithProperties properties xPath text =
  selectFromDropdownElement text =<< findWithProperties properties xPath

clickWithProperties :: Array Property -> String -> Check Unit
clickWithProperties properties =
  tryRepeatedlyTo <<< clickElement <=< findWithProperties properties

clickAllWithProperties :: Array Property -> String -> Check Unit
clickAllWithProperties properties =
  tryRepeatedlyTo <<< (traverse_ clickElement) <=< findAllWithProperties properties

hoverWithProperties :: Array Property -> String -> Check Unit
hoverWithProperties properties =
  tryRepeatedlyTo <<< hoverElement <=< findWithProperties properties

-- Independent interactions
typeString :: String -> Check Unit
typeString string = tryRepeatedlyTo $ sequence $ FeatureSequence.keys string

pressEnter :: Check Unit
pressEnter = tryRepeatedlyTo $ sequence $ FeatureSequence.sendEnter

selectAll :: Check Unit
selectAll = tryRepeatedlyTo $ (sequence <<< FeatureSequence.selectAll) =<< getModifierKey

-- Element dependent interactions
clickElement :: Element -> Check Unit
clickElement element =
  tryRepeatedlyTo
    $ (sequence $ Sequence.mouseUp leftButton element)
    <* (sequence $ Sequence.mouseDown leftButton element)

clickAllElements :: List Element -> Check Unit
clickAllElements = tryRepeatedlyTo <<< (traverse_ clickElement)

hoverElement :: Element -> Check Unit
hoverElement = tryRepeatedlyTo <<< sequence <<< Sequence.hover

provideFieldValueElement :: String -> Element -> Check Unit
provideFieldValueElement value element =
  tryRepeatedlyTo $ typeString value <* selectAll <* clickElement element

selectFromDropdownElement :: String -> Element -> Check Unit
selectFromDropdownElement text element =
  tryRepeatedlyTo $ pressEnter <* typeString text <* clickElement element

-- Element filters
elementsWithProperties :: Array Property -> List Element -> Check (List Element)
elementsWithProperties properties | Array.length properties == 0 = pure <<< id
elementsWithProperties properties =
  pure <<< map snd <=< flip indexedElementsWithProperties properties <<< zipWithIndices
  where
  values name = traverse (flip getAttribute name)
  mapEq x = map (eq x)
  tailHeadIntersectionBy f o = foldr (intersectBy f) o.head o.tail
  intersectArrayOfListsBy f = maybe Nil (tailHeadIntersectionBy f) <<< Array.uncons
  filterByBooleans xs = map fst <<< filter snd <<< zip xs
  toIndexes xs = 0 .. (length xs - 1)
  zipWithIndices xs = zip (toIndexes xs) xs
  fstEq x y = fst x == fst y
  indexedElementsWithProperty elements name expectedValue =
    pure <<< filterByBooleans elements <<< mapEq expectedValue =<< values name (map snd elements)
  nonIntersectedIndexedElementsWithProperties elements =
    traverse (uncurry (indexedElementsWithProperty elements))
  indexedElementsWithProperties elements =
    pure <<< intersectArrayOfListsBy fstEq <=< nonIntersectedIndexedElementsWithProperties elements
