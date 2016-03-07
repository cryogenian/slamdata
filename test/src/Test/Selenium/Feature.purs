module Test.Selenium.Feature where
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
import Control.Apply ((*>))
import Control.Bind ((=<<), (<=<))
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (EXCEPTION(), throw, message)
import Control.Monad.Error.Class (throwError)
import Data.Either (Either(), either)
import Data.Foldable (foldMap, foldr, traverse_)
import Data.Array ((..), index, uncons, length, singleton, elemIndex, zip, filter, head, intersectBy)
import Data.List (toUnfoldable)
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
import Test.Utils (ifTrue, ifFalse, orIfItFails, passover, throwIfEmpty, throwIfNotEmpty, singletonValue)
import Test.XPath as XPath
import Debug.Trace

type Property = Tuple String (Maybe String)
type Indexed a = Tuple Int a

-- Basic XPath dependent finders
findAll' :: String -> Check (Array Element)
findAll' = map toUnfoldable <<< findElements <=< byXPath

find' :: String -> Check Element
find' xPath =
  singletonValue throwNoElements throwMoreThanOneElement =<< findAll' xPath
  where
  throwNoElements =
    liftEff $ throw $ XPath.errorMessage "Couldn't find an element" xPath
  throwMoreThanOneElement i =
    liftEff $ throw $ XPath.errorMessage ("Found (" ++ show i ++ ") more than one element") xPath

findAtLeastOne' :: String -> Check (Array Element)
findAtLeastOne' xPath =
  headOrThrow =<< findAll xPath
  where
  headOrThrow = passover (liftEff <<< throwIfEmpty noElementsMessage)
  noElementsMessage = XPath.errorMessage "Couldn't find an element" xPath

find :: String -> Check Element
find xPath = expectPresented xPath *> find' xPath

findAll :: String -> Check (Array Element)
findAll xPath = expectPresented xPath *> findAll' xPath

-- Property and XPath dependent finders
findAllWithProperties' :: Array Property -> String -> Check (Array Element)
findAllWithProperties' properties = elementsWithProperties properties <=< findAll'

findAtLeastOneWithProperties' :: Array Property -> String -> Check (Array Element)
findAtLeastOneWithProperties' properties xPath =
  headOrThrow =<< elementsWithProperties properties =<< findAll' xPath
  where
  headOrThrow = passover (liftEff <<< throwIfEmpty noElementsMessage)
  noElementsMessage =
    XPath.errorMessage (withPropertiesMessage properties "Couldn't find an element") xPath

findWithProperties' :: Array Property -> String -> Check Element
findWithProperties' properties xPath =
  singletonValue throwNoElements throwMoreThanOneElement =<< findAllWithProperties' properties xPath
  where
  throwNoElements = liftEff $ throw $ noElementWithPropertiesError properties xPath
  throwMoreThanOneElement i = liftEff $ throw $ moreThanOneElementMessage i
  moreThanOneElementRawMessage i = "Found (" ++ show i ++ ") more than one element"
  moreThanOneElementMessage i =
    XPath.errorMessage (withPropertiesMessage properties $ moreThanOneElementRawMessage i) xPath

findAllWithProperties :: Array Property -> String -> Check (Array Element)
findAllWithProperties properties xPath =
  expectPresentedWithProperties properties xPath *> findAllWithProperties' properties xPath

findAtLeastOneWithProperties :: Array Property -> String -> Check (Array Element)
findAtLeastOneWithProperties properties xPath =
  expectPresentedWithProperties properties xPath *> findAtLeastOneWithProperties' properties xPath

findWithProperties :: Array Property -> String -> Check Element
findWithProperties properties xPath =
  expectPresentedWithProperties properties xPath *> findWithProperties' properties xPath

-- Errors
printPropertyValue :: Maybe String -> String
printPropertyValue = maybe "null" show

printProperty :: String -> Maybe String -> String
printProperty name value = name ++ "=" ++ printPropertyValue value

printProperties :: Array Property -> String
printProperties = joinWith " " <<< map (uncurry printProperty)

noElementWithPropertiesError :: Array Property -> String -> String
noElementWithPropertiesError properties =
  XPath.errorMessage $ withPropertiesMessage properties "Unable to find element with "

elementWithPropertiesError :: Array Property -> String -> String
elementWithPropertiesError properties =
  XPath.errorMessage $ withPropertiesMessage properties "Expected not to find element with "

withPropertiesMessage :: Array Property -> String -> String
withPropertiesMessage xs s | length xs == 0 = s
withPropertiesMessage xs s = s ++ " with the attributes or properties: " ++ printProperties xs

-- Expectations
expectHidden :: String -> Check Unit
expectHidden xPath = expectHiddenWithProperties [] xPath

expectPresented :: String -> Check Unit
expectPresented xPath = expectPresentedWithProperties [] xPath

validateJustTrue :: (String -> String) -> String -> Maybe String -> Check Unit
validateJustTrue _ _ (Just "true") = pure unit
validateJustTrue error xPath _ = liftEff $ throw $ error xPath

expectHiddenAria :: Array Property -> String -> Check Unit
expectHiddenAria properties xPath =
  liftEff <<< throwIfNotEmpty message =<< findAllWithProperties' properties notHiddenXPath
  where
  notHiddenXPath = xPath `XPath.ancestorOrSelf` "*[not(@aria-hidden='true')]"
  message = XPath.errorMessage (withPropertiesMessage properties rawMessage) xPath
  printedAriaHiddenProperty = printProperty "aria-hidden" (Just "true")
  rawMessage =
    "Expected an " ++ printedAriaHiddenProperty ++ " attribute of elements or their ancestors"

expectHiddenVisual :: Array Property -> String -> Check Unit
expectHiddenVisual properties xPath =
  traverse_ validate =<< findAllWithProperties' properties xPath
  where
  validate = ifTrue throwVisualError <=< isDisplayed
  throwVisualError = liftEff $ throw $ XPath.errorMessage message xPath
  message = withPropertiesMessage properties "Expected to find no visually displayed elements"

expectHiddenWithProperties :: Array Property -> String -> Check Unit
expectHiddenWithProperties properties xPath =
  tryRepeatedlyTo $ expectHiddenAria properties xPath *> expectHiddenVisual properties xPath

expectPresentedWithProperties :: Array Property -> String -> Check Unit
expectPresentedWithProperties properties xPath =
  tryRepeatedlyTo $ expectNode *> expectPresentedVisual *> expectPresentedAria
  where
  verify = either (const $ pure unit) <<< const <<< throwExpectation
  throwExpectation :: String -> Check Unit
  throwExpectation = liftEff <<< throw
  expectNode = findAtLeastOneWithProperties' properties xPath
  expectPresentedVisual = verify visualError =<< (attempt $ expectHiddenVisual properties xPath)
  expectPresentedAria = verify ariaError =<< (attempt $ expectHiddenAria properties xPath)
  visualError = XPath.errorMessage (withPropertiesMessage properties visualMessage) xPath
  ariaError = XPath.errorMessage (withPropertiesMessage properties ariaMessage) xPath
  visualMessage = "Expected to find only visually presented elements"
  ariaMessage = "Expected no true values for \"aria-hidden\" on elements or their ancestors found"

-- Interaction utilities
checkedProperty :: Maybe String -> Property
checkedProperty checked = Tuple "checked" checked

propertiesAndChecked :: Maybe String -> Array Property -> Array Property
propertiesAndChecked checked = append [checkedProperty checked]

-- XPath dependent interactions
check' :: (Maybe String) -> String -> Check Unit
check' checked xPath =
  traverse_ clickElement =<< findAllWithProperties [checkedProperty checked] xPath

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
click x = tryRepeatedlyTo $ clickElement =<< find x

clickAll :: String -> Check Unit
clickAll = tryRepeatedlyTo <<< (traverse_ clickElement) <=< findAll

hover :: String -> Check Unit
hover = tryRepeatedlyTo <<< hoverElement <=< find

-- XPath and property dependent interactions
checkWithProperties' :: (Maybe String) -> Array Property -> String -> Check Unit
checkWithProperties' checked properties xPath =
  tryRepeatedlyTo $ (traverse_ clickElement) =<< findAtLeastOneWithProperties properties' xPath
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
  tryRepeatedlyTo <<< (traverse_ clickElement) <=< findAtLeastOneWithProperties properties

hoverWithProperties :: Array Property -> String -> Check Unit
hoverWithProperties properties =
  tryRepeatedlyTo <<< hoverElement <=< findWithProperties properties

-- Independent interactions
typeString :: String -> Check Unit
typeString string = sequence $ FeatureSequence.keys string

pressEnter :: Check Unit
pressEnter = sequence $ FeatureSequence.sendEnter

selectAll :: Check Unit
selectAll = (sequence <<< FeatureSequence.selectAll) =<< getModifierKey

-- Element dependent interactions
clickElement :: Element -> Check Unit
clickElement element =
  sequence $ Sequence.mouseDown leftButton element *> Sequence.mouseUp leftButton element

clickAllElements :: Array Element -> Check Unit
clickAllElements = traverse_ clickElement

hoverElement :: Element -> Check Unit
hoverElement = tryRepeatedlyTo <<< sequence <<< Sequence.hover

provideFieldValueElement :: String -> Element -> Check Unit
provideFieldValueElement value element =
  clickElement element *> selectAll *> typeString value

selectFromDropdownElement :: String -> Element -> Check Unit
selectFromDropdownElement text element =
  clickElement element *> typeString text *> pressEnter

-- Element utilities
elementsWithProperties :: Array Property -> Array Element -> Check (Array Element)
elementsWithProperties properties | length properties == 0 = pure <<< id
elementsWithProperties properties =
  tryRepeatedlyTo <<< (pure <<< map snd) <=< flip indexedElementsWithProperties properties <<< zipWithIndices
  where
  values name = traverse (later 0 <<< flip getAttribute name)
  mapEq x = map (eq x)
  tailHeadIntersectionBy f o = foldr (intersectBy f) o.head o.tail
  intersectArrayOfArraysBy f = maybe [] (tailHeadIntersectionBy f) <<< uncons
  filterByBooleans xs = map fst <<< filter snd <<< zip xs
  toIndexes xs = 0 .. (length xs - 1)
  zipWithIndices xs = zip (toIndexes xs) xs
  fstEq x y = fst x == fst y
  indexedElementsWithProperty elements name expectedValue =
    pure <<< filterByBooleans elements <<< mapEq expectedValue =<< values name (map snd elements)
  nonIntersectedIndexedElementsWithProperties elements =
    traverse (uncurry (indexedElementsWithProperty elements))
  indexedElementsWithProperties elements =
    pure <<< intersectArrayOfArraysBy fstEq <=< nonIntersectedIndexedElementsWithProperties elements
