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
--  , expectNotPresented
--  , expectPresentedWithProperty
--  , expectNotPresentedWithProperty
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
import Selenium.ActionSequence as Sequence
import Selenium.Monad (getAttribute, clickEl, attempt, later, sequence, getText, byXPath, byId, tryRepeatedlyTo, findExact, findElements, loseElement, isDisplayed, childExact, getInnerHtml, sendKeysEl)
import Selenium.MouseButton (leftButton)
import Selenium.Types (Element())
import Test.Feature.ActionSequence as FeatureSequence
import Test.Feature.Monad (Feature(), getModifierKey)
import Test.Feature.Log (warnMsg)
import Test.Utils (ifTrue, ifFalse, orIfItFails, passover, throwIfEmpty, throwIfNotEmpty, singletonValue)
import XPath as XPath
import Debug.Trace

type Property = Tuple String (Maybe String)
type Indexed a = Tuple Int a

-- Basic XPath dependent finders
findAll' :: forall eff o. String -> Feature eff o (Array Element)
findAll' = map toUnfoldable <<< findElements <=< byXPath

find' :: forall eff o. String -> Feature eff o Element
find' xPath =
  singletonValue throwNoElements throwMoreThanOneElement =<< findAll' xPath
  where
  throwNoElements =
    liftEff $ throw $ XPath.errorMessage "Couldn't find an element" xPath
  throwMoreThanOneElement i =
    liftEff $ throw $ XPath.errorMessage ("Found (" ++ show i ++ ") more than one element") xPath

findAtLeastOne' :: forall eff o. String -> Feature eff o (Array Element)
findAtLeastOne' xPath =
  headOrThrow =<< findAll xPath
  where
  headOrThrow = passover (liftEff <<< throwIfEmpty noElementsMessage)
  noElementsMessage = XPath.errorMessage "Couldn't find an element" xPath

find :: forall eff o. String -> Feature eff o Element
find xPath = expectPresented xPath *> find' xPath

findAll :: forall eff o. String -> Feature eff o (Array Element)
findAll xPath = expectPresented xPath *> findAll' xPath

-- Property and XPath dependent finders
findAllWithProperties' :: forall eff o. Array Property -> String -> Feature eff o (Array Element)
findAllWithProperties' properties = elementsWithProperties properties <=< findAll'

findAtLeastOneWithProperties' :: forall eff o. Array Property -> String -> Feature eff o (Array Element)
findAtLeastOneWithProperties' properties xPath =
  headOrThrow =<< elementsWithProperties properties =<< findAll' xPath
  where
  headOrThrow = passover (liftEff <<< throwIfEmpty noElementsMessage)
  noElementsMessage =
    XPath.errorMessage (withPropertiesMessage properties "Couldn't find an element") xPath

findWithProperties' :: forall eff o. Array Property -> String -> Feature eff o Element
findWithProperties' properties xPath =
  singletonValue throwNoElements throwMoreThanOneElement =<< findAllWithProperties' properties xPath
  where
  throwNoElements = liftEff $ throw $ noElementWithPropertiesError properties xPath
  throwMoreThanOneElement i = liftEff $ throw $ moreThanOneElementMessage i
  moreThanOneElementRawMessage i = "Found (" ++ show i ++ ") more than one element"
  moreThanOneElementMessage i =
    XPath.errorMessage (withPropertiesMessage properties $ moreThanOneElementRawMessage i) xPath

findAllWithProperties :: forall eff o. Array Property -> String -> Feature eff o (Array Element)
findAllWithProperties properties xPath =
  expectPresentedWithProperties properties xPath *> findAllWithProperties' properties xPath

findAtLeastOneWithProperties :: forall eff o. Array Property -> String -> Feature eff o (Array Element)
findAtLeastOneWithProperties properties xPath =
  expectPresentedWithProperties properties xPath *> findAtLeastOneWithProperties' properties xPath

findWithProperties :: forall eff o. Array Property -> String -> Feature eff o Element
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
expectNotPresented' :: forall eff o. String -> Feature eff o Unit
expectNotPresented' xPath = expectNotPresentedWithProperties' [] xPath

expectPresented' :: forall eff o. String -> Feature eff o Unit
expectPresented' xPath = expectPresentedWithProperties' [] xPath

expectNotPresented :: forall eff o. String -> Feature eff o Unit
expectNotPresented xPath = expectNotPresentedWithProperties [] xPath

expectPresented :: forall eff o. String -> Feature eff o Unit
expectPresented xPath = expectPresentedWithProperties [] xPath

validateJustTrue :: forall eff o. (String -> String) -> String -> Maybe String -> Feature eff o Unit
validateJustTrue _ _ (Just "true") = pure unit
validateJustTrue error xPath _ = liftEff $ throw $ error xPath

expectNotPresentedAria :: forall eff o. Array Property -> String -> Feature eff o Unit
expectNotPresentedAria properties xPath =
  liftEff <<< throwIfNotEmpty message =<< findAllWithProperties' properties notHiddenXPath
  where
  notHiddenXPath = xPath `XPath.ancestorOrSelf` "*[not(@aria-hidden='true')]"
  message = XPath.errorMessage (withPropertiesMessage properties rawMessage) xPath
  printedAriaHiddenProperty = printProperty "aria-hidden" (Just "true")
  rawMessage =
    "Expected an " ++ printedAriaHiddenProperty ++ " attribute of elements or their ancestors"

expectNotPresentedVisual :: forall eff o. Array Property -> String -> Feature eff o Unit
expectNotPresentedVisual properties xPath =
  traverse_ validate =<< findAllWithProperties' properties xPath
  where
  validate = ifTrue throwVisualError <=< isDisplayed
  throwVisualError = liftEff $ throw $ XPath.errorMessage message xPath
  message = withPropertiesMessage properties "Expected to find no visually displayed elements"

expectNotPresentedWithProperties' :: forall eff o. Array Property -> String -> Feature eff o Unit
expectNotPresentedWithProperties' properties xPath =
  expectNotPresentedVisual properties xPath *> expectNotPresentedAria properties xPath

expectPresentedWithProperties' :: forall eff o. Array Property -> String -> Feature eff o Unit
expectPresentedWithProperties' properties xPath =
  expectNode *> expectPresentedVisual *> expectPresentedAria
  where
  verify = either (const $ pure unit) <<< const <<< throwExpectation
  throwExpectation :: forall eff o. String -> Feature eff o Unit
  throwExpectation = liftEff <<< throw
  expectNode = findAtLeastOneWithProperties' properties xPath
  expectPresentedVisual = verify visualError =<< (attempt $ expectNotPresentedVisual properties xPath)
  expectPresentedAria = verify ariaError =<< (attempt $ expectNotPresentedAria properties xPath)
  visualError = XPath.errorMessage (withPropertiesMessage properties visualMessage) xPath
  ariaError = XPath.errorMessage (withPropertiesMessage properties ariaMessage) xPath
  visualMessage = "Expected to find only visually presented elements"
  ariaMessage = "Expected no true values for \"aria-hidden\" on elements or their ancestors found"

expectNotPresentedWithProperties :: forall eff o. Array Property -> String -> Feature eff o Unit
expectNotPresentedWithProperties properties xPath =
  tryRepeatedlyTo $ expectNotPresentedWithProperties' properties xPath

expectPresentedWithProperties :: forall eff o. Array Property -> String -> Feature eff o Unit
expectPresentedWithProperties properties xPath =
  tryRepeatedlyTo $ expectPresentedWithProperties' properties xPath

-- Interaction utilities
checkedProperty :: Maybe String -> Property
checkedProperty checked = Tuple "checked" checked

updateProperty :: String -> Maybe String -> Array Property -> Array Property
updateProperty name value =
  append [Tuple name value] <<< filter (not <<< eq name <<< fst)

-- XPath dependent interactions
check :: forall eff o. String -> Feature eff o Unit
check xPath = checkWithProperties [] xPath

uncheck :: forall eff o. String -> Feature eff o Unit
uncheck xPath = uncheckWithProperties [] xPath

pushRadioButton :: forall eff o. String -> Feature eff o Unit
pushRadioButton xPath = pushRadioButtonWithProperties [] xPath

provideFieldValue :: forall eff o. String -> String -> Feature eff o Unit
provideFieldValue xPath value = provideFieldValueWithProperties [] xPath value

provideFieldValueWithExpectedValue :: forall eff o. String -> String -> String -> Feature eff o Unit
provideFieldValueWithExpectedValue expectedValue xPath value =
  provideFieldValueWithPropertiesAndExpectedValue [] expectedValue xPath value

selectFromDropdown :: forall eff o. String -> String -> Feature eff o Unit
selectFromDropdown xPath text = selectFromDropdownWithProperties [] xPath text

click :: forall eff o. String -> Feature eff o Unit
click xPath = clickWithProperties [] xPath

clickWithExpectation :: forall eff o. Feature eff o Unit -> String -> Feature eff o Unit
clickWithExpectation expectation xPath =
  clickWithPropertiesAndExpectation [] expectation xPath

clickAll :: forall eff o. String -> Feature eff o Unit
clickAll xPath = clickAllWithProperties [] xPath

hover :: forall eff o. String -> Feature eff o Unit
hover xPath = hoverWithProperties [] xPath

provideFileInputValue :: forall eff o. String -> String -> Feature eff o Unit
provideFileInputValue xPath fileName = provideFileInputValueWithProperties [] xPath fileName

-- XPath and property dependent interactions
checkWithProperties' :: forall eff o. (Maybe String) -> Array Property -> String -> Feature eff o Unit
checkWithProperties' checked properties xPath =
  (traverse_ clickElement) =<< findAtLeastOneWithProperties properties' xPath
  where
  properties' = updateProperty "checked" checked properties

checkWithProperties :: forall eff o. Array Property -> String -> Feature eff o Unit
checkWithProperties properties xPath = tryRepeatedlyTo $ check *> expectChecked
  where
  check = checkWithProperties' Nothing properties xPath
  expectChecked = expectPresentedWithProperties expectedProperties xPath
  expectedProperties = updateProperty "checked" (Just "true") properties

uncheckWithProperties :: forall eff o. Array Property -> String -> Feature eff o Unit
uncheckWithProperties properties xPath = tryRepeatedlyTo $ uncheck *> expectUnchecked
  where
  uncheck = checkWithProperties' (Just "true") properties xPath
  expectUnchecked = expectPresentedWithProperties expectedProperties xPath
  expectedProperties = updateProperty "checked" Nothing properties

pushRadioButtonWithProperties :: forall eff o. Array Property -> String -> Feature eff o Unit
pushRadioButtonWithProperties properties xPath = tryRepeatedlyTo $ push *> expectPushed
  where
  push = checkWithProperties' Nothing properties xPath
  expectPushed = expectPresentedWithProperties expectedProperties xPath
  expectedProperties = updateProperty "checked" (Just "true") properties

provideFieldValueWithProperties :: forall eff o. Array Property -> String -> String -> Feature eff o Unit
provideFieldValueWithProperties properties xPath value =
  tryRepeatedlyTo $ provideFieldValueElement value =<< findWithProperties properties xPath

provideFieldValueWithPropertiesAndExpectedValue :: forall eff o. Array Property -> String -> String -> String -> Feature eff o Unit
provideFieldValueWithPropertiesAndExpectedValue properties expectedValue xPath value =
  tryRepeatedlyTo $ provideValue *> expectValue
  where
  provideValue = provideFieldValueElement value =<< findWithProperties properties xPath
  expectValue = expectPresentedWithProperties expectedProperties xPath
  expectedProperties = updateProperty "value" (Just expectedValue) properties

selectFromDropdownWithProperties :: forall eff o. Array Property -> String -> String -> Feature eff o Unit
selectFromDropdownWithProperties properties xPath text =
  tryRepeatedlyTo $ select *> expectSelected
  where
  expectSelected = expectPresentedWithProperties expectedProperties xPath
  expectedProperties = updateProperty "value" (Just text) properties
  select = selectFromDropdownElement text =<< findWithProperties properties xPath

clickWithProperties :: forall eff o. Array Property -> String -> Feature eff o Unit
clickWithProperties properties =
  tryRepeatedlyTo <<< clickElement <=< findWithProperties properties

clickWithPropertiesAndExpectation :: forall eff o. Array Property -> Feature eff o Unit -> String -> Feature eff o Unit
clickWithPropertiesAndExpectation properties expectation xPath =
  tryRepeatedlyTo do
    clickElement =<< findWithProperties properties xPath
    expectation

clickAllWithProperties :: forall eff o. Array Property -> String -> Feature eff o Unit
clickAllWithProperties properties =
  tryRepeatedlyTo <<< (traverse_ clickElement) <=< findAtLeastOneWithProperties properties

hoverWithProperties :: forall eff o. Array Property -> String -> Feature eff o Unit
hoverWithProperties properties =
  tryRepeatedlyTo <<< hoverElement <=< findWithProperties properties

provideFileInputValueWithProperties :: forall eff o. Array Property -> String -> String -> Feature eff o Unit
provideFileInputValueWithProperties properties xPath filePath =
  tryRepeatedlyTo $ sendKeysEl filePath =<< findWithProperties' properties xPath

-- Independent interactions
typeString :: forall eff o. String -> Feature eff o Unit
typeString string = sequence $ FeatureSequence.keys string

pressEnter :: forall eff o. Feature eff o Unit
pressEnter = sequence $ FeatureSequence.sendEnter

selectAll :: forall eff o. Feature eff o Unit
selectAll = (sequence <<< FeatureSequence.selectAll) =<< getModifierKey

copy :: forall eff o. Feature eff o Unit
copy = (sequence <<< FeatureSequence.copy) =<< getModifierKey

paste :: forall eff o. Feature eff o Unit
paste = (sequence <<< FeatureSequence.paste) =<< getModifierKey

undo :: forall eff o. Feature eff o Unit
undo = (sequence <<< FeatureSequence.undo) =<< getModifierKey

focusAddressBar :: forall eff o. Feature eff o Unit
focusAddressBar = (sequence <<< FeatureSequence.focusAddressBar) =<< getModifierKey

-- Element dependent interactions
clickElement :: forall eff o. Element -> Feature eff o Unit
clickElement element = do
  sequence $ Sequence.mouseDown leftButton element
  sequence $ Sequence.mouseUp leftButton element

clickAllElements :: forall eff o. Array Element -> Feature eff o Unit
clickAllElements = traverse_ clickElement

hoverElement :: forall eff o. Element -> Feature eff o Unit
hoverElement = tryRepeatedlyTo <<< sequence <<< Sequence.hover

provideFieldValueElement :: forall eff o. String -> Element -> Feature eff o Unit
provideFieldValueElement value element =
  clickElement element *> selectAll *> typeString value

selectFromDropdownElement :: forall eff o. String -> Element -> Feature eff o Unit
selectFromDropdownElement text element =
  clickElement element *> typeString text *> pressEnter

-- Attribute getters (please only use these) when you don't know the value of an attribute or property)
--getAttributeOrProperty :: forall eff o. String -> String -> Feature eff o (Maybe String)
--getAttributeOrProperty xPath name = getAttributeOrPropertyWithProperties [] xPath name
--
--getAttributeOrPropertyWithProperties :: forall eff o. Array Property -> String -> String -> Feature eff o (Maybe String)
--getAttributeOrPropertyWithProperties properties xPath name =
--  flip getAttribute name =<< findWithProperties properties xPath

-- Element filters
elementsWithProperties :: forall eff o. Array Property -> Array Element -> Feature eff o (Array Element)
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
