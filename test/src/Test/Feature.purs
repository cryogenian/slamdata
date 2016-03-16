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
import Control.Monad (unless)
import Control.Monad.Aff.Class (liftAff)
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
import Graphics.EasyImage as Ge
import Graphics.ImageDiff as Gi
import Prelude
import Selenium.ActionSequence as Sequence
import Selenium.Monad (getAttribute, clickEl, attempt, later, sequence, getText, byXPath, byId, tryRepeatedlyTo, findExact, findElements, loseElement, isDisplayed, childExact, getInnerHtml, getLocation, getSize, saveScreenshot)
import Selenium.MouseButton (leftButton)
import Selenium.Types (Element())
import Test.Feature.ActionSequence as FeatureSequence
import Test.Feature.Monad (Feature(), getModifierKey)
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

expectNotPresentedWithProperties :: forall eff o. Array Property -> String -> Feature eff o Unit
expectNotPresentedWithProperties properties xPath =
  tryRepeatedlyTo $ expectNotPresentedVisual properties xPath *> expectNotPresentedAria properties xPath

expectPresentedWithProperties :: forall eff o. Array Property -> String -> Feature eff o Unit
expectPresentedWithProperties properties xPath =
  tryRepeatedlyTo $ expectNode *> expectPresentedVisual *> expectPresentedAria
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

type XPath = String
type FilePath = String

expectMatchScreenshot
  :: forall eff o
   . XPath
  -> FilePath
  -> FilePath
  -> Feature eff o Unit
expectMatchScreenshot =
  expectMatchScreenshotWithProperties []


expectMatchScreenshotWithProperties
  :: forall eff o
   . Array Property
  -> XPath
  -> FilePath
  -> FilePath
  -> Feature eff o Unit
expectMatchScreenshotWithProperties properties xpath presentedFPath expectedFPath =
  tryRepeatedlyTo
    $ ifFalse throwMessage
    =<< expectElementMatchScreenshot presentedFPath expectedFPath
    =<< findWithProperties' properties xpath
  where
  throwMessage = liftEff $ throw message
  message = XPath.errorMessage rawMessage xpath
  rawMessage = "Expected screenshot " <> expectedFPath <> " to match element"

-- Interaction utilities
checkedProperty :: Maybe String -> Property
checkedProperty checked = Tuple "checked" checked

propertiesAndChecked :: Maybe String -> Array Property -> Array Property
propertiesAndChecked checked = append [checkedProperty checked]

-- XPath dependent interactions
check' :: forall eff o. (Maybe String) -> String -> Feature eff o Unit
check' checked xPath =
  traverse_ clickElement =<< findAllWithProperties [checkedProperty checked] xPath

check :: forall eff o. String -> Feature eff o Unit
check = check' Nothing

uncheck :: forall eff o. String -> Feature eff o Unit
uncheck = check' (Just "true")

pushRadioButton :: forall eff o. String -> Feature eff o Unit
pushRadioButton = check' Nothing

provideFieldValue :: forall eff o. String -> String -> Feature eff o Unit
provideFieldValue xPath value =
  tryRepeatedlyTo $ provideFieldValueElement value =<< find xPath

selectFromDropdown :: forall eff o. String -> String -> Feature eff o Unit
selectFromDropdown xPath text =
  tryRepeatedlyTo $ selectFromDropdownElement text =<< find xPath

click :: forall eff o. String -> Feature eff o Unit
click x = tryRepeatedlyTo $ clickElement =<< find x

clickAll :: forall eff o. String -> Feature eff o Unit
clickAll = tryRepeatedlyTo <<< (traverse_ clickElement) <=< findAll

hover :: forall eff o. String -> Feature eff o Unit
hover = tryRepeatedlyTo <<< hoverElement <=< find

-- XPath and property dependent interactions
checkWithProperties' :: forall eff o. (Maybe String) -> Array Property -> String -> Feature eff o Unit
checkWithProperties' checked properties xPath =
  tryRepeatedlyTo $ (traverse_ clickElement) =<< findAtLeastOneWithProperties properties' xPath
  where
  properties' = propertiesAndChecked checked properties

checkWithProperties :: forall eff o. Array Property -> String -> Feature eff o Unit
checkWithProperties = checkWithProperties' Nothing

uncheckWithProperties :: forall eff o. Array Property -> String -> Feature eff o Unit
uncheckWithProperties = checkWithProperties' (Just "true")

pushRadioButtonWithProperties :: forall eff o. Array Property -> String -> Feature eff o Unit
pushRadioButtonWithProperties = checkWithProperties' Nothing

provideFieldValueWithProperties :: forall eff o. Array Property -> String -> String -> Feature eff o Unit
provideFieldValueWithProperties properties xPath value =
  provideFieldValueElement value =<< findWithProperties properties xPath

selectFromDropdownWithProperties :: forall eff o. Array Property -> String -> String -> Feature eff o Unit
selectFromDropdownWithProperties properties xPath text =
  selectFromDropdownElement text =<< findWithProperties properties xPath

clickWithProperties :: forall eff o. Array Property -> String -> Feature eff o Unit
clickWithProperties properties =
  tryRepeatedlyTo <<< clickElement <=< findWithProperties properties

clickAllWithProperties :: forall eff o. Array Property -> String -> Feature eff o Unit
clickAllWithProperties properties =
  tryRepeatedlyTo <<< (traverse_ clickElement) <=< findAtLeastOneWithProperties properties

hoverWithProperties :: forall eff o. Array Property -> String -> Feature eff o Unit
hoverWithProperties properties =
  tryRepeatedlyTo <<< hoverElement <=< findWithProperties properties

-- Independent interactions
typeString :: forall eff o. String -> Feature eff o Unit
typeString string =
  sequence $ FeatureSequence.keys string

pressEnter :: forall eff o. Feature eff o Unit
pressEnter = sequence $ FeatureSequence.sendEnter

selectAll :: forall eff o. Feature eff o Unit
selectAll = (sequence <<< FeatureSequence.selectAll) =<< getModifierKey

-- Element dependent interactions
clickElement :: forall eff o. Element -> Feature eff o Unit
clickElement element =
  sequence $ Sequence.mouseDown leftButton element *> Sequence.mouseUp leftButton element

clickAllElements :: forall eff o. Array Element -> Feature eff o Unit
clickAllElements = traverse_ clickElement

hoverElement :: forall eff o. Element -> Feature eff o Unit
hoverElement = tryRepeatedlyTo <<< sequence <<< Sequence.hover

provideFieldValueElement :: forall eff o. String -> Element -> Feature eff o Unit
provideFieldValueElement value element =
  clickElement element
  *> selectAll
  *> typeString value
  -- Have no idea why api cell doesn't work w/o this line
  *> pressEnter

selectFromDropdownElement :: forall eff o. String -> Element -> Feature eff o Unit
selectFromDropdownElement text element =
  clickElement element *> typeString text *> pressEnter

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


expectElementMatchScreenshot
  :: forall eff o
   . FilePath
  -> FilePath
  -> Element
  -> Feature eff o Boolean
expectElementMatchScreenshot presentedFPath expectedFPath el = do
  size <- getSize el
  location <- getLocation el
  saveScreenshot presentedFPath
  liftAff
    $ Ge.cropInPlace
        size.width
        size.height
        location.x
        location.y
        presentedFPath
  liftAff $ Gi.diff
    {
      expected: expectedFPath
    , actual: presentedFPath
    , diff: Nothing
    , shadow: false
    }
