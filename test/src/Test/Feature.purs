module Test.Feature
  ( Properties
  , XPath
  , FilePath
  , accessUrlFromFieldValue
  , accessUrlFromFieldValueWithProperties
  , check
  , checkWithProperties
  , clear
  , clearWithProperties
  , click
  , clickWithProperties
  , clickNotRepeatedly
  , dragAndDrop
  , dragAndDropWithProperties
  , expectDownloadedTextFileToMatchFile
  , expectNotPresented
  , expectNotPresentedWithProperties
  , expectNotPresentedNotRepeatedly
  , expectNotPresentedWithPropertiesNotRepeatedly
  , expectPresented
  , expectPresentedWithProperties
  , expectPresentedNotRepeatedly
  , expectPresentedWithPropertiesNotRepeatedly
  , expectSelectValue
  , hover
  , hoverWithProperties
  , pressEnter
  , provideFieldValue
  , provideFieldValueWithProperties
  , provideFileInputValue
  , provideFileInputValueWithProperties
  , pushRadioButton
  , pushRadioButtonWithProperties
  , selectFromDropdown
  , selectFromDropdownWithProperties
  , typeString
  , uncheck
  , uncheckWithProperties
  , smallWaitTime
  , logCurrentScreen
  ) where

import SlamData.Prelude

import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (throw)

import Data.Array (elemIndex, length)
import Data.List (toUnfoldable)
import Data.List as L
import Data.Map as Map
import Data.String as String

import Node.Buffer (toString)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readFile, readTextFile, readdir, unlink)

import Selenium.ActionSequence as Sequence
import Selenium.Monad (get, getAttribute, clickEl, attempt, later, byXPath, tryRepeatedlyTo, findElements, isDisplayed, getLocation, getSize, saveScreenshot, sendKeysEl)
import Selenium.Monad as Selenium
import Selenium.Types (Element, Location)

import Test.Feature.ActionSequence as FeatureSequence
import Test.Feature.Log (warnMsg)
import Test.Feature.Monad (Feature, await)
import Test.Utils (appendToCwd, ifTrue, ifFalse, singletonValue, throwIfEmpty, passover)

import XPath as XPath

type Properties = Map.Map String (Maybe String)
type XPath = String
type FilePath = String

smallWaitTime ∷ Int
smallWaitTime = 500

finderInternalFindAllNotRepeatedly ∷ ∀ eff o. XPath → Feature eff o (Array Element)
finderInternalFindAllNotRepeatedly =
  map toUnfoldable ∘ findElements <=< byXPath

-- Basic XPath dependent finders
findAllNotRepeatedly ∷ ∀ eff o. XPath → Feature eff o (Array Element)
findAllNotRepeatedly = findAllWithPropertiesNotRepeatedly Map.empty

findNotRepeatedly ∷ ∀ eff o. XPath → Feature eff o Element
findNotRepeatedly xPath =
  singletonValue throwNoElements throwMoreThanOneElement
    =<< findAllNotRepeatedly xPath
  where
  throwNoElements =
    liftEff
      $ throw
      $ XPath.errorMessage "Couldn't find an element" xPath

  throwMoreThanOneElement i =
    liftEff
      $ throw
      $ XPath.errorMessage
          ("Found (" ⊕ show i ⊕ ") more than one element")
          xPath

findAtLeastOneNotRepeatedly ∷ ∀ eff o. XPath → Feature eff o (Array Element)
findAtLeastOneNotRepeatedly xPath =
  headOrThrow =<< findAllNotRepeatedly xPath
  where
  headOrThrow = passover (throwIfEmpty noElementsMessage)
  noElementsMessage = XPath.errorMessage "Couldn't find an element" xPath

find ∷ ∀ eff o. XPath → Feature eff o Element
find xPath = expectPresented xPath *> findNotRepeatedly xPath

findAll ∷ ∀ eff o. XPath → Feature eff o (Array Element)
findAll xPath = expectPresented xPath *> findAllNotRepeatedly xPath

-- Property and XPath dependent finders
findAllInvisibleWithPropertiesNotRepeatedly
  ∷ ∀ eff o
  . Properties
  → XPath
  → Feature eff o (Array Element)
findAllInvisibleWithPropertiesNotRepeatedly properties xPath =
  elementsWithProperties properties
    =<< finderInternalFindAllNotRepeatedly xPath

findAllWithPropertiesNotRepeatedly
  ∷ ∀ eff o
  . Properties
  → XPath
  → Feature eff o (Array Element)
findAllWithPropertiesNotRepeatedly properties xPath =
  passover (expectPresentedVisualElements properties xPath)
    =<< elementsWithProperties properties
    =<< finderInternalFindAllNotRepeatedly xPath

findAtLeastOneWithPropertiesNotRepeatedly
  ∷ ∀ eff o
  . Properties
  → XPath
  → Feature eff o (Array Element)
findAtLeastOneWithPropertiesNotRepeatedly properties xPath =
  headOrThrow
    =<< elementsWithProperties properties
    =<< findAllNotRepeatedly xPath
  where
  headOrThrow =
    passover (throwIfEmpty noElementsMessage)
  noElementsMessage =
    XPath.errorMessage
      (withPropertiesMessage properties "Couldn't find an element")
      xPath

findInvisibleWithPropertiesNotRepeatedly
  ∷ ∀ eff o
  . Properties
  → XPath
  → Feature eff o Element
findInvisibleWithPropertiesNotRepeatedly properties xPath =
  singletonValue throwNoElements throwMoreThanOneElement
    =<< findAllInvisibleWithPropertiesNotRepeatedly properties xPath
  where
  throwNoElements =
    liftEff $ throw $ noElementWithPropertiesError properties xPath

  throwMoreThanOneElement i =
    liftEff $ throw $ moreThanOneElementMessage i

  moreThanOneElementRawMessage i = "Found (" ⊕ show i ⊕ ") more than one element"
  moreThanOneElementMessage i =
    XPath.errorMessage
      (withPropertiesMessage properties
       $ moreThanOneElementRawMessage i) xPath

findWithPropertiesNotRepeatedly
  ∷ ∀ eff o
  . Properties
  → XPath
  → Feature eff o Element
findWithPropertiesNotRepeatedly properties xPath =
  singletonValue throwNoElements throwMoreThanOneElement
    =<< findAllWithPropertiesNotRepeatedly properties xPath
  where
  throwNoElements =
    liftEff $ throw $ noElementWithPropertiesError properties xPath

  throwMoreThanOneElement i =
    liftEff $ throw $ moreThanOneElementMessage i

  moreThanOneElementRawMessage i = "Found (" ⊕ show i ⊕ ") more than one element"
  moreThanOneElementMessage i =
    XPath.errorMessage
      (withPropertiesMessage properties
       $ moreThanOneElementRawMessage i) xPath

findAllWithProperties
  ∷ ∀ eff o
  . Properties
  → XPath
  → Feature eff o (Array Element)
findAllWithProperties properties xPath =
  expectPresentedWithProperties properties xPath
    *> findAllWithPropertiesNotRepeatedly properties xPath

findAtLeastOneWithProperties
  ∷ ∀ eff o
  . Properties
  → XPath
  → Feature eff o (Array Element)
findAtLeastOneWithProperties properties xPath =
  expectPresentedWithProperties properties xPath
    *> findAtLeastOneWithPropertiesNotRepeatedly properties xPath

findWithProperties ∷ ∀ eff o. Properties → XPath → Feature eff o Element
findWithProperties properties xPath =
  expectPresentedWithProperties properties xPath
    *> findWithPropertiesNotRepeatedly properties xPath

-- Errors
printPropertyValue ∷ Maybe String → String
printPropertyValue = maybe "null" show

printProperty ∷ String → Maybe String → String
printProperty name value =
  name ⊕ "=" ⊕ printPropertyValue value

printProperties ∷ Properties → String
printProperties =
  String.joinWith " "
    ∘ L.toUnfoldable
    ∘ map (uncurry printProperty)
    ∘ Map.toList

noElementWithPropertiesError ∷ Properties → XPath → String
noElementWithPropertiesError properties =
  XPath.errorMessage
    $ withPropertiesMessage properties "Unable to find element "

elementWithPropertiesError ∷ Properties → XPath → String
elementWithPropertiesError properties =
  XPath.errorMessage
    $ withPropertiesMessage properties "Expected not to find element "

withPropertiesMessage ∷ Properties → String → String
withPropertiesMessage mp s
  | Map.isEmpty mp = s
withPropertiesMessage xs s =
  s ⊕ " with the attributes or properties: " ⊕ printProperties xs

-- Expectations
expectNotPresentedNotRepeatedly ∷ ∀ eff o. XPath → Feature eff o Unit
expectNotPresentedNotRepeatedly xPath =
  expectNotPresentedWithPropertiesNotRepeatedly Map.empty xPath

expectPresentedNotRepeatedly ∷ ∀ eff o. XPath → Feature eff o Unit
expectPresentedNotRepeatedly xPath =
  expectPresentedWithPropertiesNotRepeatedly Map.empty xPath

-- | Expect nodes found with the provided XPath to either:
-- |
-- | * not exist
-- | * or be hidden visually and for them and their ancestors to be aria-hidden.
expectNotPresented ∷ ∀ eff o. XPath → Feature eff o Unit
expectNotPresented xPath = expectNotPresentedWithProperties Map.empty xPath

-- | Expect nodes found with the providied XPath to exist, be presented visually
-- | and for them and their ancestors not to be aria-hidden.
expectPresented ∷ ∀ eff o. XPath → Feature eff o Unit
expectPresented xPath = expectPresentedWithProperties Map.empty xPath

validateJustTrue
  ∷ ∀ eff o
  . (String → String)
  → String
  → Maybe String
  → Feature eff o Unit
validateJustTrue _ _ (Just "true") = pure unit
validateJustTrue error xPath _ = liftEff $ throw $ error xPath

expectNotPresentedAria ∷ ∀ eff o. Properties → XPath → Feature eff o Unit
expectNotPresentedAria properties xPath =
  ifFalse checkAria ∘ eq 0 ∘ length
    =<< findAllInvisibleWithPropertiesNotRepeatedly properties xPath
  where
  checkAria =
    throwIfEmpty message
      =<< findAllInvisibleWithPropertiesNotRepeatedly properties hiddenXPath
  hiddenXPath =
    xPath `XPath.ancestorOrSelf` "*[@aria-hidden='true']"

  message =
    XPath.errorMessage
      (withPropertiesMessage properties rawMessage)
      xPath

  printedAriaHiddenProperty =
    printProperty "aria-hidden" (Just "true")

  rawMessage =
    "Expected no elements to be found or for them or their ancestors to have an "
      ⊕ printedAriaHiddenProperty
      ⊕ " attribute"

expectNotPresentedVisual
  ∷ ∀ eff o
  . Properties
  → XPath
  → Feature eff o Unit
expectNotPresentedVisual properties xPath =
  expectNotPresentedVisualElements properties xPath
    =<< findAllInvisibleWithPropertiesNotRepeatedly properties xPath

-- | Expect nodes found with the provided XPath which have the provided
-- | attributes or properties to either:
-- |
-- | * not exist
-- | * or be hidden visually and for them or their ancestors to be aria-hidden.
expectNotPresentedWithProperties
  ∷ ∀ eff o
  . Properties
  → XPath
  → Feature eff o Unit
expectNotPresentedWithProperties properties xPath =
  tryRepeatedlyTo
    $ expectNotPresentedWithPropertiesNotRepeatedly properties xPath

expectNotPresentedWithPropertiesNotRepeatedly
  ∷ ∀ eff o
  . Properties
  → XPath
  → Feature eff o Unit
expectNotPresentedWithPropertiesNotRepeatedly properties xPath =
  expectNotPresentedVisual properties xPath
    *> expectNotPresentedAria properties xPath

-- | Expect nodes found with the providied XPath which have the provided
-- | attributes or properties to exist, be presented visually and for them or
-- | their ancestors not to be aria-hidden.
expectPresentedWithProperties
  ∷ ∀ eff o
  . Properties
  → XPath
  → Feature eff o Unit
expectPresentedWithProperties ps xPath =
  tryRepeatedlyTo $ expectPresentedWithPropertiesNotRepeatedly ps xPath

expectPresentedWithPropertiesNotRepeatedly
  ∷ ∀ eff o
  . Properties
  → XPath
  → Feature eff o Unit
expectPresentedWithPropertiesNotRepeatedly properties xPath =
  expectNode
    *> expectPresentedVisual
    *> expectPresentedAria
  where
  verify =
    either (const $ pure unit) ∘ const ∘ throwExpectation

  throwExpectation =
    liftEff ∘ throw

  expectNode =
    findAtLeastOneWithPropertiesNotRepeatedly properties xPath

  expectPresentedVisual =
    verify visualError
      =<< (attempt $ expectNotPresentedVisual properties xPath)

  expectPresentedAria =
    verify ariaError
      =<< (attempt $ expectNotPresentedAria properties xPath)

  visualError =
    XPath.errorMessage (withPropertiesMessage properties visualMessage) xPath

  ariaError =
    XPath.errorMessage (withPropertiesMessage properties ariaMessage) xPath

  visualMessage =
    "Expected to find only visually presented elements"

  ariaMessage =
    "Expected no true values for \"aria-hidden\" on elements or their ancestors found"

-- | Expect the select node found with the provided XPath to have the option
-- | with the provided text selected.
-- |
-- | This only works when your select node's value is based on the value
-- | rather than the text of the option. It allows you to provide the text
-- | rather than the value which is an implementation detail.
-- |
-- | When your select node's value matches the option text use
-- | `expectPresentedWithProperties (Map.singleton "value" $ Just "the option text")`
-- | instead.
expectSelectValue ∷ ∀ eff o. String → XPath → Feature eff o Unit
expectSelectValue value xPath =
  tryRepeatedlyTo $ getOptionValue >>= expectPresentedWithValue
  where
  optionXPath = xPath ⊕ XPath.descendantString ⊕ "option[text()='" ⊕ value ⊕ "']"
  getOptionValue = flip getAttribute "value" =<< find optionXPath
  valueProperty = Map.singleton "value"
  expectPresentedWithValue value' = expectPresentedWithProperties (valueProperty value') xPath

-- Independent expectations
-- | Expects the frst provided file to be in the provided folder and to match
-- | the second provided file.
expectDownloadedTextFileToMatchFile
  ∷ ∀ eff o
  . FilePath
  → FilePath
  → FilePath
  → Feature eff o Unit
expectDownloadedTextFileToMatchFile downloadFolder downloadedFileName expectedFilePath = do
  expectedString ← appendToCwd expectedFilePath
  expectedFile ← lift $ readTextFile UTF8 $ expectedString
  let filePath = downloadFolder ⊕ "/" ⊕ downloadedFileName
  await ("Expected file " ⊕ filePath ⊕ " to be downloaded") do
    files ← lift $ readdir downloadFolder
    pure $ isJust $ elemIndex downloadedFileName files
  downloadedFile ← lift $ readTextFile UTF8 $ filePath
  if downloadedFile == expectedFile
    then lift $ unlink filePath
    else liftEff $ throw $ notEqualError filePath downloadedFile
  where
  notEqualError filePath fileContents =
    "Expected file "
      ⊕ filePath
      ⊕ "'s contents\n\n"
      ⊕ fileContents
      ⊕ "\n\n to match file "
      ⊕ expectedFilePath

-- Interaction utilities
checkedProperty ∷ Maybe String → Properties
checkedProperty checked = Map.singleton "checked" checked

updateProperty ∷ String → Maybe String → Properties → Properties
updateProperty name value =
  Map.union $ Map.singleton name value

-- XPath dependent interactions
-- | Clear the element's value
clear ∷ ∀ eff o. XPath → Feature eff o Unit
clear xPath = clearWithProperties Map.empty xPath

-- | Check the checkbox node found with the provided XPath.
check ∷ ∀ eff o. XPath → Feature eff o Unit
check xPath = checkWithProperties Map.empty xPath

-- | Uncheck the checkbox node found with the provided XPath.
uncheck ∷ ∀ eff o. XPath → Feature eff o Unit
uncheck xPath = uncheckWithProperties Map.empty xPath

-- | Push the radio button node found with the provided XPath.
pushRadioButton ∷ ∀ eff o. XPath → Feature eff o Unit
pushRadioButton xPath = pushRadioButtonWithProperties Map.empty xPath

-- | Provide a value for the text or number field found with the provided
-- | XPath.
provideFieldValue ∷ ∀ eff o. XPath → String → Feature eff o Unit
provideFieldValue =
  provideFieldValueWithProperties Map.empty

-- | Select option with the provided text from the select node found with
-- | the provided XPath.
selectFromDropdown ∷ ∀ eff o. XPath → String → Feature eff o Unit
selectFromDropdown xPath text = selectFromDropdownWithProperties Map.empty xPath text

-- | Click the node found with the provided XPath.
click ∷ ∀ eff o. XPath → Feature eff o Unit
click xPath = clickWithProperties Map.empty xPath

-- | Click all nodes found with the provided XPath.
clickAll ∷ ∀ eff o. XPath → Feature eff o Unit
clickAll xPath = clickAllWithProperties Map.empty xPath

clickNotRepeatedly ∷ ∀ eff o. XPath → Feature eff o Unit
clickNotRepeatedly xPath = clickWithPropertiesNotRepeatedly Map.empty xPath

-- | Drag node found with the first provided XPath to the node found with the
-- | second provided XPath.
dragAndDrop ∷ ∀ eff o. XPath → XPath → Feature eff o Unit
dragAndDrop fromXPath toXPath =
  dragAndDropWithProperties Map.empty fromXPath Map.empty toXPath

-- | Hover over the node found with the provided XPath.
hover ∷ ∀ eff o. XPath → Feature eff o Unit
hover xPath = hoverWithProperties Map.empty xPath

-- | Provide file input value to the file input found with the provided XPath.
provideFileInputValue ∷ ∀ eff o. XPath → FilePath → Feature eff o Unit
provideFileInputValue xPath fileName = provideFileInputValueWithProperties Map.empty xPath fileName

-- | Navigate to the URL presented in the field found with the providied Xath.
-- |
-- | URLs are often presented in text inputs so they can be copied. This
-- | function allows feature tests to access such URLs.
accessUrlFromFieldValue ∷ ∀ eff o. XPath → Feature eff o Unit
accessUrlFromFieldValue xPath = accessUrlFromFieldValueWithProperties Map.empty xPath

-- XPath and property dependent interactions
clearWithProperties
  ∷ ∀ eff o
  . Properties
  → XPath
  → Feature eff o Unit
clearWithProperties properties xPath =
  tryRepeatedlyTo $ Selenium.clearEl =<< findWithPropertiesNotRepeatedly properties xPath

checkWithProperties'
  ∷ ∀ eff o
  . (Maybe String)
  → Properties
  → XPath
  → Feature eff o Unit
checkWithProperties' checked properties xPath =
  (traverse_ clickEl) =<< findAtLeastOneWithPropertiesNotRepeatedly properties' xPath
  where
  properties' = updateProperty "checked" checked properties

-- | Check the checkbox node with the provided properties found with the
-- | provided XPath.
checkWithProperties ∷ ∀ eff o. Properties → XPath → Feature eff o Unit
checkWithProperties properties xPath =
  tryRepeatedlyTo $ checkWithProperties' Nothing properties xPath

-- | Uncheck the checkbox node with the provided properties found with the
-- | provided XPath.
uncheckWithProperties ∷ ∀ eff o. Properties → XPath → Feature eff o Unit
uncheckWithProperties properties xPath =
  tryRepeatedlyTo $ checkWithProperties' (Just "true") properties xPath

-- | Push the radio button node with the provided properties found with the
-- | provided XPath.
pushRadioButtonWithProperties ∷ ∀ eff o. Properties → XPath → Feature eff o Unit
pushRadioButtonWithProperties properties xPath =
  tryRepeatedlyTo $ checkWithProperties' Nothing properties xPath

-- | Provide a value for the text or number field with the provided properties
-- | found with the provided XPath.
provideFieldValueWithProperties
  ∷ ∀ eff o
  . Properties
  → XPath
  → String
  → Feature eff o Unit
provideFieldValueWithProperties properties xPath value =
  tryRepeatedlyTo $ provideFieldValueElement value =<< findWithPropertiesNotRepeatedly properties xPath

-- | Select option with the provided text from the select node with the provided
-- | attributes or properties found with the provided XPath.
selectFromDropdownWithProperties ∷ ∀ eff o. Properties → XPath → String → Feature eff o Unit
selectFromDropdownWithProperties properties xPath text =
  tryRepeatedlyTo $ selectFromDropdownElement text =<< findWithPropertiesNotRepeatedly properties xPath

clickWithPropertiesNotRepeatedly
  ∷ ∀ eff o
  . Properties
  → XPath
  → Feature eff o Unit
clickWithPropertiesNotRepeatedly properties =
  clickEl <=< findWithPropertiesNotRepeatedly properties


-- | Click node with the provided properties or attributes found with the
-- | provided XPath.
clickWithProperties
  ∷ ∀ eff o
  . Properties
  → XPath
  → Feature eff o Unit
clickWithProperties properties xPath =
  tryRepeatedlyTo $ clickEl =<< findWithPropertiesNotRepeatedly properties xPath

-- | Click all nodes with the provided properties or attributes found with the
-- | provided XPath.
clickAllWithProperties ∷ ∀ eff o. Properties → XPath → Feature eff o Unit
clickAllWithProperties properties xPath =
  tryRepeatedlyTo
    $ (traverse_ clickEl)
    =<< findAtLeastOneWithPropertiesNotRepeatedly properties xPath

-- | Drag node with the first provided properties or attributes found with the first
-- | provided XPath to the node with the second provided properties or attributes found with
-- | the second provided XPath.
dragAndDropWithProperties
  ∷ ∀ eff o
  . Properties
  → XPath
  → Properties
  → XPath
  → Feature eff o Unit
dragAndDropWithProperties fromProperties fromXPath toProperties toXPath =
  tryRepeatedlyTo do
    from ← findWithPropertiesNotRepeatedly fromProperties fromXPath
    fromLocation ← getCenterLocation from
    toLocation ← getCenterLocation =<< findWithPropertiesNotRepeatedly toProperties toXPath
    dragAndDropElement from $ offset fromLocation toLocation
  where
  offset from to =
    { x: to.x - from.x
    , y: to.y - from.y
    }
  getCenterLocation element = do
    location ← getLocation element
    size ← getSize element
    pure
      { x: location.x + (size.width / 2)
      , y: location.y + (size.height / 2)
      }

-- | Hover over the node with the provided properties or attributes found with
-- | the provided XPath.
hoverWithProperties ∷ ∀ eff o. Properties → XPath → Feature eff o Unit
hoverWithProperties properties xPath =
  tryRepeatedlyTo $ hoverElement =<< findWithPropertiesNotRepeatedly properties xPath

-- | Provide file input value to the file input with the provided attributes or
-- | properties found with the provided XPath. This also will find invisible
-- | file inputs.
provideFileInputValueWithProperties ∷ ∀ eff o. Properties → XPath → FilePath → Feature eff o Unit
provideFileInputValueWithProperties properties xPath filePath =
  tryRepeatedlyTo $ sendKeysEl filePath
    =<< findInvisibleWithPropertiesNotRepeatedly properties xPath

-- | Navigate to the URL presented in the field with the provided attributes or
-- | properties found with the providied Xath.
-- |
-- | URLs are often presented in text inputs so they can be copied. This
-- | function allows feature tests to access such URLs.
accessUrlFromFieldValueWithProperties ∷ ∀ eff o. Properties → XPath → Feature eff o Unit
accessUrlFromFieldValueWithProperties properties xPath =
  tryRepeatedlyTo $ get =<< maybe throwNullValueError pure =<< getValue =<< findField
  where
  findField = findWithPropertiesNotRepeatedly properties xPath
  getValue = flip getAttribute "value"
  throwNullValueError = liftEff $ throw nullValueErrorMessage
  nullValueErrorMessage =
    XPath.errorMessage (withPropertiesMessage properties rawNullValueErrorMessage) xPath
  rawNullValueErrorMessage =
    "Expected a non null value attribute or property for the element found with"

-- Independent interactions
typeString ∷ ∀ eff o. String → Feature eff o Unit
typeString string = Selenium.sequence $ FeatureSequence.keys string

pressEnter ∷ ∀ eff o. Feature eff o Unit
pressEnter = Selenium.sequence $ FeatureSequence.sendEnter

-- Element dependent interactions
clickAllElements ∷ ∀ eff o. Array Element → Feature eff o Unit
clickAllElements = traverse_ clickEl

clearElement ∷ ∀ eff o. Element → Feature eff o Unit
clearElement element =
  Selenium.clickEl element
    *> moveToEndOfString
    *> typeEnoughBackspaces
  where
  getValueLength ∷ Feature eff o (Maybe Int)
  getValueLength = map String.length <$> Selenium.getAttribute element "value"

  getTextLength ∷ Feature eff o Int
  getTextLength = map String.length $ Selenium.getText element

  getTotalLength ∷ Feature eff o Int
  getTotalLength = do
    vl ← map (fromMaybe zero) getValueLength
    tl ← getTextLength
    pure $ vl + tl

  typeEnoughBackspaces ∷ Feature eff o Unit
  typeEnoughBackspaces = typeBackspaces =<< getTotalLength

  moveToEndOfString ∷ Feature eff o Unit
  moveToEndOfString = typeRightArrow =<< getTotalLength

  typeBackspaces ∷ Int → Feature eff o Unit
  typeBackspaces = Selenium.sequence ∘ FeatureSequence.sendBackspaces

  typeRightArrow ∷ Int → Feature eff o Unit
  typeRightArrow = Selenium.sequence ∘ FeatureSequence.sendRights


hoverElement ∷ ∀ eff o. Element → Feature eff o Unit
hoverElement = Selenium.sequence ∘ Sequence.hover

provideFieldValueElement ∷ ∀ eff o. String → Element → Feature eff o Unit
provideFieldValueElement value element =
  Selenium.clickEl element *> clearElement element *> typeString value

selectFromDropdownElement ∷ ∀ eff o. String → Element → Feature eff o Unit
selectFromDropdownElement text element = do
  clickEl element
  typeString text
  pressEnter

dragAndDropElement ∷ ∀ eff o. Element → Location → Feature eff o Unit
dragAndDropElement from =
  Selenium.sequence <<< Sequence.dndToLocation from

-- Element dependent functions
elementsWithProperties
  ∷ ∀ eff o
  . Properties
  → Array Element
  → Feature eff o (Array Element)
elementsWithProperties properties
  | Map.isEmpty properties = pure
elementsWithProperties properties =
  map filterElementsPropertiesPairs
    ∘ elementsPropertiesTuples
  where
  propKeys ∷ L.List String
  propKeys = Map.keys properties

  filterElementsPropertiesPairs
    ∷ ∀ f
    . (Foldable f)
    ⇒ f (Tuple Element Properties)
    → Array Element
  filterElementsPropertiesPairs =
    foldMap (\(Tuple el ps) → guard (ps == properties) $> el)

  getPropertiesForElement
    ∷ Element
    → Feature eff o Properties
  getPropertiesForElement el =
    Map.fromFoldable
      <$> traverse (\k → Tuple k <$> ((later 0 $ getAttribute el k))) propKeys

  elementsPropertiesTuples
    ∷ ∀ t
    . (Traversable t)
    ⇒ t Element
    → Feature eff o (t (Tuple Element Properties))
  elementsPropertiesTuples =
    traverse (\el → Tuple el <$> getPropertiesForElement el)

expectNotPresentedVisualElements
  ∷ ∀ eff o
  . Properties
  → XPath
  → Array Element
  → Feature eff o Unit
expectNotPresentedVisualElements properties xPath =
  traverse_ validate
  where
  validate =
    ifTrue throwVisualError <=< isDisplayed

  throwVisualError =
    liftEff $ throw $ XPath.errorMessage message xPath

  message =
    withPropertiesMessage
      properties
      "Expected to find no visually displayed elements"

expectPresentedVisualElements
  ∷ ∀ eff o
  . Properties
  → String
  → Array Element
  → Feature eff o Unit
expectPresentedVisualElements properties xPath =
  traverse_ (\element → hoverElement element *> validate element)
  where
  validate =
    ifFalse throwVisualError <=< isDisplayed

  throwVisualError =
    liftEff $ throw $ XPath.errorMessage message xPath

  message =
    withPropertiesMessage
      properties
      "Expected to find only visually displayed elements"

-- Utilities
logCurrentScreen ∷ ∀ eff o. Feature eff o Unit
logCurrentScreen =
  saveScreenshot path *> logScreenshot path
  where
  path = "current.png"
  message = ("Screenshot taken now:\ndata:image/png;base64," <> _)
  logScreenshot p = (message <$> showImageFile p) >>= warnMsg

showImageFile ∷ ∀ eff o. FilePath → Feature eff o String
showImageFile =
  showBuffer <=< readBuffer <=< getFullPath
  where
  getFullPath = appendToCwd
  readBuffer = lift ∘ readFile
  showBuffer = liftEff ∘ toString Base64
