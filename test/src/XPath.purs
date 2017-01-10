module XPath where

import SlamData.Prelude
import Data.String (take, drop, length, joinWith)
import Data.Foldable (intercalate)

ancestorOrSelfString ∷ String
ancestorOrSelfString = "/ancestor-or-self::"

ancestorString ∷ String
ancestorString = "/ancestor::"

followingString ∷ String
followingString = "/following::"

descendantString ∷ String
descendantString = "/descendant::"

following ∷ String → String → String
following x y = x ⊕ followingString ⊕ y

ancestorOrSelf ∷ String → String → String
ancestorOrSelf x y = x ⊕ ancestorOrSelfString ⊕ y

ancestor ∷ String → String → String
ancestor x y = x ⊕ ancestorString ⊕ y

inOrder ∷ forall m. (Foldable m) => m String → String
inOrder = intercalate followingString

index ∷ String → Int → String
index xPath = indexString xPath ∘ show

last ∷ String → String
last xPath = indexString xPath "last()"

first ∷ String → String
first xPath = index xPath 1

indexString ∷ String → String → String
indexString xPath i = "(" ⊕ xPath ⊕ ")[" ⊕ i ⊕ "]"

withTextGreaterThan ∷ String → String
withTextGreaterThan s = "text() > '" ⊕ s ⊕ "'"

withTextLessThan ∷ String → String
withTextLessThan s = "text() > '" ⊕ s ⊕ "'"

withText ∷ String → String
withText s = "text() = '" ⊕ s ⊕ "'"

ariaLabel ∷ String → String
ariaLabel s = "@aria-label = '" ⊕ s ⊕ "'"

ariaDisabled ∷ String → String
ariaDisabled s = "@aria-disabled = '" ⊕ s ⊕ "'"

withoutText ∷ String → String
withoutText s = "text() != '" ⊕ s ⊕ "'"

attribute ∷ String → String
attribute name = "@" ⊕ name

withTextContaining ∷ String → String
withTextContaining s = "contains(text(), '" ⊕ s ⊕ "')"

predicate ∷ String → String
predicate s = "[" ⊕ s ⊕ "]"

withPredicate ∷ String → String → String
withPredicate xPath p = xPath ⊕ predicate p

nodeWithExactText ∷ String → String → String
nodeWithExactText name text = name ⊕ (predicate $ withText text)

nodeWithText ∷ String → String → String
nodeWithText name text = name ⊕ "[contains(text(), '" ⊕ text ⊕ "')]"

nodeWithExactAttribute ∷ String → String → String → String
nodeWithExactAttribute name xPath value = xPath ⊕ "[@" ⊕ name ⊕ "='" ⊕ value ⊕ "']"

nodeWithAttribute ∷ String → String → String → String
nodeWithAttribute name xPath value = xPath ⊕ "[contains(@" ⊕ name ⊕ ", '" ⊕ value ⊕ "')]"

nodeWithAttributeWithAnyValue ∷ String → String → String
nodeWithAttributeWithAnyValue name xPath = xPath ⊕ "[@" ⊕ name ⊕ "]"

nodeWithExactAriaLabel ∷ String → String → String
nodeWithExactAriaLabel = nodeWithExactAttribute "aria-label"

nodeWithAriaLabel ∷ String → String → String
nodeWithAriaLabel = nodeWithAttribute "aria-label"

nodeAtPosition ∷ Int → String → String
nodeAtPosition pos name = name ⊕ "[position()=" ⊕ show pos ⊕ "]"

anyWithAttributeWithAnyValue ∷ String → String
anyWithAttributeWithAnyValue = nodeWithAttributeWithAnyValue any

anyWithExactText ∷ String → String
anyWithExactText = nodeWithExactText any

anyWithText ∷ String → String
anyWithText = nodeWithText any

anyWithExactAriaLabel ∷ String → String
anyWithExactAriaLabel = nodeWithExactAriaLabel any

anyWithAriaLabel ∷ String → String
anyWithAriaLabel = nodeWithAriaLabel any

inputWithExactPlaceholder ∷ String → String
inputWithExactPlaceholder = nodeWithExactAttribute "placeholder" "input"

inputWithPlaceholder ∷ String → String
inputWithPlaceholder = nodeWithAttribute "placeholder" "input"

withLabel ∷ String → String → String
withLabel xPath labelXPath =
  xPath ⊕ predicate (anyOfThesePredicates [forPredicate, ancestorPredicate])
  where
  ancestorPredicate = "ancestor::" ⊕ labelXPath
  forPredicate = "@id=(//" ⊕ labelXPath ⊕ "/@for)"

withLabelWithExactText ∷ String → String → String
withLabelWithExactText xPath = withLabel xPath ∘ labelXPath
  where
  labelXPath text =
    "label[text()= '" ⊕ text ⊕ "' or descendant::*[text()= '" ⊕ text ⊕ "']]"

thWithExactText ∷ String → String
thWithExactText thText = "tr/th[text()='" ⊕ thText ⊕ "']"

tdWithTh ∷ String → String → String → String
tdWithTh tableXPath thXPath tdXPath =
  withPredicate
    (inTable tdXPath)
    (rangePredicate tdPosition thStartPosition thEndPosition)
  where
  tdPosition =
    startPosition "" "td"
  thStartPosition =
    startPosition (inTable thXPath) "th"
  thEndPosition =
    endPosition (inTable thXPath) "th"
  inTable s =
    tableXPath ⊕ descendantString ⊕ s
  rangePredicate x y z =
    "(" ⊕ x ⊕ " >= " ⊕ y ⊕ " and " ⊕ x ⊕ " < " ⊕ z ⊕ ")"
  endPosition s t =
    startPosition s t ⊕ " + " ⊕ colspan s
  colspan s =
    "count(" ⊕ s ⊕ "[not(@colspan)]) + sum(" ⊕ s ⊕ "/@colspan)"
  comment =
    "\"Comment: "
      ⊕ "Cards matching " ⊕ tdXPath ⊕ " "
      ⊕ "in columns with headers matching " ⊕ thXPath ⊕ " "
      ⊕ "in tables matching " ⊕ tableXPath ⊕ "\""
  inside s
    | s == "" = ""
    | drop (length s - 1) s == "/" = s
    | otherwise = s ⊕ "/"
  startPosition s t =
    "(sum(" ⊕ inside s ⊕ "preceding-sibling::" ⊕ t ⊕ "/@colspan)"
      ⊕ " + count(" ⊕ inside s ⊕ "preceding-sibling::" ⊕ t ⊕ "[not(@colspan)])"
      ⊕ " + 1)"

selectWithOptionsWithExactTexts ∷ Array String → String
selectWithOptionsWithExactTexts optionTexts =
  "select" `withDescendants` (map optionWithExactText optionTexts)
  where
  optionWithExactText = nodeWithExactText "option"

withDescendants ∷ String → Array String → String
withDescendants xPath =
  append xPath ∘ predicate ∘ allOfThesePredicates ∘ map descendant
  where
  descendant = append "descendant::"

parent ∷ String → String
parent xPath = xPath ⊕ "/.."

anywhere ∷ String → String
anywhere xPath = if anywhered then xPath else "//" ⊕ xPath
  where
  anywhered = take 2 xPath == "//"

precedingSibling ∷ String → String → String
precedingSibling x y = x ⊕ "/preceding-sibling::" ⊕ y

not :: String -> String
not s = "not(" <> s <> ")"

any ∷ String
any = "*"

errorMessage ∷ String → String → String
errorMessage errorPartial xPath = errorPartial ⊕ " using the xPath: " ⊕ xPath

tdWithThAndPredicate ∷ String → String → String → String
tdWithThAndPredicate tableXPath thXPath predicate' =
  tdWithTh tableXPath thXPath tdXPath
  where
  tdXPath = "tr/td" ⊕ predicate predicate'

tdWithThAndTextEq ∷ String → String → String → String
tdWithThAndTextEq tableXPath thXPath =
  tdWithThAndPredicate tableXPath thXPath ∘ withText

tdWithThAndTextContaining ∷ String → String → String → String
tdWithThAndTextContaining tableXPath thXPath =
  tdWithThAndPredicate tableXPath thXPath ∘ withTextContaining

tdWithThAndTextNotEq ∷ String → String → String → String
tdWithThAndTextNotEq tableXPath thXPath =
  tdWithThAndPredicate tableXPath thXPath ∘ withoutText

tdWithThAndTextGT ∷ String → String → String → String
tdWithThAndTextGT tableXPath thXPath =
  tdWithThAndPredicate tableXPath thXPath ∘ withTextGreaterThan

tdWithThAndTextLT ∷ String → String → String → String
tdWithThAndTextLT tableXPath thXPath =
  tdWithThAndPredicate tableXPath thXPath ∘ withTextLessThan

tdWithThAndTextEqOneOf ∷ String → String → Array String → String
tdWithThAndTextEqOneOf tableXPath thXPath =
  tdWithThAndPredicate tableXPath thXPath ∘ anyOfThesePredicates ∘ map withText

tdWithThAndTextNotEqOneOf ∷ String → String → Array String → String
tdWithThAndTextNotEqOneOf tableXPath thXPath =
  tdWithThAndPredicate tableXPath thXPath ∘ anyOfThesePredicates ∘ map withoutText

anyOfThesePredicates ∷ Array String → String
anyOfThesePredicates = joinWith " or "

allOfThesePredicates ∷ Array String → String
allOfThesePredicates = joinWith " and "

textInput ∷ String
textInput = nodeWithExactAttribute "type" "input" "text"

numberInput ∷ String
numberInput = nodeWithExactAttribute "type" "input" "number"

anyWithExactSrc ∷ String → String
anyWithExactSrc = nodeWithExactAttribute "src" "img"
