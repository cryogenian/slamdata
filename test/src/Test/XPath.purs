module Test.XPath where

import Prelude
import Data.String (take)
import Data.Foldable (Foldable, intercalate)

followingString :: String
followingString = "/following::"

following :: String -> String -> String
following x y = x ++ followingString ++ y

inOrder :: forall m. (Foldable m) => m String -> String
inOrder = intercalate followingString

index :: String -> Int -> String
index xPath = indexString xPath <<< show

indexString :: String -> String -> String
indexString xPath index = "(" ++ xPath ++ ")[" ++ index ++ "]"

nodeWithExactText :: String -> String -> String
nodeWithExactText name text = name ++ "[text()='" ++ text ++ "']"

nodeWithText :: String -> String -> String
nodeWithText name text = name ++ "[contains(text(), '" ++ text ++ "')]"

nodeWithExactAttribute :: String -> String -> String -> String
nodeWithExactAttribute attribute name text = name ++ "[@" ++ attribute ++ "='" ++ text ++ "']"

nodeWithAttribute :: String -> String -> String -> String
nodeWithAttribute attribute name text = name ++ "[contains(@" ++ attribute ++ ", '" ++ text ++ "')]"

nodeWithExactAriaLabel :: String -> String -> String
nodeWithExactAriaLabel = nodeWithExactAttribute "aria-label"

nodeWithAriaLabel :: String -> String -> String
nodeWithAriaLabel = nodeWithAttribute "aria-label"

anyWithExactText :: String -> String
anyWithExactText = nodeWithExactText any

anyWithText :: String -> String
anyWithText = nodeWithText any

anyWithExactAriaLabel :: String -> String
anyWithExactAriaLabel = nodeWithExactAriaLabel any

anyWithAriaLabel :: String -> String
anyWithAriaLabel = nodeWithAriaLabel any

inputWithExactPlaceholder :: String -> String
inputWithExactPlaceholder = nodeWithExactAttribute "placeholder" "input"

inputWithPlaceholder :: String -> String
inputWithPlaceholder = nodeWithAttribute "placeholder" "input"

withLabel :: String -> String -> String
withLabel xPath labelXPath = xPath ++ "[@id=(" ++ labelXPath ++ "/@for)]"

tdWithThText :: String -> String -> String
tdWithThText tableXPath thText = indexString unindexedTdXPath thIndex
  where
  thXPath = tableXPath ++ "/thead/tr/th[text()='" ++ thText ++ "']"
  precedingThXPath = thXPath `precedingSibling` "th"
  thIndex = "(count(" ++ precedingThXPath ++ ") + 1)"
  unindexedTdXPath = tableXPath ++ "/tbody/tr/td"

parent :: String -> String
parent xPath = xPath ++ "/.."

thisOrItsParents :: forall a m. (Alt m) => (String -> m a) -> String -> m a
thisOrItsParents f =
  later 0 <<< orIfItFails f (orIfItFails (thisOrItsParents f <=< parent) f)

anywhere :: String -> String
anywhere xPath = if anywhered then xPath else "//" ++ xPath
  where
  anywhered = take 2 xPath == "//"

parent :: String -> String
parent xPath = xPath ++ "/.."

precedingSibling :: String -> String -> String
precedingSibling x y = x ++ "/preceding-sibling::" ++ y

any :: String
any = "*"

