module Test.XPath where

-- These are simple string based functions rather than parser combinators or typesafe construtors.

import Prelude
import Data.String (take)
import Data.Foldable (Foldable, intercalate)

followingString :: String
followingString = "/following::"

following :: String -> String -> String
following x y = x ++ followingString ++ y

inOrder :: forall m. (Foldable m) => m String -> String
inOrder = intercalate followingString

-- In general usage anywhere is applied before index.
index :: String -> Int -> String
index xPath indexInt = "(" ++ xPath ++ ")[" ++ show indexInt ++ "]"

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
inputWithExactPlaceholder placeholder = "input[@placeholder='" ++ placeholder ++ "']"

inputWithPlaceholder :: String -> String
inputWithPlaceholder placeholder = "input[contains(@placeholder, '" ++ placeholder ++ "')]"

-- In general use only apply this just before using the XPath. This makes XPaths more composable.
-- E.g. findAllByXPath $ anywhere xPath
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

