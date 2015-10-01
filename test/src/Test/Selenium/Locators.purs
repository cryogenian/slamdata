module Test.Selenium.Locators where

import Prelude

import Test.Selenium.Monad (Check())
import Selenium.Monad (byCss)
import Selenium.Types (Locator())

inputSelector :: String -> String -> String
inputSelector inputId inputType = "input#" ++ inputId ++ "[type=\"" ++ inputType ++ "\"]"

checkableLocator :: String -> Boolean -> String -> Check Locator
checkableLocator inputType true inputId  = byCss $ inputSelector inputId inputType ++ ":checked"
checkableLocator inputType false inputId = byCss $ inputSelector inputId inputType ++ ":not(:checked)"

