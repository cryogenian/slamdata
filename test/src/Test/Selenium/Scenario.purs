module Test.Selenium.Scenario where

import Prelude

import Control.Apply ((*>))
import Control.Monad.Eff.Exception (message)
import Data.Either (Either(..))
import Data.String (joinWith)
import Test.Selenium.Log (sectionMsg, warnMsg, errorMsg)
import Test.Selenium.Monad (Check())
import Selenium.Monad (attempt)

scenario :: String -> Check Unit -> Check Unit -> String -> Array String -> Check Unit -> Check Unit
scenario epic before after title knownIssues actions = sectionMsg title' *> before *> actions'
  where
  title' :: String
  title' = epic ++ ": " ++ title

  knownIssuesString :: String
  knownIssuesString = separate $ indent <$> knownIssues

  knownIssuesWarning :: String
  knownIssuesWarning = "These known issues caused this scenario to fail:\n" ++ knownIssuesString

  separate :: Array String -> String
  separate = joinWith "\n"

  indent :: String -> String
  indent s = "  " ++ s

  warning :: String -> String
  warning s = "Warning: " ++ s

  warn :: String -> Check Unit
  warn = warnMsg <<< warning

  fail :: Check Unit
  fail = errorMsg "Ok despite known issues, if these issues are resolved please remove them"

  actions' :: Check Unit
  actions' | knownIssues == [] = actions *> after
  actions' = do
    e <- attempt actions
    case e of
      Left e -> warn (message e) *> warn knownIssuesWarning *> after
      Right _ -> after *> fail
