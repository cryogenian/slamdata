module Test.Feature.Scenario where

import Prelude

import Control.Apply ((*>))
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (message, throw)
import Data.Either (Either(..))
import Data.String (joinWith)
import Test.Feature.Log (sectionMsg, warnMsg)
import Test.Feature (logCurrentScreen)
import Test.Feature.Monad (Feature)
import Selenium.Monad (attempt)

type EpicTitle = String
type Hook eff o = Feature eff o
type ScenarioTitle = String
type KnownIssueUrl = String

scenario
  ∷ forall eff o
  .  EpicTitle
  → Hook eff o Unit
  → Hook eff o Unit
  → ScenarioTitle
  → Array KnownIssueUrl
  → Feature eff o Unit
  → Feature eff o Unit
scenario epic before after title knownIssues actions =
  sectionMsg title' *> before *> actions'
  where
  title' ∷ String
  title' = epic <> ": " <> title

  knownIssuesString ∷ String
  knownIssuesString = separate $ indent <$> knownIssues

  knownIssuesWarning ∷ String
  knownIssuesWarning = "These known issues caused this scenario to fail:\n" <> knownIssuesString

  separate ∷ Array String → String
  separate = joinWith "\n"

  indent ∷ String → String
  indent s = "  " <> s

  warning ∷ String → String
  warning s = "Warning: " <> s

  warn ∷ String → Feature eff o Unit
  warn = warnMsg <<< warning

  unexpectedSuccess ∷ Feature eff o Unit
  unexpectedSuccess =
    warn
      $ "Ok despite known issues, if these issues are resolved please remove them\n"
      <> knownIssuesString

  actions' ∷ Feature eff o Unit
  actions' | knownIssues == [] = do
    e <- attempt actions
    case e of
      Left e' → logCurrentScreen *> (liftEff $ throw $ message e')
      Right _ → after
  actions' = do
    e <- attempt actions
    case e of
      Left e' → logCurrentScreen *> warn (message e') *> warn knownIssuesWarning *> after
      Right _ → after *> unexpectedSuccess
