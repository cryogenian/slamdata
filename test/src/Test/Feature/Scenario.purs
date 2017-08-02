module Test.Feature.Scenario where

import Prelude

import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (message, throw)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), maybe)
import Data.String (joinWith)
import Selenium.Monad (attempt)
import Test.Feature (logCurrentScreen)
import Test.Feature.Log (debugMsg, sectionMsg, successMsg, warnMsg)
import Test.Feature.Monad (Feature)
import Test.SlamData.Feature.Monad (Connector(..))

type EpicTitle = String
type Hook eff o = Feature eff o
type ScenarioTitle = String
type KnownIssueUrl = String

type KnownIssues =
  { mongo ∷ Maybe String
  , couchbase ∷ Maybe String
  , marklogic ∷ Maybe String
  }

noIssues ∷ KnownIssues
noIssues = { mongo: Nothing, couchbase: Nothing, marklogic: Nothing }

mongoIssue ∷ String → KnownIssues
mongoIssue s = { mongo: Just s, couchbase: Nothing, marklogic: Nothing }

couchbaseIssue ∷ String → KnownIssues
couchbaseIssue s = { mongo: Nothing, couchbase: Just s, marklogic: Nothing }

marklogicIssue ∷ String → KnownIssues
marklogicIssue s = { mongo: Nothing, couchbase: Nothing, marklogic: Just s }

allIssue ∷ String → KnownIssues
allIssue s = { mongo: Just s, couchbase: Just s, marklogic: Just s }

scenario
  ∷ forall eff o
  . { epic ∷ EpicTitle
    , before ∷ Hook eff o Unit
    , after ∷ Hook eff o Unit
    , title ∷ ScenarioTitle
    , knownIssues ∷ KnownIssues
    , connector ∷ Connector
    }
  → Feature eff o Unit
  → Feature eff o Unit
scenario { epic, before, after, title, knownIssues, connector } actions =
  sectionMsg title' *> runBeforeHook before *> actions'
  where
  title' ∷ String
  title' = space <> "\n" <> epic <> ": " <> title <> "\n" <> space

  space ∷ String
  space = "-------------------------------"

  knownIssuesString ∷ String
  knownIssuesString = separate $ indent <$> knownIssuesForConnector

  knownIssuesWarning ∷ String
  knownIssuesWarning = "These known issues caused this scenario to fail:\n" <> knownIssuesString

  knownIssuesHookWarning = "These known issues caused this scenario hook to fail:\n" <> knownIssuesString

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

  knownIssuesForConnector = maybe [] pure case connector of
    Couchbase → knownIssues.couchbase
    Mongo → knownIssues.mongo
    Marklogic → knownIssues.marklogic

  actions' ∷ Feature eff o Unit
  actions' | knownIssuesForConnector == [] = do
    successMsg "-------- Starting test --------"
    attempt actions >>=
      case _ of
        Left e → logCurrentScreen *> (liftEff $ throw $ message e)
        Right _ → do
          successMsg space
          runAfterHook after
  actions' = do
    pure unit

  runBeforeHook ∷ Feature eff o Unit → Feature eff o Unit
  runBeforeHook hook | knownIssuesForConnector == [] = do
    debugMsg "--------- before test ---------"
    attempt hook >>=
      case _ of
        Left e → logCurrentScreen *> (liftEff $ throw $ message e)
        Right _ → pure unit
  runBeforeHook hook = do
    warnMsg "-------- Skipping test ----------"
    warn knownIssuesHookWarning

  runAfterHook ∷ Feature eff o Unit → Feature eff o Unit
  runAfterHook hook | knownIssuesForConnector == [] = do
    debugMsg "---------- after test ---------"
    attempt hook >>=
      case _ of
        Left e → logCurrentScreen *> (liftEff $ throw $ message e)
        Right _ → pure unit
  runAfterHook hook = do
    warnMsg "-------- Skipping test ----------"
    warn knownIssuesHookWarning
