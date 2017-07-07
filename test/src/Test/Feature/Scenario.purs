module Test.Feature.Scenario where

import Prelude

import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (message, throw)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), maybe)
import Data.String (joinWith)
import Selenium.Monad (attempt)
import Test.Feature (logCurrentScreen)
import Test.Feature.Log (sectionMsg, warnMsg)
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
  sectionMsg title' *> runHook before *> actions'
  where
  title' ∷ String
  title' = epic <> ": " <> title

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
  actions' | knownIssuesForConnector == [] =
    attempt actions >>=
      case _ of
        Left e → logCurrentScreen *> (liftEff $ throw $ message e)
        Right _ → runHook after
  actions' =
    attempt actions >>=
      case _ of
        Left e → logCurrentScreen *> warn (message e) *> warn knownIssuesWarning *> runHook after
        Right _ → runHook after *> unexpectedSuccess

  runHook ∷ Feature eff o Unit → Feature eff o Unit
  runHook hook | knownIssuesForConnector == [] =
    attempt hook >>=
      case _ of
        Left e → logCurrentScreen *> (liftEff $ throw $ message e)
        Right _ → pure unit
  runHook hook =
    attempt hook >>=
      case _ of
        Left e → logCurrentScreen *> warn (message e) *> warn knownIssuesHookWarning
        Right _ → pure unit
