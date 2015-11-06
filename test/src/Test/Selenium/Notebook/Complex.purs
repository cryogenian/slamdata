module Test.Selenium.Notebook.Complex (test) where

import Prelude

import Control.Alt ((<|>))
import Control.Apply ((*>))
import Control.MonadPlus (guard)
import Data.Array (last)
import Data.Either (Either(..))
import Data.List (length)
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (traverse)

import Selenium.Types
import Selenium.Monad
import Selenium.ActionSequence hiding (sequence)
import Selenium.Combinators (tryToFind)

import Test.Config
import Test.Selenium.ActionSequence
import Test.Selenium.Monad
import Test.Selenium.Log
import Test.Selenium.Common
import Test.Selenium.Expect
import Test.Selenium.Notebook.Getters
import Test.Selenium.Notebook.Contexts
import Test.Selenium.Notebook.Viz (actualCanvasScreenshot)
import Test.Selenium.Notebook.Markdown.Interactions (insertMdCell)

import qualified Config as SDCfg
import qualified Data.String as S

checkMarkdownViz :: Check Unit
checkMarkdownViz = onlyFirefox do
  setUp
  checkInput
  goToViewMode
  checkInput
  tearDown
  where
  goToViewMode = do
    config <- getConfig
    waitTime $ SDCfg.autosaveTick * 5
    map (S.replace "edit" "view") getCurrentUrl >>= get
    waitCanvas
    tryRepeatedlyTo do
      input <- tryToFind $ byCss config.complex.inputSelector
      value <- getAttribute input "value"
      expect value toEq $ last config.complex.values
  setUp = do
    config <- getConfig
    insertMdCell
    mdCell <- getCell 0
    mdAce <- getAceFor mdCell
    mdPlay <- getPlayButtonFor mdCell
    sequence do
      leftClick mdAce
      sendBackspaces 200
      keys "max = #____(10)"
      leftClick mdPlay
    waitNextQueryCellFor mdCell >>= sequence <<< leftClick
    await "Error: query cell has not been added"
      $ map (eq 2 <<< length) getCells
    queryCell <- getCell 1
    queryAce <- getAceFor queryCell
    queryPlay <- getPlayButtonFor queryCell

    sequence do
      leftClick queryAce
      sendBackspaces 200
      keys config.query.parameterized
      leftClick queryPlay

    waitNextVizCellFor queryCell >>= sequence <<< leftClick
    await "Error: viz cell has not been added"
      $ map (eq 3 <<< length) getCells

  checkInput = do
    config <- getConfig
    traverse (tryRepeatedlyTo <<< checkOneInput) config.complex.values
    successMsg "Ok, value propagated"

  checkOneInput val = do
    config <- getConfig
    modifierKey <- getModifierKey
    input <- tryToFind $ byCss config.complex.inputSelector
    sequence do
      leftClick input
      selectAll modifierKey
      sendDelete
      keys val
      sendEnter

    await ("Error: incorrect chart ("<> val <> ")") do
      actualCanvasScreenshot
      screenshotsEqual $ config.screenshot.complex <> val <> ".png"

  tearDown = do
    navigateBack
    deleteAllCells

warnAboutDeletionNonDetermenism :: Context
warnAboutDeletionNonDetermenism check = do
  (check *> warnMsg successWarning) <|> warnMsg failWarning
  where
  deletionNonDeterministicIssue = "https://slamdata.atlassian.net/browse/SD-1050"
  failWarning = "Warning: This scenario failed most likely due to known issue "
                <> deletionNonDeterministicIssue
                <> "."
  successWarning =
    "Warning: scenario succeeded despite known issue "
    <> deletionNonDeterministicIssue
    <> ". If this has been resolved please remove known issue handlers."
    <> "."

checkMarkdownDefaultsInQuery :: Check Unit
checkMarkdownDefaultsInQuery = do
  insertMdCell
  mdCell <- getCell 0
  mdAce <- getAceFor mdCell
  mdPlay <- getPlayButtonFor mdCell
  sequence do
    leftClick mdAce
    sendBackspaces 200
    keys "foo = ____(AGAWAM)"
    leftClick mdPlay
  waitNextQueryCellFor mdCell >>= sequence <<< leftClick
  await "Error: query cell has not been added"
    $ map (eq 2 <<< length) getCells
  queryCell <- getCell 1
  queryAce <- getAceFor queryCell
  queryPlay <- getPlayButtonFor queryCell
  sequence do
    leftClick queryAce
    sendBackspaces 200
    keys "select * from \"/test-mount/testDb/smallZips\" where city = :foo"
    leftClick queryPlay
  await "Error: incorrect number or error in AGAWAM query" do
    agawams <- tryRepeatedlyTo $ map _.table getRowCount
    pure $ agawams == 1
  successMsg "Ok, correct row count"
  sequence do
    leftClick mdAce
    sendBackspaces 200
    keys "bar = ____(CHICOPEE)"
    leftClick mdPlay
  sequence do
    leftClick queryAce
    sendBackspaces 200
    keys "select * from \"/test-mount/testDb/smallZips\" where city = :bar"
    leftClick queryPlay
  await "Error: incorect number or error in CHICOPEE query" do
    chicopees <- tryRepeatedlyTo $ map _.table getRowCount
    pure $ chicopees == 2
  successMsg "Ok, correct row count"
  successMsg "Ok, default values propagates from markdown to query cell"


test :: Check Unit
test = do
  sectionMsg
    $  "Checking rendered markdown events propagating through\n"
    <> "query cell to viz cell"
  warnAboutDeletionNonDetermenism checkMarkdownViz
  sectionMsg
    $  "Checking default values propagated from markdown to query\n"
    <> "after markdown content has been changed"
  warnAboutDeletionNonDetermenism checkMarkdownDefaultsInQuery
