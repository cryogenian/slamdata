{-
Copyright 2015 SlamData, Inc.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
-}

-- | Contains common for all cells tests and
-- | generic tests that are specified in custom modules
-- | (i.e. Test.Selenium.Notebook.Explore)
module Test.Selenium.Notebook.Common where

--import Prelude
--
--import Config as SDConfig
--import Control.Apply ((*>))
--import Control.Monad.Eff.Random (randomInt)
--
--import Data.Either (isRight)
--import Data.Foldable (traverse_)
--import Data.Functor.Eff (liftEff)
--import Data.List (List(), length, replicateM)
--import Data.Maybe (Maybe(..))
--import Data.StrMap as SM
--import Data.Tuple (Tuple(..))
--
--import Halogen.CustomProps as Cp
--import Halogen.HTML as H
--import Halogen.HTML.Properties as P
--import Halogen.HTML.Renderer.String (renderHTML)
--
--import Selenium.ActionSequence hiding (sequence)
--import Selenium.Combinators (checker, tryToFind)
--import Selenium.Monad
--import Selenium.Types
--
--import Test.Selenium.Common
--import Test.Selenium.Expect (expect, toEq)
--import Test.Selenium.Log
--import Test.Selenium.Monad
--import Test.Selenium.Notebook.Contexts
--import Test.Selenium.Notebook.Getters
--
--checkNextCells :: SM.StrMap String -> Check Unit
--checkNextCells m = do
--  traverse_ traverseFn $ SM.toList m
--  successMsg "Ok, all next cells are found"
--  where
--  traverseFn (Tuple msg sel) = void do
--    tryToFind $ byCss sel
--
--
--checkInitial :: Check Unit -> Check Unit
--checkInitial custom = do
--  config <- getConfig
--  checkNotExists "Embed button is shown" config.cell.embedButton
--  successMsg "Ok, there is no embed button"
--  checkNotExists "Next cell menu is show" config.cell.nextCellList
--  successMsg "Ok, there is no next cell menu"
--  checkNotExists "Cell ouput label is shown" config.cell.cellOutputLabel
--  successMsg "Ok, there is no output label"
--  checkNotExists "Cell output result is shown" config.cell.cellOutputResult
--  successMsg "Ok, there is no output result"
--  checkNotExists "Failure messages button is shown" config.cell.showMessages
--  successMsg "Ok, there is no show messages button"
--  checkNotExists "Failures is shown" config.cell.failures
--  successMsg "Ok, there is no failures"
--  getElementByCss config.cell.evalLine "There is no eval line, but should"
--  successMsg "Ok, there is eval line"
--  status <- getStatus
--  successMsg "Ok, status text exists"
--  html <- getInnerHtml status
--  if html /= ""
--    then errorMsg "Status text should be empty"
--    else successMsg "Ok, status text is empty"
--  custom
--  value <- getElementByCss config.explore.input "there is no input"
--           >>= flip getAttribute "value"
--  expect value toEq $ Just ""
--  findPlayButton
--  successMsg "Ok, there is play button"
--  getRefreshButton
--  successMsg "Ok, there is refresh button"
--  checkNotExists "Hide failures button should not exist" config.cell.hideMessages
--  successMsg "Ok, there is no hide failures button"
--
--
--checkEmbedButton :: Check Unit
--checkEmbedButton = do
--  config <- getConfig
--  embed <- getEmbedButton
--  sequence $ leftClick embed
--  wait (checker $ isRight <$> attempt getModal) config.selenium.waitTime
--  modal <- getElementByCss config.modalShown "Modal should be visible"
--  box <- getElementByCss config.cell.embedBox "Embed box hidden"
--  value <- getAttribute box "value"
--  expected <- expectedValue
--  expect value toEq $ Just expected
--  sequence $ leftClick modal
--  tryRepeatedlyTo $ checkNotExists "Error: modal should be hidden" config.modalShown
--  where
--  getModal = do
--    config <- getConfig
--    getElementByCss config.cell.embedBox "Embed box hidden"
--  expectedValue = do
--    config <- getConfig
--    pure $ renderHTML $
--      H.iframe [ P.src $ url config
--               , P.width $ P.Percent 100.0
--               , P.height $ P.Percent 100.0
--               , Cp.frameBorder 0
--               ]
--  url config =
--    config.slamdataUrl <> config.notebookUrl <> "#/" <> config.mount.name <> "/" <>
--    config.database.name <> config.explore.notebookPath
--
---- | This check passes if cel errors after click on `btn`
--checkIncorrect :: Check Element -> Check Unit
--checkIncorrect btnCheck = do
--  btn <- btnCheck
--  config <- getConfig
--  sequence $ leftClick btn
--  failures <- tryToFind $ byCss config.cell.failures
--  html <- getInnerHtml failures
--  show <- tryToFind $ byCss config.cell.showMessages
--  sequence $ leftClick show
--  await "There is no difference between hidden and shown failures" do
--    shownHtml <- getInnerHtml failures
--    pure $ shownHtml /= html
--  successMsg "Ok, shown failures is defferent with hidden"
--  hide <- tryToFind $ byCss config.cell.hideMessages
--  sequence $ leftClick hide
--  await "Hidden failures are not equal with initial" do
--    hiddenHtml <- getInnerHtml failures
--    pure $ hiddenHtml == html
--  successMsg "Ok, hidden failures is equal with initial"
--
--checkTableEmpty :: Check Unit
--checkTableEmpty = do
--  config <- getConfig
--  tryRepeatedlyTo $ byCss config.cell.failures >>= loseElement
--  waitOutputLabel
--  table <- tryToFind $ byCss "table"
--  tableHtml <- getInnerHtml table
--  if tableHtml == "<thead></thead><tbody></tbody>"
--    then successMsg "Ok, table is empty"
--    else errorMsg "Table should be empty"
--
--test :: Check Unit
--test = do
--  sectionMsg "check notebook page loaded"
--  notebookLoaded
--
