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

-- | Setups, teardowns and execution contexts for notebook tests
module Test.Selenium.Notebook.Contexts where

import Prelude
--
--import Control.Monad.Eff.Random (randomInt)
--
--import Data.Either (isRight)
--import Control.Apply ((*>))
import Data.Foldable (foldl, traverse_)
import Data.Functor.Aff (liftAff)
--import Data.Functor.Eff (liftEff)
--import Data.List (List(..), length, null, (!!), catMaybes, filter)
--import Data.Maybe (maybe)
--import Data.String.Regex as R
--import Data.Traversable (traverse)
--
import Node.FS.Aff (mkdir)
--
--import Selenium.ActionSequence hiding (sequence)
--import Selenium.Combinators (checker, tryToFind)
import Selenium.Monad
--import Selenium.Types
--
--import Test.Selenium.ActionSequence (keys)
--import Test.Selenium.Common
--import Test.Selenium.File hiding (test)
--import Test.Selenium.Log
import Test.Selenium.Monad
--import Test.Selenium.Notebook.Getters
--
--type Context = (Check Unit -> Check Unit)
--
--reloadAndSpyXHR :: Check Unit
--reloadAndSpyXHR = do
--  stats <- filter filterFn <$> getXHRStats
--  if not $ null stats
--    then errorMsg $ "double slash requests were detected\nURLS:"
--         <> foldl (\s a -> s <> "\n" <> a.url) "" stats
--    else pure unit
--  refresh
--  startSpying
--  where
--  filterFn {url: url} = R.test (R.regex "//" R.noFlags) url
--
-- setUp :: Check Unit
-- setUp = void $ createTestDirs *> home *> goodMountDatabase
--
--onlyFirefox :: Context
--onlyFirefox action = do
--  isF <- isFirefox
--  if isF
--    then action
--    else warnMsg "This test runs only in FireFox"
--
--
createTestDirs :: Check Unit
createTestDirs = do
  config <- getConfig
  if not config.collectingScreenshots
    then pure unit
    else
    traverse_ (apathize <<< liftAff <<< mkdir) config.screenshot.dirs

--  config <- getConfig
--  els <- byCss config.cell.trash >>= findElements
--  if null els
--    then pure unit
--    else do
--    -- to check not only top
--    i <- liftEff $ randomInt 0 (length els - 1)
--    maybe (pure unit) go $ els !! i
--  where
--  go el = do
--    config <- getConfig
--    old <- length <$> getCellTitles
--    sequence $ leftClick el
--    els <- byCss config.cell.trash >>= findElements
--    if null els
--      then pure unit
--      else do
--      await "cell has not been deleted" do
--        new <- length <$> getCellTitles
--        pure $ new == old - one
--      deleteAllCells
--
--
--cellHasRun :: Check Boolean
--cellHasRun = do
--  statusText <- getStatus >>= getInnerHtml
--  embed <- attempt getEmbedButton
--  pure (statusText /= "" && isRight embed)
--
--fileOpened :: String -> Context
--fileOpened file action = do
--  config <- getConfig
--  input <- getInput
--  play <- findPlayButton
--  sequence do
--    leftClick input
--    sendKeys file
--    leftClick play
--  await "error during opening file" cellHasRun
--  action
--
--queryEvaluated :: String -> Context
--queryEvaluated queryStr action = do
--  config <- getConfig
--  play <- findPlayButton
--  input <- getAceInput
--  sequence do
--    leftClick input
--    keys queryStr
--    leftClick play
--  await "error during evaluating query" cellHasRun
--  action
--
--fileSearched :: String -> String -> Context
--fileSearched file query action = do
--  config <- getConfig
--  fl <- getSearchFileList
--  qu <- getSearchInput
--  play <- findPlayButton
--  sequence do
--    leftClick fl
--    sendKeys file
--    leftClick qu
--    sendKeys query
--    leftClick play
--  await "error during search file" cellHasRun
--  action
--
----withChart :: String -> Context
----withChart query action =
----  withQueryCell $ queryEvaluated query do
----    waitNextVizCell >>= sequence <<< leftClick
----    await "Viz cell has not been created" do
----      ((eq 2) <<< length) <$> getCellTitles
----    action
--
--tableChanged :: String -> Check Boolean
--tableChanged old = do
--  html <- getTable >>= getInnerHtml
--  pure $ html /= old
--
--afterTableChanged :: forall a. Check a -> Check a
--afterTableChanged action = do
--  config <- getConfig
--  html <- getTable >>= getInnerHtml
--  res <- action
--  await "Table content has not been changed" $ tableChanged html
--  pure res
--
--afterTableReload :: String -> Check Unit
--afterTableReload html = do
--  config <- getConfig
--  wait (checker $ tableChanged html) (config.selenium.waitTime * 10)

