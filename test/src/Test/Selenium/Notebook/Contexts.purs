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
import Control.Bind ((>=>))
import Control.Monad.Eff.Random (randomInt)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Aff.Class (liftAff)
import Data.Either (Either(..), either, isRight)
import Data.List (List(..), length, null, (!!), catMaybes, filter)
import Data.Foreign (readArray, readString)
import Data.Maybe (Maybe(..), maybe)
import Data.Foldable (foldl, traverse_)
import Data.Traversable (traverse)
import Node.FS.Aff (mkdir)
import Test.Config
import Selenium.ActionSequence hiding (sequence)
import Selenium.MouseButton
import Selenium.Types
import Selenium.Monad
import Selenium.Combinators (checker, tryToFind)
import Test.Selenium.Common
import Test.Selenium.Monad
import Test.Selenium.Log
import Test.Selenium.Notebook.Getters
import Test.Selenium.ActionSequence (keys)
import Test.Selenium.File hiding (test)
import qualified Data.String.Regex as R
import qualified Data.String as S
import qualified Config as SDConfig

type Context = (Check Unit -> Check Unit)

reloadAndSpyXHR :: Check Unit
reloadAndSpyXHR = do
  stats <- filter filterFn <$> getXHRStats
  if not $ null stats
    then errorMsg $ "double slash requests were detected\nURLS:"
         <> foldl (\s a -> s <> "\n" <> a.url) "" stats
    else pure unit
  refresh
  startSpying
  where
  filterFn {url: url} = R.test (R.regex "//" R.noFlags) url
-- | We are in notebook after `setUp`. No need to test if notebook created
-- | it's tested in `Test.Selenium.File`
setUp :: Check Unit
setUp = void do
  createTestDirs
  home
  goodMountDatabase
  enterMount
  createNotebookAndThen $ pure unit
  startSpying


onlyFirefox :: Context
onlyFirefox action = do
  isF <- isFirefox
  if isF
    then action
    else warnMsg "This test runs only in FireFox"


createTestDirs :: Check Unit
createTestDirs = do
  config <- getConfig
  if not config.collectingScreenshots
    then pure unit
    else
    traverse_ (apathize <<< liftAff <<< mkdir) config.screenshot.dirs

makeCell :: String -> Check Unit
makeCell sel = do
  config <- getConfig
  newCellMenu <- newCellMenuExpanded
  if newCellMenu
    then pure unit
    else do
    trigger <- getNewCellMenuTrigger
    sequence $ leftClick trigger
  exploreBtn <- tryToFind $ byCss sel
  count <- length <$> getCells
  sequence $ leftClick exploreBtn
  await "Cell has not been added" $ cellAdded count
  where
  cellAdded old = do
    new <- length <$> getCells
    pure $ new == old + 1

makeExploreCell :: Check Unit
makeExploreCell = getConfig >>= _.newCellMenu >>> _.exploreButton >>> makeCell

makeSearchCell :: Check Unit
makeSearchCell = getConfig >>= _.newCellMenu >>> _.searchButton >>> makeCell

makeQueryCell :: Check Unit
makeQueryCell = getConfig >>= _.newCellMenu >>> _.queryButton >>> makeCell

deleteAllCells :: Check Unit
deleteAllCells = do
  config <- getConfig
  els <- byCss config.cell.trash >>= findElements
  if null els
    then pure unit
    else do
    -- to check not only top
    i <- liftEff $ randomInt 0 (length els - 1)
    maybe (pure unit) go $ els !! i
  where
  go el = do
    config <- getConfig
    old <- length <$> getCells
    sequence $ leftClick el
    els <- byCss config.cell.trash >>= findElements
    if null els
      then pure unit
      else do
      await "cell has not been deleted" do
        new <- length <$> getCells
        pure $ new == old - one
      deleteAllCells

deleteCells :: Check (List Element) -> Check Unit
deleteCells cellsCheck = do
  config <- getConfig
  cells <- cellsCheck
  els <- catMaybes <$> (traverse traverseFn cells)
  case els of
    Nil -> successMsg "Ok, cells deleted"
    Cons el _ -> do
      sequence $ leftClick el
      await "Cell has not been deleted (deleteCell)" do
        count <- length <$> cellsCheck
        pure $ length cells == count + one
      deleteCells cellsCheck
  where
  traverseFn cell = do
    config <- getConfig
    byCss config.cell.trash >>= findChild cell


withCell :: Check Unit -> Context
withCell make action = do
  make
  action
  deleteAllCells

withExploreCell :: Context
withExploreCell = withCell makeExploreCell

withSearchCell :: Context
withSearchCell = withCell makeSearchCell

withQueryCell :: Context
withQueryCell = withCell makeQueryCell

cellHasRun :: Check Boolean
cellHasRun = do
  statusText <- getStatus >>= getInnerHtml
  embed <- attempt getEmbedButton
  pure (statusText /= "" && isRight embed)

fileOpened :: String -> Context
fileOpened file action = do
  config <- getConfig
  input <- getInput
  play <- findPlayButton
  sequence do
    leftClick input
    sendKeys file
    leftClick play
  await "error during opening file" cellHasRun
  action

queryEvaluated :: String -> Context
queryEvaluated queryStr action = do
  config <- getConfig
  play <- findPlayButton
  input <- getAceInput
  sequence do
    leftClick input
    keys queryStr
    leftClick play
  await "error during evaluating query" cellHasRun
  action

withFileOpened :: Context -> String -> Context
withFileOpened context file action = context $ fileOpened file action


withFileOpenedExplore :: String -> Context
withFileOpenedExplore = withFileOpened withExploreCell


fileSearched :: String -> String -> Context
fileSearched file query action = do
  config <- getConfig
  fl <- getSearchFileList
  qu <- getSearchInput
  play <- findPlayButton
  sequence do
    leftClick fl
    sendKeys file
    leftClick qu
    sendKeys query
    leftClick play
  await "error during search file" cellHasRun
  action



withFileSearched :: String -> String -> Context
withFileSearched file query action = withSearchCell $ fileSearched file query action

withSmallZipsSearchedAll :: Context
withSmallZipsSearchedAll action = do
  config <- getConfig
  withFileSearched config.explore.smallZips config.searchCell.allQuery action

withSmallZipsOpened :: Context
withSmallZipsOpened action =
  getConfig >>= _.explore >>> _.smallZips >>> flip withFileOpenedExplore action


withSmallZipsQueriedAll :: Context
withSmallZipsQueriedAll action = withQueryCell do
  config <- getConfig
  queryEvaluated config.query.smallZipsAll action

withOlympicsOpened :: Context
withOlympicsOpened action =
  getConfig >>= _.explore >>> _.olympics >>> flip withFileOpenedExplore action

withNestedOpened :: Context
withNestedOpened action =
  getConfig >>= _.explore >>> _.nested >>> flip withFileOpenedExplore action



withChart :: String -> Context
withChart query action =
  withQueryCell $ queryEvaluated query do
    waitNextVizCell >>= sequence <<< leftClick
    await "Viz cell has not been created" do
      ((eq 2) <<< length) <$> getCells
    action

withSmallZipsAllChart :: Context
withSmallZipsAllChart action =
  getConfig >>= _.query >>> _.smallZipsAll >>> flip withChart action


withFlatVizChart :: Context
withFlatVizChart action =
  getConfig >>= _.query >>> _.flatVizAll >>> flip withChart action

withFlatVizMeasures :: Context
withFlatVizMeasures action =
  getConfig >>= _.query >>> _.flatVizMeasures >>> flip withChart action

withFlatVizOneOption :: Context
withFlatVizOneOption action =
  getConfig >>= _.query >>> _.flatVizOneOption >>> flip withChart action

tableChanged :: String -> Check Boolean
tableChanged old = do
  html <- getTable >>= getInnerHtml
  pure $ html /= old

afterTableChanged :: forall a. Check a -> Check a
afterTableChanged action = do
  config <- getConfig
  html <- getTable >>= getInnerHtml
  res <- action
  await "Table content has not been changed" $ tableChanged html
  pure res

afterTableReload :: String -> Check Unit
afterTableReload html = do
  config <- getConfig
  wait (checker $ tableChanged html) (config.selenium.waitTime * 10)

withFileList :: Context -> Context
withFileList context action = context do
  config <- getConfig
  visible <- fileListVisible
  if visible
    then pure unit
    else do
    expander <- getElementByCss config.explore.expand "expand button not found"
    sequence $ leftClick expander
  action

withFileListExplore :: Context
withFileListExplore = withFileList withExploreCell
