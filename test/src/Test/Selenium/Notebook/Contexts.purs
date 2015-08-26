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
import Control.Monad.Eff.Random (randomInt)
import Control.Monad.Eff.Class (liftEff)
import Data.Either (Either(..), either, isRight)
import Data.List (length, null, (!!))
import Data.Foreign (readArray, readString)
import Data.Maybe (Maybe(..), maybe)
import Data.Foldable (foldl)
import Data.Traversable (traverse)
import Test.Config
import Selenium.ActionSequence
import Selenium.MouseButton
import Selenium.Types 
import Test.Selenium.Common
import Test.Selenium.Monad
import Test.Selenium.Log
import Test.Selenium.Notebook.Getters
import Test.Selenium.File hiding (test)
import qualified Data.Array as A
import qualified Data.String.Regex as R
import qualified Data.String as S
import qualified Config as SDConfig

type Context = (Check Unit -> Check Unit)

reloadAndSpyXHR :: Check Unit
reloadAndSpyXHR = do
  f <- script " return window.DOUBLE_SLASH_XHRS; "
  arr <- either (const $ pure []) pure (readArray f >>= traverse readString)
  if not A.null arr
    then errorMsg $ "double slash requests were detected\nURLS:"
         <> foldl (\s a -> s <> "\n" <> a) "" arr
    else pure unit
  reload 
  spyXHR

-- | We are in notebook after `setUp`. No need to test if notebook created
-- | it's tested in `Test.Selenium.File`
setUp :: Check Unit
setUp = void do
  home
  goodMountDatabase
  enterMount
  createNotebookAndThen $ pure unit
  spyXHR

makeCell :: String -> Check Unit
makeCell sel = do
  config <- getConfig 
  newCellMenu <- newCellMenuExpanded
  if newCellMenu
    then pure unit
    else do
    trigger <- getNewCellMenuTrigger
    actions $ leftClick trigger
  exploreBtn <- waitExistentCss sel 
                "There is no explore buttton in new cell menu"
  count <- length <$> getCells
  actions $ leftClick exploreBtn
  await "Cell has not been added" $ cellAdded count
  successMsg "Ok, cell has been added"
  where 
  cellAdded old = do
    new <- length <$> getCells
    pure $ new == old + 1

makeExploreCell :: Check Unit
makeExploreCell = getConfig >>= _.newCellMenu >>> _.exploreButton >>> makeCell

makeSearchCell :: Check Unit
makeSearchCell = getConfig >>= _.newCellMenu >>> _.searchButton >>> makeCell 

deleteAllCells :: Check Unit
deleteAllCells = do
  config <- getConfig
  els <- css config.cell.trash >>= elements
  if null els
    then pure unit
    else do
    -- to check not only top
    i <- liftEff $ randomInt 0 (length els - 1)
    maybe (pure unit) go $ els !! i
  where
  go el = do
    old <- length <$> getCells
    actions $ leftClick el
    await "cell has not been deleted" do
      new <- length <$> getCells
      pure $ new == old - 1
    deleteAllCells

withCell :: Check Unit -> Context
withCell make action = do
  make
  action
  deleteAllCells

withExploreCell :: Context
withExploreCell = withCell makeExploreCell

withSearchCell :: Context
withSearchCell = withCell makeSearchCell


withFileOpened :: Context -> String -> Context
withFileOpened context file action = context do 
  config <- getConfig
  input <- getInput
  play <- getPlayButton
  actions do
    leftClick input
    sendKeys file 
    leftClick play
  await "error during opening file" do
    statusText <- getStatusText >>= innerHtml
    embed <- attempt getEmbedButton
    pure (statusText /= "" && isRight embed)
  action

withFileOpenedExplore :: String -> Context
withFileOpenedExplore = withFileOpened withExploreCell

    
withSmallZipsOpened :: Context
withSmallZipsOpened action =
  getConfig >>= _.explore >>> _.smallZips >>> flip withFileOpenedExplore action

withOlympicsOpened :: Context
withOlympicsOpened action =
  getConfig >>= _.explore >>> _.olympics >>> flip withFileOpenedExplore action

withNestedOpened :: Context
withNestedOpened action =
  getConfig >>= _.explore >>> _.nested >>> flip withFileOpenedExplore action

tableChanged :: String -> Check Boolean
tableChanged old = do
  html <- getTable >>= innerHtml
  pure $ html /= old

afterTableChanged :: forall a. Check a -> Check a
afterTableChanged action = do
  config <- getConfig
  html <- getTable >>= innerHtml
  res <- action
  await "Table content has not been changed" $ tableChanged html
  pure res

afterTableReload :: String -> Check Unit 
afterTableReload html = do
  config <- getConfig
  waitCheck (checker $ tableChanged html) (config.selenium.waitTime * 10)

withFileList :: Context -> Context
withFileList context action = context do
  config <- getConfig
  visible <- fileListVisible
  if visible
    then pure unit
    else do
    expander <- getElementByCss config.explore.expand "expand button not found"
    actions $ leftClick expander
  action

withFileListExplore :: Context
withFileListExplore = withFileList withExploreCell
