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

module Test.Selenium.Notebook.Viz (test) where

import Prelude

import Control.Monad.Eff.Random (randomInt)
import Control.Monad.Eff.Class (liftEff)
import Data.List (length)

import Selenium.Types
import Selenium.Monad
import Selenium.ActionSequence hiding (sequence)

import Test.Config
import Test.Platform
import Test.Selenium.Monad
import Test.Selenium.Log
import Test.Selenium.Common
import Test.Selenium.Notebook.Getters
import Test.Selenium.Notebook.Contexts
import qualified Test.Selenium.Notebook.Common as C

import Utils (s2i)

checkNextVizCell :: Check Unit
checkNextVizCell = do
  waitNextVizCell >>= sequence <<< leftClick
  await "Viz cell has not been created" do
    ((eq 2) <<< length) <$> getCells
  successMsg "Ok, next cell viz works"

checkSetHeightWidth :: Check Unit
checkSetHeightWidth = withSmallZipsAllChart do
  widthInput <- waitVizWidthInput
  heightInput <- waitVizHeightInput

  w <- liftEff $ randomInt 100 400
  h <- liftEff $ randomInt 100 400

  sequence do
    leftClick widthInput
    sendSelectAll platform
    sendDelete
    sendKeys $ show w

    leftClick heightInput
    sendSelectAll platform
    sendDelete
    sendKeys $ show h

  await "Dimensions has not been setted" do
    dims <- getDims
    pure
       $ dims.w == pure w
      && dims.h == pure h
  successMsg "Ok, dimensions setted"

  where
  getDims = do
    canvas <- waitCanvas 
    { w: _, h: _ }
      <$> (s2i <$> getCssValue canvas "width")
      <*> (s2i <$> getCssValue canvas "height")
  
test :: Check Unit
test = do
  config <- getConfig
  sectionMsg "check next viz cell (search)"
  withSmallZipsSearchedAll $ checkNextVizCell

  sectionMsg "check next viz cell (explore)"
  withSmallZipsOpened $ checkNextVizCell

  sectionMsg "check next viz cell (query)"
  withSmallZipsQueriedAll $ checkNextVizCell

  sectionMsg "check next viz cell (query)"
  withSmallZipsQueriedAll $ checkNextVizCell

  sectionMsg "check set height/width"
  checkSetHeightWidth

  sectionMsg "check hide/show"
  withSmallZipsAllChart do
    config <- getConfig 
    cell <- getCell 1
    C.checkHideShowCell cell config.cell.vizEditor
  
