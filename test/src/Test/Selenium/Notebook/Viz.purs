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
import Data.List (length, List(..), fromList)
import Data.Maybe (isJust, isNothing, Maybe(..), maybe)
import Data.Maybe.Unsafe (fromJust)
import Data.Traversable (traverse)

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

type ChartEditors =
  { pie :: Maybe Element
  , line :: Maybe Element
  , bar :: Maybe Element
  }

barShown :: ChartEditors -> Boolean
barShown es =
  isJust es.bar && isNothing es.line && isNothing es.pie

lineShown :: ChartEditors -> Boolean
lineShown es =
  isNothing es.bar && isJust es.line && isNothing es.pie

pieShown :: ChartEditors -> Boolean
pieShown es =
  isNothing es.bar && isNothing es.line && isJust es.pie

editor :: ChartEditors -> Element
editor es =
  fromJust
  $ if barShown es
    then es.bar
    else if pieShown es
         then es.pie
         else es.line

import Utils.Log

getChartEditors :: Check ChartEditors
getChartEditors = do
  config <- getConfig

  pie <- waitExistentCss config.viz.pieEditor "There is no pie editor"
  line <- waitExistentCss config.viz.lineEditor "There is no line editor"
  bar <- waitExistentCss  config.viz.barEditor "There is no bar editor"

  pieDisplayed <- isDisplayed pie
  barDisplayed <- isDisplayed bar
  lineDisplayed <- isDisplayed line


  pure { pie: if pieDisplayed then pure pie else Nothing
       , line: if lineDisplayed then pure line else Nothing
       , bar: if barDisplayed then pure bar else Nothing
       }

getPieTypeIcon :: Check Element
getPieTypeIcon = do
  getConfig >>= _.viz >>> _.pieIcon
    >>> flip waitExistentCss "There is no pie type switcher"

getLineTypeIcon :: Check Element
getLineTypeIcon = do
  getConfig >>= _.viz >>> _.lineIcon
    >>> flip waitExistentCss "There is no line type switcher"

getBarTypeIcon :: Check Element
getBarTypeIcon = do
  getConfig >>= _.viz >>> _.barIcon
    >>> flip waitExistentCss "There is no bar type switcher"

getOptions :: Maybe Element -> Check (List Element)
getOptions el = maybe (pure Nil) (\p -> byCss "option" >>= findChildren p) $ el

type ChartSwitchers =
  { bar :: Element
  , line :: Element
  , pie :: Element
  }

getChartSwitchers :: Check ChartSwitchers
getChartSwitchers = do
  {bar: _, line: _, pie: _}
  <$> getBarTypeIcon
  <*> getLineTypeIcon
  <*> getPieTypeIcon


switchToPie :: Check Unit
switchToPie = do
  getChartSwitchers >>= sequence <<< leftClick <<< _.pie
  await "Pie switch doesn't work" do
    pieShown <$> getChartEditors

switchToLine :: Check Unit
switchToLine = do
 getChartSwitchers >>= sequence <<< leftClick <<< _.line
 await "Line switch doesn't work" do
    lineShown <$> getChartEditors

switchToBar :: Check Unit
switchToBar = do
 getChartSwitchers >>= sequence <<< leftClick <<< _.bar
 await "Bar switch doesn't work" do
    barShown <$> getChartEditors

checkSwitchers :: Check Unit
checkSwitchers = withFlatVizChart do
  editors  <- getChartEditors
  if pieShown editors
    then successMsg "Ok, initial chart type -- pie"
    else errorMsg "Incorrect initial chart type"

  switchToLine
  successMsg "pie -> line"

  switchToBar
  successMsg "line -> bar"

  switchToPie
  successMsg "bar -> pie"

  switchToBar
  successMsg "pie -> bar"

  switchToLine
  successMsg "bar -> line"

  switchToPie
  successMsg "line -> pie"

checkAlert :: Check Unit
checkAlert = withFlatVizMeasures do
  config <- getConfig
  waitExistentCss config.viz.alert "There is no alert but should"
  successMsg "ok, alert found"

getChartOptions :: Element -> Check ChartOptions
getChartOptions el = do
  config <- getConfig
  p <- { measureOne: _
       , measureTwo: _
       , category: _
       , dimension: _
       , seriesOne: _
       , seriesTwo: _
       }
    <$> optionTxts config.viz.measureOne
    <*> optionTxts config.viz.measureTwo
    <*> optionTxts config.viz.category
    <*> optionTxts config.viz.dimension
    <*> optionTxts config.viz.seriesOne
    <*> optionTxts config.viz.seriesTwo
  pure $ ChartOptions p
  where
  optionTxts sel =
    map fromList
      $ byCss sel
    >>= findChild el
    >>= getOptions
    >>= traverse getInnerHtml

checkOptions :: Check Unit
checkOptions = withFlatVizChart do
  config <- getConfig
  switchToPie
  await "incorrect pie options" do
    eq config.vizOptions.flatVizAll.pie
      <$> ((editor <$> getChartEditors) >>= getChartOptions)
  successMsg "Ok, pie chart options are correct"

  switchToLine
  await "incorrect line options" do
    eq config.vizOptions.flatVizAll.line
      <$> ((editor <$> getChartEditors) >>= getChartOptions)
  successMsg "Ok, line chart options are correct"

  switchToBar
  await "incorrect bar options" do
    eq config.vizOptions.flatVizAll.bar
      <$> ((editor <$> getChartEditors) >>= getChartOptions)
  successMsg "Ok, bar chart options are correct"

-- Don't know how to name it better.
-- This test will look if after switching options in
-- category -> series1 -> series2
-- measure1 -> measure2
-- dimension -> series1 -> series2
-- are filtered
checkOptionUniqueness :: Check Unit
checkOptionUniqueness = do
  pure unit

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


  sectionMsg "check switchers"
  checkSwitchers

  sectionMsg "alert when no available types"
  checkAlert

  sectionMsg "check availbale select options"
  checkOptions

  sectionMsg "check option filters"
  checkOptionUniqueness

