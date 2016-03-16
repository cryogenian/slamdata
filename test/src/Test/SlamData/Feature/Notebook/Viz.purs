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

module Test.SlamData.Feature.Notebook.Viz where

--import Prelude
--
--import Control.Monad.Eff.Random (randomInt)
--
--import Data.Foldable (foldl, traverse_)
--import Data.Function (on)
--import Data.Functor.Eff (liftEff)
--import Data.Int as Int
--import Data.List (length, List(..), filter)
--import Data.Maybe (Maybe())
--import Data.Monoid (mempty)
--import Data.Set as S
--import Data.Traversable (traverse)
--import Data.Tuple (Tuple(..))
--
--import Selenium.ActionSequence hiding (sequence)
--import Selenium.Monad
--import Selenium.Types
--
--import Test.SlamData.Feature.ActionSequence (selectAll, sendDelete, sendEnter)
--import Test.SlamData.Feature.Common
--import Test.SlamData.Feature.Log
--import Test.SlamData.Feature.Monad
--import Test.SlamData.Feature.Notebook.Common as C
--import Test.SlamData.Feature.Notebook.Contexts
--import Test.SlamData.Feature.Notebook.Getters
--import Config as SDConfig
--
--import Utils.Random

--checkNextVizCell :: SlamFeature Unit
--checkNextVizCell = do
--  waitNextVizCell >>= sequence <<< leftClick
--  await "Viz cell has not been created"
--    $ map (eq 2 <<< length) getCellTitles
--  successMsg "Ok, next cell viz works"
--
--checkSetHeightWidth :: SlamFeature Unit
--checkSetHeightWidth = withSmallZipsAllChart do
--  widthInput <- waitVizWidthInput
--  heightInput <- waitVizHeightInput
--
--  w <- liftEff $ randomInt 100 400
--  h <- liftEff $ randomInt 100 400
--
--  modifierKey <- getModifierKey
--
--  sequence do
--    leftClick widthInput
--    selectAll modifierKey
--    sendDelete
--    sendKeys $ show w
--
--    leftClick heightInput
--    selectAll modifierKey
--    sendDelete
--    sendKeys $ show h
--
--  await "Dimensions has not been setted" do
--    dims <- getDims
--    pure
--       $ dims.w == pure w
--      && dims.h == pure h
--  successMsg "Ok, dimensions setted"
--
--  where
--  getDims = do
--    canvas <- waitCanvas
--    { w: _, h: _ }
--      <$> (stringToInt <$> getCssValue canvas "width")
--      <*> (stringToInt <$> getCssValue canvas "height")
--
--
--
--switchToPie :: SlamFeature Unit
--switchToPie = do
--  tryRepeatedlyTo $ getChartSwitchers >>= sequence <<< leftClick <<< _.pie
--  await "Pie switch doesn't work" pieShown
--
--
--switchToLine :: SlamFeature Unit
--switchToLine = do
-- tryRepeatedlyTo $ getChartSwitchers >>= sequence <<< leftClick <<< _.line
-- await "Line switch doesn't work" lineShown
--
--
--switchToBar :: SlamFeature Unit
--switchToBar = do
-- tryRepeatedlyTo $ getChartSwitchers >>= sequence <<< leftClick <<< _.bar
-- await "Bar switch doesn't work" barShown
--
--
--checkSwitchers :: SlamFeature Unit
--checkSwitchers = withFlatVizChart do
--  pieDisplayed <- pieShown
--  if pieDisplayed
--    then successMsg "Ok, initial chart type -- pie"
--    else errorMsg "Incorrect initial chart type"
--
--  switchToLine
--  successMsg "pie -> line"
--
--  switchToBar
--  successMsg "line -> bar"
--
--  switchToPie
--  successMsg "bar -> pie"
--
--  switchToBar
--  successMsg "pie -> bar"
--
--  switchToLine
--  successMsg "bar -> line"
--
--  switchToPie
--  successMsg "line -> pie"
--
--checkAlert :: SlamFeature Unit
--checkAlert = withFlatVizMeasures do
--  config <- getConfig
--  tryRepeatedlyTo $ byCss config.vizSelectors.alert >>= findExact
--  successMsg "ok, alert found"
--
--checkOptions :: SlamFeature Unit
--checkOptions = withFlatVizChart do
--  config <- getConfig
--  switchToPie
--  await "incorrect pie options"
--    $ map (eq config.vizOptions.flatVizAll.pie) getCurrentChartOptions
--  successMsg "Ok, pie chart options are correct"
--
--  switchToLine
--  await "incorrect line options"
--    $ map (eq config.vizOptions.flatVizAll.line) getCurrentChartOptions
--  successMsg "Ok, line chart options are correct"
--
--  switchToBar
--  await "incorrect bar options"
--    $ map (eq config.vizOptions.flatVizAll.bar) getCurrentChartOptions
--
--  successMsg "Ok, bar chart options are correct"
--
---- Don't know how to name it better.
---- This test will look if after switching options in
---- category -> series1 -> series2
---- measure1 -> measure2
---- dimension -> series1 -> series2
---- are filtered
--checkOptionUniqueness :: SlamFeature Unit
--checkOptionUniqueness = withFlatVizChart do
--  config <- getConfig
--
--  sectionMsg "check pie uniqueness"
--  switchToPie
--  await "incorrect pie options"
--    $ map (eq config.vizOptions.flatVizAll.pie) getCurrentChartOptions
--  checkCategoryUniqueness getCategoryInput getSeriesOneInput getSeriesTwoInput
--
--  sectionMsg "check bar uniqueness"
--  switchToBar
--  await "incorrect bar options"
--    $ map (eq config.vizOptions.flatVizAll.bar) getCurrentChartOptions
--
--  checkCategoryUniqueness getCategoryInput getSeriesOneInput getSeriesTwoInput
--
--  sectionMsg "check line uniqueness"
--  switchToLine
--  await "incorrec line options"
--    $ map (eq config.vizOptions.flatVizAll.line) getCurrentChartOptions
--
--  checkCategoryUniqueness getDimensionInput getSeriesOneInput getSeriesTwoInput
--  checkMeasureUniqueness
--
--checkMeasureUniqueness :: SlamFeature Unit
--checkMeasureUniqueness = do
--  config <- getConfig
--  mOne <- getMeasureOneInput
--  mTwo <- getMeasureTwoInput
--
--  toSelectOne <- filterAndGetValue mOne mTwo "measures"
--  toSelectTwo <- getRandomOption mTwo
--
--  fillSelect mTwo toSelectTwo
--  fillSelect mOne toSelectTwo
--  await "Options has not been dropped"
--    $ map (/= toSelectTwo) $ getSelectValue mTwo
--
--  fillSelect mTwo toSelectTwo
--  fillSelect mOne config.vizOptions.clearSelection
--  await "Option should be disabled"
--    $ map not $ isEnabled mTwo
--  successMsg "Ok, measure uniqueness"
--
--checkCategoryUniqueness :: SlamFeature Element -> SlamFeature Element -> SlamFeature Element -> SlamFeature Unit
--checkCategoryUniqueness catM serOneM serTwoM = do
--  config <- getConfig
--  cat <- catM
--  seriesOne <- serOneM
--  seriesTwo <- serTwoM
--
--  toSelectCat <- filterAndGetValue cat seriesOne "category"
--  toSelectSerOne <- filterAndGetValue seriesOne seriesTwo "series one"
--
--  toSelectSerTwo <- getRandomOption seriesTwo
--  fillSelect seriesTwo toSelectSerTwo
--  fillSelect seriesOne toSelectSerTwo
--  await "Options has not been dropped (one -> two)"
--    $ map (/= toSelectSerTwo) $ getSelectValue seriesTwo
--  successMsg "Ok, select value dropped"
--
--  fillSelect cat toSelectSerTwo
--  await "Options has not been dropped (root -> one)"
--    $ map (/= toSelectSerOne) $ getSelectValue seriesOne
--  successMsg "Ok, dependent selects values are dropped"
--
--  fillSelect cat toSelectCat
--  fillSelect seriesOne toSelectSerOne
--  fillSelect seriesTwo toSelectSerTwo
--  fillSelect cat toSelectSerTwo
--  await "Optoins has not been dropped (root -> two)"
--    $ map (/= toSelectSerTwo) $ getSelectValue seriesTwo
--
--  fillSelect cat toSelectCat
--  fillSelect seriesOne toSelectSerOne
--  fillSelect seriesTwo toSelectSerTwo
--  fillSelect seriesOne config.vizOptions.clearSelection
--  await "Select should be disabled (series two)"
--    $ map not $ isEnabled seriesTwo
--
--  fillSelect seriesOne toSelectSerOne
--  fillSelect seriesTwo toSelectSerTwo
--  fillSelect cat config.vizOptions.clearSelection
--  await "Select should be disabled (series one, series two)" do
--    one <- isEnabled seriesOne
--    two <- isEnabled seriesTwo
--    pure $ not one && not two
--  successMsg "Ok, dependent selects are disabled"
--
--  successMsg "OK, category uniqueness"
--
--filterAndGetValue :: Element -> Element -> String -> SlamFeature String
--filterAndGetValue parent child msg = do
--  toSelect <- getRandomOption parent
--  fillSelect parent toSelect
--  await ("Options are not filtered (" <> msg <> ")") do
--    parentOptions <- optionTxts parent
--    childOptions <- optionTxts child
--    pure $ on eq S.fromList parentOptions (Cons toSelect childOptions)
--  successMsg $ "Ok, select filtering works (" <> msg <> ")"
--  pure toSelect
--
--getSelectValue :: Element -> SlamFeature String
--getSelectValue el = do
--  val <- getAttribute el "value"
--  getOptions (pure el) >>= foldl (foldFn val) (pure mempty)
--  where
--  foldFn :: Maybe String -> SlamFeature String -> Element -> SlamFeature String
--  foldFn selectedVal mStr el = do
--    val <- getAttribute el "value"
--    if val == selectedVal
--      then getInnerHtml el
--      else mStr
--
--getRandomOption :: Element -> SlamFeature String
--getRandomOption el = do
--  config <- getConfig
--  await "there is no options"
--    $ map ((> 2) <<< length) $ getOptions $ pure el
--  getOptions (pure el)
--    >>= traverse getInnerHtml
--    >>= randomInM <<< filter (not <<< eq config.vizOptions.clearSelection)
--    >>= \x -> if x == mempty
--              then errorMsg "empty option list"
--              else pure x
--fillSelect :: Element -> String -> SlamFeature Unit
--fillSelect el txt =
--  sequence do
--    leftClick el
--    sendKeys txt
--    sendEnter
--
--fillSelectRandom :: Element -> SlamFeature Unit
--fillSelectRandom el = do
--  opt <- getRandomOption el
--  fillSelect el opt
--
--optionTxts :: Element -> SlamFeature (List String)
--optionTxts el = getOptions (pure el) >>= traverse getInnerHtml
--
--checkOptionAutoSelect :: SlamFeature Unit
--checkOptionAutoSelect = do
--  config <- getConfig
--  withFlatVizOneOption do
--    switchToPie
--    waitPieOrBarFullyDisabled "bar"
--    pOpts <- getCurrentChartOptions
--    assertBoolean "Error: incorrect pie options"
--      $ pOpts == config.vizOptions.flatVizOneOption.pie
--
--    switchToBar
--    waitPieOrBarFullyDisabled "pie"
--    bOpts <- getCurrentChartOptions
--    assertBoolean "Error: incorrect bar options"
--      $ bOpts == config.vizOptions.flatVizOneOption.bar
--
--    switchToLine
--    waitForDisabled "line"
--      $ [ getSeriesOneInput
--        , getSeriesTwoInput
--        , getDimensionInput
--        , getMeasureOneInput
--        , getMeasureTwoInput
--        ]
--    lOpts <- getCurrentChartOptions
--    assertBoolean "Error: incorrect line options"
--      $ lOpts == config.vizOptions.flatVizOneOption.line
--  successMsg "Ok, options are correct when there is one series and one measure"
--  where
--  waitForDisabled msg getters =
--    await ("Error: Incorrect states of selects, they must be disabled (" <> msg <> ")")
--    $ foldl (\b x -> conj <$> b <*> (map not $ x >>= isEnabled)) (pure true)
--    $ getters
--  waitPieOrBarFullyDisabled msg =
--    waitForDisabled msg
--    $ [ getSeriesOneInput
--      , getSeriesTwoInput
--      , getMeasureOneInput
--      , getCategoryInput
--      ]
--
--checkOptionSave :: SlamFeature Unit
--checkOptionSave = do
--  checkOptionSaveBarOrPie switchToBar "bar"
--  checkOptionSaveBarOrPie switchToPie "pie"
--  checkOptionSaveLine
--
--checkOptionSaveLine :: SlamFeature Unit
--checkOptionSaveLine = withFlatVizChart do
--  switchToLine
--  dimension <- getDimensionInput
--  measureOne <- getMeasureOneInput
--  measureTwo <- getMeasureTwoInput
--  seriesOne <- getSeriesOneInput
--  seriesTwo <- getSeriesTwoInput
--
--  valueDimension <- filterAndGetValue dimension seriesOne "dimension"
--  valueSeriesOne <- filterAndGetValue seriesOne seriesTwo "series"
--  valueSeriesTwo <- getRandomOption seriesTwo
--  fillSelect seriesTwo valueSeriesTwo
--  valueMeasureOne <- filterAndGetValue measureOne measureTwo "measure"
--  valueMeasureTwo <- getRandomOption measureTwo
--  fillSelect measureTwo valueMeasureTwo
--
--  let assertion = do
--        d <- assertSelected getDimensionInput valueDimension
--        m1 <- assertSelected getMeasureOneInput valueMeasureOne
--        m2 <- assertSelected getMeasureTwoInput valueMeasureTwo
--        s1 <- assertSelected getSeriesOneInput valueSeriesOne
--        s2 <- assertSelected getSeriesTwoInput valueSeriesTwo
--        pure $ d && m1 && m2 && s1 && s2
--
--  switchToPie
--  switchToLine
--  await "Error: line options have not been saved" assertion
--
--  switchToBar
--  switchToLine
--  await "Error: line options have not been saved" assertion
--
--  waitTime $ SDConfig.autosaveTick * 2
--  reloadAndSpyXHR
--
--  switchToLine
--  await "Error: line options saved but not permanently" assertion
--  successMsg "Ok, line options saved"
--
--
--
--assertSelected :: SlamFeature Element -> String -> SlamFeature Boolean
--assertSelected getSelect value =
--  map (eq value) $ getSelect >>= getSelectValue
--
--checkOptionSaveBarOrPie :: SlamFeature Unit -> String -> SlamFeature Unit
--checkOptionSaveBarOrPie switcher msg = withFlatVizChart do
--  switcher
--  category <- getCategoryInput
--  measure <- getMeasureOneInput
--  seriesOne <- getSeriesOneInput
--  seriesTwo <- getSeriesTwoInput
--
--  valueCategory <- filterAndGetValue category seriesOne "category"
--  valueSeriesOne <- filterAndGetValue seriesOne seriesTwo "series"
--  valueSeriesTwo <- getRandomOption seriesTwo
--  valueMeasure <- getRandomOption measure
--
--  fillSelect seriesTwo valueSeriesTwo
--  fillSelect measure valueMeasure
--
--  let assertion = do
--        c <- assertSelected getCategoryInput valueCategory
--        m <- assertSelected getMeasureOneInput valueMeasure
--        s1 <- assertSelected getSeriesOneInput valueSeriesOne
--        s2 <- assertSelected getSeriesTwoInput valueSeriesTwo
--        pure $ c && m && s1 && s2
--
--  switchToBar
--  switcher
--  await ("Error: Hasn't been saved (" <> msg <> ")") assertion
--
--  switchToLine
--  switcher
--  await ("Error: Hasn't been saved (" <> msg <> ")") assertion
--
--  switchToPie
--  switcher
--  await ("Error: Hasn't been saved (" <> msg <> ")") assertion
--
--  successMsg $ "Ok, chart options have been saved (" <> msg <> ")"
--
--  waitTime $ SDConfig.autosaveTick * 2
--  reloadAndSpyXHR
--
--  switcher
--  getCurrentEditor
--  await "Error: saved, but not permanently" assertion
--  successMsg $ "Ok, chart options have been saved permanently (" <> msg <> ")"
--
--checkTrashingVizCell :: SlamFeature Unit
--checkTrashingVizCell = withFlatVizChart do
--  config <- getConfig
--  vizCell <- getCellTitle 1
--  byCss config.cell.trash
--    >>= childExact vizCell
--    >>= sequence <<< leftClick
--
--  checkThatOneCellRemaining
--  waitTime $ SDConfig.autosaveTick * 2
--  reloadAndSpyXHR
--  checkThatOneCellRemaining
--  successMsg "Ok, trash button works"
--  where
--  checkThatOneCellRemaining =
--    await "Error: cell has not been deleted" do
--      map (eq one <<< length) getCellTitles
--
--checkRunRefreshEmbed :: SlamFeature Unit
--checkRunRefreshEmbed = onlyFirefox $ withFlatVizChart do
--  switchToPie
--  category <- getCategoryInput
--  measure <- getMeasureOneInput
--  seriesOne <- getSeriesOneInput
--  seriesTwo <- getSeriesTwoInput
--
--  valueCategory <- filterAndGetValue category seriesOne "category"
--  valueSeriesOne <- filterAndGetValue seriesOne seriesTwo "series"
--  valueSeriesTwo <- getRandomOption seriesTwo
--  valueMeasure <- getRandomOption measure
--
--  fillSelect seriesTwo valueSeriesTwo
--  fillSelect measure valueMeasure
--
--  saveInitialScreenshot
--
--  findPlayButton >>= sequence <<< leftClick
--
--
--  await "Error: chart has not been rendered (pie)"
--    $ checkScreenshotsDiffer
--  C.checkEmbedButton
--
--  saveInitialScreenshot
--  switchToBar
--
--  categoryBar <- getCategoryInput
--  measureBar <- getMeasureOneInput
--  valueCategoryBar <- getRandomOption categoryBar
--  valueMeasureBar <- getRandomOption measureBar
--  fillSelect categoryBar valueCategoryBar
--  fillSelect measureBar valueMeasureBar
--
--  await "Error: chart has not been rendered (bar)"
--    $ checkScreenshotsDiffer
--  C.checkEmbedButton
--
--  switchToLine
--
--  saveInitialScreenshot
--  dimension <- getDimensionInput
--  measureLine <- getMeasureOneInput
--  valueDimension <- getRandomOption categoryBar
--  valueMeasureLine <- getRandomOption measureBar
--  fillSelect dimension valueDimension
--  fillSelect measureLine valueMeasureLine
--
--  await "Error: chart has not been rendered (line)"
--    $ checkScreenshotsDiffer
--  C.checkEmbedButton
--
--  successMsg "Ok, embed/run works"
--
--checkScreenshotsDiffer :: SlamFeature Boolean
--checkScreenshotsDiffer = do
--  config <- getConfig
--  if config.collectingScreenshots
--    then pure true
--    else do
--    config <- getConfig
--    saveActualScreenshot
--    map not $ screenshotsEqual config.screenshot.initial
--
--checkElementScreensDiffer :: SlamFeature Element -> SlamFeature Boolean
--checkElementScreensDiffer checkEl = do
--  config <- getConfig
--  checkEl >>= actualElementScreenshot
--  map not $ screenshotsEqual config.screenshot.initial
--
--checkAggregation :: SlamFeature Unit
--checkAggregation = onlyFirefox $ withFlatVizOneOption do
--  aggConfig <- map (_.screenshot >>> _.aggregation) getConfig
--
--  switchToLine
--  traverse_ (checkOneAggregation aggConfig.line "line") tpls
--
--  switchToBar
--  traverse_ (checkOneAggregation aggConfig.bar "bar") tpls
--
--  switchToPie
--  traverse_ (checkOneAggregation aggConfig.pie "pie") tpls
--  successMsg "Ok, aggregation works"
--  where
--  tpls :: Array (Tuple (Check Unit) String)
--  tpls = [ Tuple setProductAggregation "product"
--         , Tuple setSumAggregation "sum"
--         , Tuple setMinAggregation "min"
--         , Tuple setMaxAggregation "max"
--         , Tuple setAverageAggregation "average"
--         ]
--
--  checkOneAggregation :: String -> String -> Tuple (Check Unit) String -> SlamFeature Unit
--  checkOneAggregation pathToExpected chartMark (Tuple setAgg aggMark) = do
--    setAgg
--    await ("Error: aggregation (" <> chartMark <> " - " <> aggMark <> ")") do
--      actualCanvasScreenshot
--      screenshotsEqual $ pathToExpected <> aggMark <> ".png"
--
--setAggregation :: String -> SlamFeature Unit
--setAggregation val = do
--  sel <- getAggregationSelect
--  option <- getAggregationOption sel val
--  sequence do
--    leftClick sel
--    leftClick option
--    sendEnter
--
--setSumAggregation :: SlamFeature Unit
--setSumAggregation =
--  getConfig >>= _.vizAggregation >>> _.sum >>> setAggregation
--
--setProductAggregation :: SlamFeature Unit
--setProductAggregation =
--  getConfig >>= _.vizAggregation >>> _.product >>> setAggregation
--
--setMinAggregation :: SlamFeature Unit
--setMinAggregation =
--  getConfig >>= _.vizAggregation >>> _.min >>> setAggregation
--
--setMaxAggregation :: SlamFeature Unit
--setMaxAggregation =
--  getConfig >>= _.vizAggregation >>> _.max >>> setAggregation
--
--setAverageAggregation :: SlamFeature Unit
--setAverageAggregation =
--  getConfig >>= _.vizAggregation >>> _.average >>> setAggregation
--
--
--checkCharts :: SlamFeature Unit
--checkCharts = onlyFirefox $ withFlatVizChart do
--  config <- map (_.screenshot >>> _.charts) getConfig
--  toSet <- map (_.vizOptions >>> _.set) getConfig
--  switchToPie
--  barOrPie "pie" config.pie
--
--  switchToBar
--  barOrPie "bar" config.bar
--
--  switchToLine
--  dimension <- getDimensionInput
--  measureOne <- getMeasureOneInput
--  measureTwo <- getMeasureTwoInput
--  seriesOne <- getSeriesOneInput
--  seriesTwo <- getSeriesTwoInput
--  fillSelect dimension toSet.dimension
--  fillSelect measureOne toSet.measureOne
--  await "Error: incorrect chart (line - category)" do
--    actualCanvasScreenshot
--    screenshotsEqual $ config.line <> "category.png"
--  fillSelect measureTwo toSet.measureTwo
--  await "Error: incorrect chart (line - measure)" do
--    actualCanvasScreenshot
--    screenshotsEqual $ config.line <> "measure.png"
--  fillSelect seriesOne toSet.seriesOne
--  await "Error: incorrect chart (line - series1)" do
--    actualCanvasScreenshot
--    screenshotsEqual $ config.line <> "series1.png"
--  fillSelect seriesTwo toSet.seriesTwo
--  await "Error: incorrect chart (line - series2)" do
--    actualCanvasScreenshot
--    screenshotsEqual $ config.line <> "series2.png"
--
--  successMsg "Ok, charts are correct"
--  where
--  barOrPie :: String -> String -> SlamFeature Unit
--  barOrPie label basePath = do
--    toSet <- map (_.vizOptions >>> _.set) getConfig
--    category <- getCategoryInput
--    measure <- getMeasureOneInput
--    seriesOne <- getSeriesOneInput
--    seriesTwo <- getSeriesTwoInput
--
--    fillSelect category toSet.category
--    fillSelect measure toSet.measureOne
--    await ("Error: incorrect chart (" <> label <> ") - category)") do
--      actualCanvasScreenshot
--      screenshotsEqual $ basePath <> "category.png"
--
--    fillSelect seriesOne toSet.seriesOne
--    await ("Error: incorrect chart (" <> label <> ") - series1)") do
--      actualCanvasScreenshot
--      screenshotsEqual $ basePath <> "series1.png"
--
--    fillSelect seriesTwo toSet.seriesTwo
--    await ("Error: incorrect chart (" <> label <> ") - series2)") do
--      actualCanvasScreenshot
--      screenshotsEqual $ basePath <> "series2.png"
--

--actualCanvasScreenshot :: SlamFeature Unit
--actualCanvasScreenshot = do
--  config <- getConfig
--  if config.collectingScreenshots
--    then do
--    waitTime 5000
--    waitCanvas >>= flip elementScreenshot config.tmpFileForScreenshots
--    else
--    waitCanvas >>= actualElementScreenshot

--test :: SlamFeature Unit
--test = do
--  config <- getConfig
--
--  sectionMsg "lol viz"
--  sectionMsg "check next viz cell (search)"
--  withSmallZipsSearchedAll $ checkNextVizCell
--
--  sectionMsg "check next viz cell (explore)"
--  withSmallZipsOpened $ checkNextVizCell
--
--  sectionMsg "check next viz cell (query)"
--  withSmallZipsQueriedAll $ checkNextVizCell
--
--  sectionMsg "check set height/width"
--  checkSetHeightWidth
--
--  sectionMsg "check hide/show"
--  withSmallZipsAllChart do
--    config <- getConfig
--    cell <- getCellTitle 1
--    C.checkHideShowCell cell config.cell.vizEditor
--
--  sectionMsg "check switchers"
--  checkSwitchers
--
--  sectionMsg "alert when no available types"
--  checkAlert
--
--  sectionMsg "check availbale select options"
--  checkOptions
--
--  sectionMsg "check option filters"
--  checkOptionUniqueness
--
--  sectionMsg "check option auto select"
--  checkOptionAutoSelect
--
--  sectionMsg "check option save"
--  checkOptionSave
--
--  sectionMsg "trash viz cell"
--  checkTrashingVizCell
--
--  sectionMsg "run/refresh/embed"
--  checkRunRefreshEmbed
--
--  sectionMsg "aggregation"
--  checkAggregation
--
--  sectionMsg "check charts"
--  checkCharts
