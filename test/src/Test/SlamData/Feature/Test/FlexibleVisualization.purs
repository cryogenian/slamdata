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

module Test.SlamData.Feature.Test.FlexibleVisualation where

import Prelude

import Data.String as Str
import Test.Feature.Log (successMsg)
import Test.SlamData.Feature.Expectations as Expect
import Test.SlamData.Feature.Monad (SlamFeature)
import Test.SlamData.Feature.Interactions as Interact
import Test.Feature.Scenario (scenario)

apiVizScenario :: String -> Array String -> SlamFeature Unit -> SlamFeature Unit
apiVizScenario =
  scenario
    "Flexible Visualization"
    (Interact.createNotebookInTestFolder "Flexible Visualization")
    (Interact.deleteFileInTestFolder "Flexible Visualization.slam")

expectedImagesBasePath :: String
expectedImagesBasePath =
  "test/image/flexible-visualization/"

expectedColoradoWithoutSeriesChartImages :: Array String
expectedColoradoWithoutSeriesChartImages =
  map (append expectedImagesBasePath)
    [
      "CO-without-series-mac.png"
    , "CO-without-series-linux.png"
    ]

expectedColoradoChartImages :: Array String
expectedColoradoChartImages =
  map (append expectedImagesBasePath)
    [
      "CO-mac.png"
    , "CO-linux.png"
    ]

expectedNebraskaChartImages :: Array String
expectedNebraskaChartImages =
  map (append expectedImagesBasePath)
    [
      "NE-mac.png"
    , "NE-linux.png"
    ]

test :: SlamFeature Unit
test = do
  apiVizScenario "Make embedable patients-city charts" [] do
    Interact.insertApiCardAsFirstCardInNewStack
    Interact.provideApiVariableBindingsForApiCard "state" "Text" "CO"
    Interact.playLastCard
    Interact.insertQueryCardAsNextAction
    Interact.provideQueryInLastQueryCard $ Str.joinWith " "
      $ [ "SELECT count(*) as ct, city, gender"
        , "FROM `/test-mount/testDb/patients`"
        , "WHERE state = :state"
        , "GROUP BY city, gender"
        , "ORDER BY ct DESC"
        , "LIMIT 30"
        ]
    Interact.playLastCard
    Interact.insertVisualizeCardAsNextAction
    Interact.switchToBarChart
    Interact.provideCategoryForLastVisualizeCard ".city"
    Expect.measureInLastVisualizeCard ".ct"
    Expect.measureDisabledInLastVisualizeCard
    Expect.lastChartScreenshotToMatchAny expectedColoradoWithoutSeriesChartImages
    Interact.provideSeriesForLastVizualizeCard ".gender"
    Interact.playLastCard
    Expect.lastChartScreenshotToMatchAny expectedColoradoChartImages
    Interact.accessNotebookWithModifiedURL (flip append "/?state=%22NE%22")
    Expect.lastChartScreenshotToMatchAny expectedNebraskaChartImages
    Interact.accessNotebookWithModifiedURL (Str.replace "NE" "CO")
    Expect.lastChartScreenshotToMatchAny expectedColoradoChartImages
    successMsg "Successfully created flexible patient chart"
