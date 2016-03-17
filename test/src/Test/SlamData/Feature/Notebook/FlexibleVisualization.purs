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

module Test.SlamData.Feature.Notebook.FlexibleVisualation where

import Prelude

import Data.String as Str
import Test.Feature.Log (successMsg)
import Test.SlamData.Feature.Common (waitTime)
import Test.SlamData.Feature.Expectations as Expect
import Test.SlamData.Feature.Monad (SlamFeature())
import Test.SlamData.Feature.Notebook.Interactions as Interact
import Test.Feature.Scenario (scenario)

import XPath as XPath

apiVizScenario :: String -> Array String -> SlamFeature Unit -> SlamFeature Unit
apiVizScenario =
  scenario
    "Flexible Visualization"
    (Interact.createNotebookInTestFolder "Flexible Visualization")
    (Interact.deleteFileInTestFolder "Flexible Visualization.slam")

test :: SlamFeature Unit
test = do
  apiVizScenario "Make embeddable patients-city charts" [] do
    Interact.insertApiCardAsFirstCardInNewStack
    Interact.provideApiVariableBindingsForApiCard "state" "Text" "CO"
    Interact.playLastCard
    Interact.insertQueryCardAsNextAction
    Interact.provideQueryInLastQueryCard $ Str.joinWith "\n "
      $ [
          "SELECT"
        , "  count(*) as ct,"
        , "  city,"
        , "  gender"
        , "FROM `/test-mount/testDb/patientsSmall`"
        , "WHERE"
        , "  state = :state"
        , "GROUP BY city, gender"
        , "ORDER BY ct DESC"
        , "LIMIT 30"
        ]
    Interact.playLastCard
    Interact.insertVisualizeCardAsNextAction
    Interact.switchToBarChart
    Interact.provideCategoryForLastVisualizeCard ".city"

    Interact.expectMeasureEqualsForLastVisualizeCard ".ct"
    Interact.expectMeasureDisabledForLastVisualizeCard
    Interact.expectLastChartElementBeEqualWithScreenshot
      $ pathToExpectedImages <> "CO_wo_gender.png"
    Interact.provideSeriesForLastVizualizeCard ".gender"
    Interact.playLastCard
    Interact.expectLastChartElementBeEqualWithScreenshot
      $ pathToExpectedImages <> "CO.png"
    Interact.accessNotebookWithModifiedURL (flip append "/?state=%22CO%22")
    Interact.expectLastChartElementBeEqualWithScreenshot
      $ pathToExpectedImages <> "CO.png"
    Interact.accessNotebookWithModifiedURL (Str.replace "CO" "NE")
    Interact.expectLastChartElementBeEqualWithScreenshot
      $ pathToExpectedImages <> "NE.png"

    successMsg "Successfully created flexible patient chart"


  where
  pathToExpectedImages :: String
  pathToExpectedImages = "test/image/flexible-visualization/"
