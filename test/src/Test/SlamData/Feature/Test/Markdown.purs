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

module Test.SlamData.Feature.Test.Markdown where

import Data.String (joinWith)
import Control.Apply ((*>))
import Prelude
import Test.Feature.Log (successMsg)
import Test.SlamData.Feature.Monad (SlamFeature)
import Test.SlamData.Feature.Interactions as Interact
import Test.SlamData.Feature.Expectations as Expect
import Test.Feature.Scenario (scenario)

mdScenario :: String -> Array String -> SlamFeature Unit -> SlamFeature Unit
mdScenario =
  scenario
    "Markdown"
    (Interact.createNotebookInTestFolder "Markdown")
    (Interact.deleteFileInTestFolder "Untitled Notebook.slam"
       *> Interact.browseRootFolder)

test :: SlamFeature Unit
test = do
  mdScenario "Provide and play markdown" [] do
    Interact.insertMdCardAsFirstCardInNewStack
    Interact.provideMdInLastMdCard
      $ joinWith "\n\n"
          [ "discipline = __"
          , "sport = __ (Bobsleigh)"
          , "age = #__"
          , "year = #__ (2002)"
          , "startDate = __ - __ - __"
          , "finishDate = __ - __ - __ (2002-06-06)"
          , "startTime = __ : __"
          , "finishTime = __ : __ (20:39)"
          , "event = {1000m, 1500m, 3000m} (1500m)"
          , "gender = []M []W []X"
          , "color = [x]Red []Green [x]Blue"
          , "type = (x)Gold ()Silver ()Bronze"
          ]
    Interact.insertFormCardAsNextAction
    Expect.fieldInLastMdCard "discipline" "text" ""
    Expect.fieldInLastMdCard "sport" "text" "Bobsleigh"
    Expect.fieldInLastMdCard "age" "number" ""
    Expect.fieldInLastMdCard "year" "number" "2002"
    Expect.fieldInLastMdCard "startDate" "text" ""
    Expect.fieldInLastMdCard "finishDate" "text" "2002-06-06"
    Expect.fieldInLastMdCard "startTime" "text" ""
    Expect.fieldInLastMdCard "finishTime" "text" "20:39"
    Expect.labelInLastMdCard "event"
    Expect.dropdownInLastMdCard "1500m" ["1000m", "1500m", "3000m"]
    Expect.labelInLastMdCard "gender"
    Expect.checkableFieldInLastMdCard "X" "checkbox" false
    Expect.checkableFieldInLastMdCard "W" "checkbox" false
    Expect.checkableFieldInLastMdCard "M" "checkbox" false
    Expect.labelInLastMdCard "color"
    Expect.checkableFieldInLastMdCard "Red" "checkbox" true
    Expect.checkableFieldInLastMdCard "Green" "checkbox" false
    Expect.checkableFieldInLastMdCard "Blue" "checkbox" true
    Expect.labelInLastMdCard "type"
    Expect.checkableFieldInLastMdCard "Gold" "radio" true
    Expect.checkableFieldInLastMdCard "Silver" "radio" false
    Expect.checkableFieldInLastMdCard "Bronze" "radio" false
    Expect.lastCardToBeFinished
    successMsg "Ok, succesfully provided and played markdown."

  mdScenario "Change and play markdown" [] do
    Interact.insertMdCardAsFirstCardInNewStack
    Interact.provideMdInLastMdCard "discipline = __"
    Interact.insertFormCardAsNextAction
    Interact.provideMdInLastMdCard "sport =  __ (Bobsleigh)"

    Expect.fieldInLastMdCard "sport" "text" "Bobsleigh"
    successMsg "Ok, successfully changed and played markdown."

  mdScenario "Provide and play markdown with evaluated content" [] do
    Interact.insertMdCardAsFirstCardInNewStack
    Interact.provideMdInLastMdCard $ joinWith "\n\n"
      [ "discipline = __ (!``SELECT discipline FROM `/test-mount/testDb/olympics` LIMIT 1``)"
      , "year = __ (!``SELECT year FROM `/test-mount/testDb/olympics` LIMIT 1``)"
      , "country = {!``SELECT DISTINCT country FROM `/test-mount/testDb/olympics` ``} (!``SELECT country FROM `/test-mount/testDb/olympics` LIMIT 1``)"
      , "type = (!``SELECT DISTINCT type FROM `/test-mount/testDb/olympics` LIMIT 1``) !``SELECT DISTINCT type FROM `/test-mount/testDb/olympics` OFFSET 1``"
      , "gender = [!``SELECT gender FROM `/test-mount/testDb/olympics` LIMIT 1``] !``SELECT DISTINCT gender FROM `/test-mount/testDb/olympics` ``"
      ]
    Interact.insertFormCardAsNextAction
    Expect.fieldInLastMdCard "discipline" "text" "Figure skating"
    Expect.fieldInLastMdCard "year" "text" "1924"
    Expect.labelInLastMdCard "country"
    Expect.dropdownInLastMdCard "AUT"
      [ "LAT"
      , "CZE"
      , "UKR"
      , "SLO"
      , "RUS"
      , "SVK"
      , "KAZ"
      , "AUS"
      , "LUX"
      , "UZB"
      , "EUN"
      , "DEN"
      , "CHN"
      , "ROU"
      , "GDR"
      , "PRK"
      , "CRO"
      , "URS"
      , "BLR"
      , "BUL"
      , "POL"
      , "EUA"
      , "KOR"
      , "NED"
      , "ITA"
      , "FRG"
      , "EST"
      , "SWE"
      , "GBR"
      , "TCH"
      , "BEL"
      , "FIN"
      , "USA"
      , "YUG"
      , "SUI"
      , "LIE"
      , "CAN"
      , "JPN"
      , "HUN"
      , "GER"
      , "NOR"
      , "NZL"
      , "FRA"
      , "AUT"
      , "ESP"
      ]
    Expect.labelInLastMdCard "gender"
    Expect.checkableFieldInLastMdCard "X" "checkbox" false
    Expect.checkableFieldInLastMdCard "W" "checkbox" true
    Expect.checkableFieldInLastMdCard "M" "checkbox" false
    Expect.labelInLastMdCard "type"
    Expect.checkableFieldInLastMdCard "Gold" "radio" false
    Expect.checkableFieldInLastMdCard "Silver" "radio" true
    Expect.checkableFieldInLastMdCard "Bronze" "radio" false
    successMsg "Ok, successfully provided and played markdown with evaluated content"

  mdScenario "Filter query results with default field values" [] do
    Interact.insertMdCardAsFirstCardInNewStack
    Interact.provideMdInLastMdCard $ joinWith "\n\n"
      [ "discipline = __ (!``SELECT discipline FROM `/test-mount/testDb/olympics` LIMIT 1``)"
      , "year = __ (!``SELECT year FROM `/test-mount/testDb/olympics` LIMIT 1``)"
      , "country = {!``SELECT DISTINCT country FROM `/test-mount/testDb/olympics` ``} (!``SELECT country FROM `/test-mount/testDb/olympics` LIMIT 1``)"
      , "type = (!``SELECT DISTINCT type FROM `/test-mount/testDb/olympics` LIMIT 1``) !``SELECT DISTINCT type FROM `/test-mount/testDb/olympics` OFFSET 1``"
      , "gender = [!``SELECT gender FROM `/test-mount/testDb/olympics` LIMIT 1``] !``SELECT DISTINCT gender FROM `/test-mount/testDb/olympics` ``"
      ]
    Interact.insertFormCardAsNextAction
    Interact.insertQueryCardAsNextAction
    Interact.provideQueryInLastQueryCard
      "SELECT * FROM `/test-mount/testDb/olympics` WHERE discipline = :discipline AND type NOT IN :type[_] AND gender IN :gender[_] AND year > :year AND country IN :country[_]"
--    Interact.playLastCard
    Interact.insertJTableCardAsNextAction
    Expect.cardsInTableColumnInLastCardToEq 2 "discipline" "Figure skating"
    Expect.cardsInTableColumnInLastCardToEq 2 "country" "AUT"
    Expect.cardsInTableColumnInLastCardToEq 2 "gender" "W"
    Expect.cardsInTableColumnInLastCardToBeGT 2 "year" "1924"
    Expect.cardsInTableColumnInLastCardToNotEq 2 "type" "Silver"
    successMsg "Ok, Filtered query results with fields"

  mdScenario "Filter query results by changing field values" ["https://slamdata.atlassian.net/browse/SD-1605"] do
    Interact.insertMdCardAsFirstCardInNewStack
    Interact.provideMdInLastMdCard $ joinWith "\n\n"
      [ "discipline = __ (!``SELECT discipline FROM `/test-mount/testDb/olympics` LIMIT 1``)"
      , "year = __ (!``SELECT year FROM `/test-mount/testDb/olympics` LIMIT 1``)"
      , "country = {!``SELECT DISTINCT country FROM `/test-mount/testDb/olympics` ``} (!``SELECT country FROM `/test-mount/testDb/olympics` LIMIT 1``)"
      , "type = (!``SELECT DISTINCT type FROM `/test-mount/testDb/olympics` LIMIT 1``) !``SELECT DISTINCT type FROM `/test-mount/testDb/olympics` OFFSET 1``"
      , "gender = [!``SELECT gender FROM `/test-mount/testDb/olympics` LIMIT 1``] !``SELECT DISTINCT gender FROM `/test-mount/testDb/olympics` ``"
      ]
    Interact.insertFormCardAsNextAction
    Interact.insertQueryCardAsNextAction
    Interact.provideQueryInLastQueryCard
      "SELECT * FROM `/test-mount/testDb/olympics` WHERE discipline = :discipline AND type NOT IN :type[_] AND gender IN :gender[_] AND year > :year AND country IN :country[_]"
    Interact.insertJTableCardAsNextAction
    Interact.provideFieldValueInLastMdCard "discipline" "Luge"
    Interact.provideFieldValueInLastMdCard "year" "1950"
    Interact.uncheckFieldInLastMdCard "W"
    Interact.checkFieldInLastMdCard "X"
    Interact.checkFieldInLastMdCard "M"
    Interact.pushRadioButtonInLastMdCard "Gold"
    Interact.selectFromDropdownInLastMdCard "country" "GDR"
    Expect.cardsInTableColumnInLastCardToEq 8 "discipline" "Luge"
    Expect.cardsInTableColumnInLastCardToEqOneOf 8 "gender" ["M", "X"]
    Expect.cardsInTableColumnInLastCardToBeGT 8 "year" "1950"
    Expect.cardsInTableColumnInLastCardToNotEq 8 "type" "Gold"
    successMsg "Ok, Filtered query results by changing field values"
    Test.Feature.Log.errorMsg "This scenario passed but is known to fail non-deterministically"
