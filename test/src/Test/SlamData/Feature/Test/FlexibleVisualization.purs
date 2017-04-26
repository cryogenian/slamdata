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

import SlamData.Prelude

import Data.String as S
import Data.StrMap as SM

import Test.Feature.Log (successMsg)
import Test.Feature.Scenario (scenario)
import Test.SlamData.Feature.Expectations as Expect
import Test.SlamData.Feature.Interactions as Interact
import Test.SlamData.Feature.Monad (SlamFeature)

import Selenium.Monad (script, tryRepeatedlyTo)

variablesChartScenario ∷ String → Array String → SlamFeature Unit → SlamFeature Unit
variablesChartScenario =
  scenario
    "Flexible Visualization"
    (Interact.createWorkspaceInTestFolder "Flexible Visualization")
    (Interact.deleteFileInTestFolder "Flexible Visualization.slam")

test ∷ SlamFeature Unit
test =
  variablesChartScenario "Make embeddable patients-city charts" [] do
    _ ← tryRepeatedlyTo $ script """
      var run = function() {
        var __init = echarts.init;
        echarts.init = function (el) {
          var chart = __init.call(echarts, el);
          chart.setOption = function (options) {
            delete options.grid;
            delete options.color;
            el.innerHTML = "<pre>" + JSON.stringify(options) + "</pre>";
          };
          return chart;
        };
      };
      run();
    """
    Interact.insertVariablesCardInLastDeck
    successMsg "Inserted Variables Card"
    Interact.provideApiVariableBindingsForVariablesCard "state" "Text" "CO"
    successMsg "Provided variables; Will access next card"
    Interact.accessNextCardInLastDeck
    successMsg "Accessed next card, will insert Troubleshoot card"
    Interact.insertTroubleshootCardInLastDeck
    Expect.troubleshootCardPresented
    successMsg "Presented Troubleshoot card, will access next card"
    Interact.accessNextCardInLastDeck
    successMsg "Will insert query card"
    Interact.insertQueryCardInLastDeck
    Interact.provideQueryInLastQueryCard $ S.joinWith " "
      $ [ "SELECT count(*) as ct, city, gender"
        , "FROM `/test-mount/testDb/patients`"
        , "WHERE state = :state"
        , "GROUP BY city, gender"
        , "ORDER BY ct DESC"
        , "LIMIT 30"
        ]
    Interact.runQuery
    Interact.accessNextCardInLastDeck
    Interact.selectBuildChart
    Interact.insertBuildBarChartCard
    Expect.categoryEnabledInLastBuildChartCard
    successMsg "Ok, category field is enabled"
    Interact.activateCategoryForChartBuilder
    Interact.selectInChartBuilder ["city"]
    Interact.activateMeasureForChartBuilder
    Interact.selectInChartBuilder ["ct"]
    Interact.accessNextCardInLastDeck
    successMsg "Will insert chart options card"
    Interact.insertChartCardInLastDeck

    Expect.lastEChart chart_CO
    Interact.accessPreviousCardInLastDeck
    Interact.activateStackForChartBuilder
    Interact.selectInChartBuilder ["gender"]

    Interact.accessNextCardInLastDeck

    Expect.lastEChart chart_CO_gender
    Interact.setVarMapForDeck "Flexible Visualization" $ SM.singleton "state" "NE"

    Expect.lastEChart chart_NE_gender
    Interact.accessWorkspaceWithModifiedURL $ S.replace (S.Pattern "NE") (S.Replacement "CO")

    Expect.lastEChart chart_CO_gender

    successMsg "Successfully created flexible patient chart"

chart_CO ∷ String
chart_CO =
  """{"series":[{"stack":"default stack","data":[{"value":["ANTONITO",1],"name":"ANTONITO"},{"value":["ARVADA",1],"name":"ARVADA"},{"value":["DEL NORTE",1],"name":"DEL NORTE"},{"value":["DENVER",1],"name":"DENVER"},{"value":["HENDERSON",1],"name":"HENDERSON"},{"value":["JOHNSTOWN",1],"name":"JOHNSTOWN"},{"value":["KIOWA",1],"name":"KIOWA"},{"value":["LITTLETON",2],"name":"LITTLETON"},{"value":["LONGMONT",1],"name":"LONGMONT"},{"value":["MANZANOLA",1],"name":"MANZANOLA"},{"value":["PITKIN",1],"name":"PITKIN"},{"value":["ROCKVALE",1],"name":"ROCKVALE"},{"value":["SILVERTHORNE",1],"name":"SILVERTHORNE"},{"value":["VERNON",1],"name":"VERNON"},{"value":["WALSH",1],"name":"WALSH"}],"type":"bar"}],"legend":{"top":"bottom","left":"left","data":[],"textStyle":{"fontFamily":"Ubuntu, sans"}},"yAxis":{"splitLine":{"lineStyle":{"width":1}},"axisLine":{"lineStyle":{"width":1}},"axisLabel":{"textStyle":{"fontFamily":"Ubuntu, sans"}},"type":"value"},"xAxis":{"axisLabel":{"textStyle":{"fontFamily":"Ubuntu, sans"},"rotate":0,"interval":0},"data":["ANTONITO","ARVADA","DEL NORTE","DENVER","HENDERSON","JOHNSTOWN","KIOWA","LITTLETON","LONGMONT","MANZANOLA","PITKIN","ROCKVALE","SILVERTHORNE","VERNON","WALSH"],"boundaryGap":true,"type":"category"},"tooltip":{"trigger":"axis","textStyle":{"fontSize":12}}}"""

chart_CO_gender ∷ String
chart_CO_gender =
  """{"series":[{"name":"female","stack":"default stack","data":[{"value":["ANTONITO",1],"name":"ANTONITO"},null,{"value":["DEL NORTE",1],"name":"DEL NORTE"},null,{"value":["HENDERSON",1],"name":"HENDERSON"},{"value":["JOHNSTOWN",1],"name":"JOHNSTOWN"},null,{"value":["LITTLETON",1],"name":"LITTLETON"},{"value":["LONGMONT",1],"name":"LONGMONT"},{"value":["MANZANOLA",1],"name":"MANZANOLA"},null,null,null,null,{"value":["WALSH",1],"name":"WALSH"}],"type":"bar"},{"name":"male","stack":"default stack","data":[null,{"value":["ARVADA",1],"name":"ARVADA"},null,{"value":["DENVER",1],"name":"DENVER"},null,null,{"value":["KIOWA",1],"name":"KIOWA"},{"value":["LITTLETON",1],"name":"LITTLETON"},null,null,{"value":["PITKIN",1],"name":"PITKIN"},{"value":["ROCKVALE",1],"name":"ROCKVALE"},{"value":["SILVERTHORNE",1],"name":"SILVERTHORNE"},{"value":["VERNON",1],"name":"VERNON"},null],"type":"bar"}],"legend":{"top":"bottom","left":"left","data":["female","male"],"textStyle":{"fontFamily":"Ubuntu, sans"}},"yAxis":{"splitLine":{"lineStyle":{"width":1}},"axisLine":{"lineStyle":{"width":1}},"axisLabel":{"textStyle":{"fontFamily":"Ubuntu, sans"}},"type":"value"},"xAxis":{"axisLabel":{"textStyle":{"fontFamily":"Ubuntu, sans"},"rotate":0,"interval":0},"data":["ANTONITO","ARVADA","DEL NORTE","DENVER","HENDERSON","JOHNSTOWN","KIOWA","LITTLETON","LONGMONT","MANZANOLA","PITKIN","ROCKVALE","SILVERTHORNE","VERNON","WALSH"],"boundaryGap":true,"type":"category"},"tooltip":{"trigger":"axis","textStyle":{"fontSize":12}}}"""


chart_NE_gender ∷ String
chart_NE_gender =
  """{"series":[{"name":"female","stack":"default stack","data":[{"value":["BEE",1],"name":"BEE"},{"value":["BLAIR",1],"name":"BLAIR"},null,null,null,null,{"value":["OMAHA",2],"name":"OMAHA"},null,null,{"value":["PIERCE",1],"name":"PIERCE"},null,null,{"value":["WOOD LAKE",1],"name":"WOOD LAKE"},{"value":["WYNOT",1],"name":"WYNOT"}],"type":"bar"},{"name":"male","stack":"default stack","data":[{"value":["BEE",1],"name":"BEE"},null,{"value":["CAIRO",1],"name":"CAIRO"},{"value":["CRAIG",1],"name":"CRAIG"},{"value":["GOTHENBURG",1],"name":"GOTHENBURG"},{"value":["LINCOLN",1],"name":"LINCOLN"},{"value":["OMAHA",2],"name":"OMAHA"},{"value":["ORD",1],"name":"ORD"},{"value":["OSHKOSH",1],"name":"OSHKOSH"},{"value":["PIERCE",1],"name":"PIERCE"},{"value":["SAINT EDWARD",1],"name":"SAINT EDWARD"},{"value":["WILLOW ISLAND",1],"name":"WILLOW ISLAND"},null,null],"type":"bar"}],"legend":{"top":"bottom","left":"left","data":["female","male"],"textStyle":{"fontFamily":"Ubuntu, sans"}},"yAxis":{"splitLine":{"lineStyle":{"width":1}},"axisLine":{"lineStyle":{"width":1}},"axisLabel":{"textStyle":{"fontFamily":"Ubuntu, sans"}},"type":"value"},"xAxis":{"axisLabel":{"textStyle":{"fontFamily":"Ubuntu, sans"},"rotate":0,"interval":0},"data":["BEE","BLAIR","CAIRO","CRAIG","GOTHENBURG","LINCOLN","OMAHA","ORD","OSHKOSH","PIERCE","SAINT EDWARD","WILLOW ISLAND","WOOD LAKE","WYNOT"],"boundaryGap":true,"type":"category"},"tooltip":{"trigger":"axis","textStyle":{"fontSize":12}}}"""
