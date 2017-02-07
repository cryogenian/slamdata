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
  variablesChartScenario "Make embeddable patients-city charts" ["https://github.com/slamdata/slamdata/issues/1197"] do
    tryRepeatedlyTo $ script """
      var run = function() {
        var __init = echarts.init;
        echarts.init = function (el) {
          var chart = __init.call(echarts, el);
          chart.setOption = function (options) {
            delete options.grid;
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
    Interact.selectInChartBuilder [".city"]
    Expect.measureInLastChartCard ".ct"
    Interact.accessNextCardInLastDeck
    successMsg "Will insert chart options card"
    Interact.insertChartCardInLastDeck

    Expect.lastEChart chart_CO
    Interact.accessPreviousCardInLastDeck
    Interact.activateStackForChartBuilder
    Interact.selectInChartBuilder [".gender"]

    Interact.accessNextCardInLastDeck

    Expect.lastEChart chart_CO_gender
    Interact.setVarMapForDeck "Flexible Visualization" $ SM.singleton "state" "NE"

    Expect.lastEChart chart_NE_gender
    Interact.accessWorkspaceWithModifiedURL $ S.replace (S.Pattern "NE") (S.Replacement "CO")

    Expect.lastEChart chart_CO_gender

    successMsg "Successfully created flexible patient chart"

chart_CO ∷ String
chart_CO =
  """{"series":[{"stack":"default stack","data":[{"value":["ANTONITO",1],"name":"ANTONITO"},{"value":["ARVADA",1],"name":"ARVADA"},{"value":["DEL NORTE",1],"name":"DEL NORTE"},{"value":["DENVER",1],"name":"DENVER"},{"value":["HENDERSON",1],"name":"HENDERSON"},{"value":["JOHNSTOWN",1],"name":"JOHNSTOWN"},{"value":["KIOWA",1],"name":"KIOWA"},{"value":["LITTLETON",2],"name":"LITTLETON"},{"value":["LONGMONT",1],"name":"LONGMONT"},{"value":["MANZANOLA",1],"name":"MANZANOLA"},{"value":["PITKIN",1],"name":"PITKIN"},{"value":["ROCKVALE",1],"name":"ROCKVALE"},{"value":["SILVERTHORNE",1],"name":"SILVERTHORNE"},{"value":["VERNON",1],"name":"VERNON"},{"value":["WALSH",1],"name":"WALSH"}],"type":"bar"}],"legend":{"top":"bottom","left":"left","data":[],"textStyle":{"fontFamily":"Ubuntu, sans"}},"yAxis":{"splitLine":{"lineStyle":{"width":1}},"axisLine":{"lineStyle":{"width":1}},"axisLabel":{"textStyle":{"fontFamily":"Ubuntu, sans"}},"type":"value"},"xAxis":{"axisLabel":{"textStyle":{"fontFamily":"Ubuntu, sans"},"rotate":0,"interval":0},"data":["ANTONITO","ARVADA","DEL NORTE","DENVER","HENDERSON","JOHNSTOWN","KIOWA","LITTLETON","LONGMONT","MANZANOLA","PITKIN","ROCKVALE","SILVERTHORNE","VERNON","WALSH"],"boundaryGap":true,"type":"category"},"color":["#93a9a6","#cda71f","#eb6f76","#66b35b","#bd97e2","#5b5925","#884a6f","#51b3f6","#cca067","#398465","#3c607a","#81463c","#b65f33","#9aae31","#ce8b9e","#6c6356","#95a779","#44aecb","#e987c2","#8a7ea0","#3d6c2f","#40b994","#87984a","#c1a088","#9b6e2d","#428c8d","#b8766a","#eb8666","#df883e","#bb6273","#c994bf","#929de0","#7ba5cb","#ae9093","#66557b","#936370","#3c6d64","#84693f","#c19744","#e77aa1","#5d555f","#4078a2","#3fafaf","#698b99","#486a4c","#7ea48b","#b57e57","#8c72aa","#609050","#56b379","#489f8b","#714d4a","#9a8867","#93b66b","#7da93f","#877424","#c75d56","#b774ac","#7b7a3e","#73581c","#398ea3","#964734","#df8d89","#af97cc","#96951f","#a37791","#7c4d2e","#78865f","#216b74","#935524","#6fafb6","#75ab76","#a48b50","#d28dd0","#be9aaf","#ad8d22","#d89576","#964860","#9b9a61","#4daadb","#a9628d","#98943b","#486366","#6d7e2b","#cf9a2f","#827a8b","#876a69","#495f23","#677f45","#805845","#a2544d","#8c5157","#6b6c9e","#236443","#919b82","#cc8e55","#3e8555","#a08a7a","#767870","#6d9643","#87658f","#3bb069","#6a5d42","#586249","#1f7769","#6daf8e","#8fa7be","#b7a82c","#a09da0","#7d8aa6","#78a3e0","#719186","#765771","#a37ea7","#8e8cbc","#a76840","#49934b","#a27c62","#3da27b","#a9ac53","#6685b4","#5f728a","#cb6b4a","#9f8dd3","#b7a66e","#a998b3","#85a362","#595146"],"tooltip":{"trigger":"axis"}}"""

chart_CO_gender ∷ String
chart_CO_gender =
  """{"series":[{"name":"female","stack":"default stack","data":[{"value":["ANTONITO",1],"name":"ANTONITO"},null,{"value":["DEL NORTE",1],"name":"DEL NORTE"},null,{"value":["HENDERSON",1],"name":"HENDERSON"},{"value":["JOHNSTOWN",1],"name":"JOHNSTOWN"},null,{"value":["LITTLETON",1],"name":"LITTLETON"},{"value":["LONGMONT",1],"name":"LONGMONT"},{"value":["MANZANOLA",1],"name":"MANZANOLA"},null,null,null,null,{"value":["WALSH",1],"name":"WALSH"}],"type":"bar"},{"name":"male","stack":"default stack","data":[null,{"value":["ARVADA",1],"name":"ARVADA"},null,{"value":["DENVER",1],"name":"DENVER"},null,null,{"value":["KIOWA",1],"name":"KIOWA"},{"value":["LITTLETON",1],"name":"LITTLETON"},null,null,{"value":["PITKIN",1],"name":"PITKIN"},{"value":["ROCKVALE",1],"name":"ROCKVALE"},{"value":["SILVERTHORNE",1],"name":"SILVERTHORNE"},{"value":["VERNON",1],"name":"VERNON"},null],"type":"bar"}],"legend":{"top":"bottom","left":"left","data":["female","male"],"textStyle":{"fontFamily":"Ubuntu, sans"}},"yAxis":{"splitLine":{"lineStyle":{"width":1}},"axisLine":{"lineStyle":{"width":1}},"axisLabel":{"textStyle":{"fontFamily":"Ubuntu, sans"}},"type":"value"},"xAxis":{"axisLabel":{"textStyle":{"fontFamily":"Ubuntu, sans"},"rotate":0,"interval":0},"data":["ANTONITO","ARVADA","DEL NORTE","DENVER","HENDERSON","JOHNSTOWN","KIOWA","LITTLETON","LONGMONT","MANZANOLA","PITKIN","ROCKVALE","SILVERTHORNE","VERNON","WALSH"],"boundaryGap":true,"type":"category"},"color":["#93a9a6","#cda71f","#eb6f76","#66b35b","#bd97e2","#5b5925","#884a6f","#51b3f6","#cca067","#398465","#3c607a","#81463c","#b65f33","#9aae31","#ce8b9e","#6c6356","#95a779","#44aecb","#e987c2","#8a7ea0","#3d6c2f","#40b994","#87984a","#c1a088","#9b6e2d","#428c8d","#b8766a","#eb8666","#df883e","#bb6273","#c994bf","#929de0","#7ba5cb","#ae9093","#66557b","#936370","#3c6d64","#84693f","#c19744","#e77aa1","#5d555f","#4078a2","#3fafaf","#698b99","#486a4c","#7ea48b","#b57e57","#8c72aa","#609050","#56b379","#489f8b","#714d4a","#9a8867","#93b66b","#7da93f","#877424","#c75d56","#b774ac","#7b7a3e","#73581c","#398ea3","#964734","#df8d89","#af97cc","#96951f","#a37791","#7c4d2e","#78865f","#216b74","#935524","#6fafb6","#75ab76","#a48b50","#d28dd0","#be9aaf","#ad8d22","#d89576","#964860","#9b9a61","#4daadb","#a9628d","#98943b","#486366","#6d7e2b","#cf9a2f","#827a8b","#876a69","#495f23","#677f45","#805845","#a2544d","#8c5157","#6b6c9e","#236443","#919b82","#cc8e55","#3e8555","#a08a7a","#767870","#6d9643","#87658f","#3bb069","#6a5d42","#586249","#1f7769","#6daf8e","#8fa7be","#b7a82c","#a09da0","#7d8aa6","#78a3e0","#719186","#765771","#a37ea7","#8e8cbc","#a76840","#49934b","#a27c62","#3da27b","#a9ac53","#6685b4","#5f728a","#cb6b4a","#9f8dd3","#b7a66e","#a998b3","#85a362","#595146"],"tooltip":{"trigger":"axis"}}"""


chart_NE_gender ∷ String
chart_NE_gender =
  """{"series":[{"name":"female","stack":"default stack","data":[{"value":["BEE",1],"name":"BEE"},{"value":["BLAIR",1],"name":"BLAIR"},null,null,null,null,{"value":["OMAHA",2],"name":"OMAHA"},null,null,{"value":["PIERCE",1],"name":"PIERCE"},null,null,{"value":["WOOD LAKE",1],"name":"WOOD LAKE"},{"value":["WYNOT",1],"name":"WYNOT"}],"type":"bar"},{"name":"male","stack":"default stack","data":[{"value":["BEE",1],"name":"BEE"},null,{"value":["CAIRO",1],"name":"CAIRO"},{"value":["CRAIG",1],"name":"CRAIG"},{"value":["GOTHENBURG",1],"name":"GOTHENBURG"},{"value":["LINCOLN",1],"name":"LINCOLN"},{"value":["OMAHA",2],"name":"OMAHA"},{"value":["ORD",1],"name":"ORD"},{"value":["OSHKOSH",1],"name":"OSHKOSH"},{"value":["PIERCE",1],"name":"PIERCE"},{"value":["SAINT EDWARD",1],"name":"SAINT EDWARD"},{"value":["WILLOW ISLAND",1],"name":"WILLOW ISLAND"},null,null],"type":"bar"}],"legend":{"top":"bottom","left":"left","data":["female","male"],"textStyle":{"fontFamily":"Ubuntu, sans"}},"yAxis":{"splitLine":{"lineStyle":{"width":1}},"axisLine":{"lineStyle":{"width":1}},"axisLabel":{"textStyle":{"fontFamily":"Ubuntu, sans"}},"type":"value"},"xAxis":{"axisLabel":{"textStyle":{"fontFamily":"Ubuntu, sans"},"rotate":0,"interval":0},"data":["BEE","BLAIR","CAIRO","CRAIG","GOTHENBURG","LINCOLN","OMAHA","ORD","OSHKOSH","PIERCE","SAINT EDWARD","WILLOW ISLAND","WOOD LAKE","WYNOT"],"boundaryGap":true,"type":"category"},"color":["#93a9a6","#cda71f","#eb6f76","#66b35b","#bd97e2","#5b5925","#884a6f","#51b3f6","#cca067","#398465","#3c607a","#81463c","#b65f33","#9aae31","#ce8b9e","#6c6356","#95a779","#44aecb","#e987c2","#8a7ea0","#3d6c2f","#40b994","#87984a","#c1a088","#9b6e2d","#428c8d","#b8766a","#eb8666","#df883e","#bb6273","#c994bf","#929de0","#7ba5cb","#ae9093","#66557b","#936370","#3c6d64","#84693f","#c19744","#e77aa1","#5d555f","#4078a2","#3fafaf","#698b99","#486a4c","#7ea48b","#b57e57","#8c72aa","#609050","#56b379","#489f8b","#714d4a","#9a8867","#93b66b","#7da93f","#877424","#c75d56","#b774ac","#7b7a3e","#73581c","#398ea3","#964734","#df8d89","#af97cc","#96951f","#a37791","#7c4d2e","#78865f","#216b74","#935524","#6fafb6","#75ab76","#a48b50","#d28dd0","#be9aaf","#ad8d22","#d89576","#964860","#9b9a61","#4daadb","#a9628d","#98943b","#486366","#6d7e2b","#cf9a2f","#827a8b","#876a69","#495f23","#677f45","#805845","#a2544d","#8c5157","#6b6c9e","#236443","#919b82","#cc8e55","#3e8555","#a08a7a","#767870","#6d9643","#87658f","#3bb069","#6a5d42","#586249","#1f7769","#6daf8e","#8fa7be","#b7a82c","#a09da0","#7d8aa6","#78a3e0","#719186","#765771","#a37ea7","#8e8cbc","#a76840","#49934b","#a27c62","#3da27b","#a9ac53","#6685b4","#5f728a","#cb6b4a","#9f8dd3","#b7a66e","#a998b3","#85a362","#595146"],"tooltip":{"trigger":"axis"}}"""
