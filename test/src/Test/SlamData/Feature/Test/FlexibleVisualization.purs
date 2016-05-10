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
import Test.Feature.Scenario (scenario)
import Test.SlamData.Feature.Expectations as Expect
import Test.SlamData.Feature.Interactions as Interact
import Test.SlamData.Feature.Monad (SlamFeature)
import Selenium.Monad (script, tryRepeatedlyTo)

apiVizScenario :: String -> Array String -> SlamFeature Unit -> SlamFeature Unit
apiVizScenario =
  scenario
    "Flexible Visualization"
    (Interact.createNotebookInTestFolder "Flexible Visualization")
    (Interact.deleteFileInTestFolder "Untitled Notebook.slam")

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
test =
  apiVizScenario "Make embedable patients-city charts" [] do
    tryRepeatedlyTo $ script """
      var run = function() {
        var __init = echarts.init;
        echarts.init = function (el) {
          var chart = __init.call(echarts, el);
          chart.setOption = function (options) {
            el.innerHTML = "<pre>" + JSON.stringify(options) + "</pre>";
          };
          return chart;
        };
      };
      run();
    """
    Interact.insertApiCardInLastDeck
    Interact.provideApiVariableBindingsForApiCard "state" "Text" "CO"
    Interact.accessNextCardInLastDeck
    Interact.insertApiResultsCardInLastDeck
    Interact.accessNextCardInLastDeck
    Interact.insertQueryCardInLastDeck
    Interact.provideQueryInLastQueryCard $ Str.joinWith " "
      $ [ "SELECT count(*) as ct, city, gender"
        , "FROM `/test-mount/testDb/patients`"
        , "WHERE state = :state"
        , "GROUP BY city, gender"
        , "ORDER BY ct DESC"
        , "LIMIT 30"
        ]
    Interact.accessNextCardInLastDeck
    Interact.insertJTableCardInLastDeck
    Interact.accessNextCardInLastDeck
    Interact.insertVisualizeCardInLastDeck
    Interact.switchToBarChart
    Interact.provideCategoryForLastVisualizeCard ".city"
    Expect.measureInLastVisualizeCard ".ct"
    Expect.measureDisabledInLastVisualizeCard
    Interact.accessNextCardInLastDeck
    Interact.insertChartCardInLastDeck
    Expect.lastEChartOptions chartOptions_CO
    Interact.accessPreviousCardInLastDeck
    Interact.provideSeriesForLastVizualizeCard ".gender"
    Interact.accessNextCardInLastDeck
    Expect.lastEChartOptions chartOptions_CO_gender
    Interact.accessNotebookWithModifiedURL (flip append "/?state=%22NE%22")
    Expect.lastEChartOptions chartOptions_NE_gender
    Interact.accessNotebookWithModifiedURL (Str.replace "NE" "CO")
    Expect.lastEChartOptions chartOptions_CO_gender
    successMsg "Successfully created flexible patient chart"

chartOptions_CO :: String
chartOptions_CO =
  """{"color":["#93A9A6","#CDA71F","#EB6F76","#66B35B","#BD97E2","#5B5925","#884A6F","#51B3F6","#CCA067","#398465","#3C607A","#81463C","#B65F33","#9AAE31","#CE8B9E","#6C6356","#95A779","#44AECB","#E987C2","#8A7EA0","#3D6C2F","#40B994","#87984A","#C1A088","#9B6E2D","#428C8D","#B8766A","#EB8666","#DF883E","#BB6273","#C994BF","#929DE0","#7BA5CB","#AE9093","#66557B","#936370","#3C6D64","#84693F","#C19744","#E77AA1","#5D555F","#4078A2","#3FAFAF","#698B99","#486A4C","#7EA48B","#B57E57","#8C72AA","#609050","#56B379","#489F8B","#714D4A","#9A8867","#93B66B","#7DA93F","#877424","#C75D56","#B774AC","#7B7A3E","#73581C","#398EA3","#964734","#DF8D89","#AF97CC","#96951F","#A37791","#7C4D2E","#78865F","#216B74","#935524","#6FAFB6","#75AB76","#A48B50","#D28DD0","#BE9AAF","#AD8D22","#D89576","#964860","#9B9A61","#4DAADB","#A9628D","#98943B","#486366","#6D7E2B","#CF9A2F","#827A8B","#876A69","#495F23","#677F45","#805845","#A2544D","#8C5157","#6B6C9E","#236443","#919B82","#CC8E55","#3E8555","#A08A7A","#767870","#6D9643","#87658F","#3BB069","#6A5D42","#586249","#1F7769","#6DAF8E","#8FA7BE","#B7A82C","#A09DA0","#7D8AA6","#78A3E0","#719186","#765771","#A37EA7","#8E8CBC","#A76840","#49934B","#A27C62","#3DA27B","#A9AC53","#6685B4","#5F728A","#CB6B4A","#9F8DD3","#B7A66E","#A998B3","#85A362","#595146"],"series":[{"type":"bar","data":[1,1,1,1,1,1,1,2,1,1,1,1,1,1,1],"stack":"total "}],"tooltip":{"trigger":"axis"},"legend":{"data":[]},"grid":{"y2":"15.0%"},"xAxis":{"type":"category","axisTick":{"interval":0},"axisLabel":{"textStyle":{"fontSize":12},"rotate":30},"data":["ANTONITO","ARVADA","DEL NORTE","DENVER","HENDERSON","JOHNSTOWN","KIOWA","LITTLETON","LONGMONT","MANZANOLA","PITKIN","ROCKVALE","SILVERTHORNE","VERNON","WALSH"]},"yAxis":{"type":"value"}}"""

chartOptions_CO_gender :: String
chartOptions_CO_gender =
    """{"color":["#93A9A6","#CDA71F","#EB6F76","#66B35B","#BD97E2","#5B5925","#884A6F","#51B3F6","#CCA067","#398465","#3C607A","#81463C","#B65F33","#9AAE31","#CE8B9E","#6C6356","#95A779","#44AECB","#E987C2","#8A7EA0","#3D6C2F","#40B994","#87984A","#C1A088","#9B6E2D","#428C8D","#B8766A","#EB8666","#DF883E","#BB6273","#C994BF","#929DE0","#7BA5CB","#AE9093","#66557B","#936370","#3C6D64","#84693F","#C19744","#E77AA1","#5D555F","#4078A2","#3FAFAF","#698B99","#486A4C","#7EA48B","#B57E57","#8C72AA","#609050","#56B379","#489F8B","#714D4A","#9A8867","#93B66B","#7DA93F","#877424","#C75D56","#B774AC","#7B7A3E","#73581C","#398EA3","#964734","#DF8D89","#AF97CC","#96951F","#A37791","#7C4D2E","#78865F","#216B74","#935524","#6FAFB6","#75AB76","#A48B50","#D28DD0","#BE9AAF","#AD8D22","#D89576","#964860","#9B9A61","#4DAADB","#A9628D","#98943B","#486366","#6D7E2B","#CF9A2F","#827A8B","#876A69","#495F23","#677F45","#805845","#A2544D","#8C5157","#6B6C9E","#236443","#919B82","#CC8E55","#3E8555","#A08A7A","#767870","#6D9643","#87658F","#3BB069","#6A5D42","#586249","#1F7769","#6DAF8E","#8FA7BE","#B7A82C","#A09DA0","#7D8AA6","#78A3E0","#719186","#765771","#A37EA7","#8E8CBC","#A76840","#49934B","#A27C62","#3DA27B","#A9AC53","#6685B4","#5F728A","#CB6B4A","#9F8DD3","#B7A66E","#A998B3","#85A362","#595146"],"series":[{"name":"female","type":"bar","data":[1,0,1,0,1,1,0,1,1,1,0,0,0,0,1],"stack":"total "},{"name":"male","type":"bar","data":[0,1,0,1,0,0,1,1,0,0,1,1,1,1,0],"stack":"total "}],"tooltip":{"trigger":"axis"},"legend":{"data":[{"name":"female"},{"name":"male"}]},"grid":{"y2":"15.0%"},"xAxis":{"type":"category","axisTick":{"interval":0},"axisLabel":{"textStyle":{"fontSize":12},"rotate":30},"data":["ANTONITO","ARVADA","DEL NORTE","DENVER","HENDERSON","JOHNSTOWN","KIOWA","LITTLETON","LONGMONT","MANZANOLA","PITKIN","ROCKVALE","SILVERTHORNE","VERNON","WALSH"]},"yAxis":{"type":"value"}}"""

chartOptions_NE_gender :: String
chartOptions_NE_gender =
  """{"color":["#93A9A6","#CDA71F","#EB6F76","#66B35B","#BD97E2","#5B5925","#884A6F","#51B3F6","#CCA067","#398465","#3C607A","#81463C","#B65F33","#9AAE31","#CE8B9E","#6C6356","#95A779","#44AECB","#E987C2","#8A7EA0","#3D6C2F","#40B994","#87984A","#C1A088","#9B6E2D","#428C8D","#B8766A","#EB8666","#DF883E","#BB6273","#C994BF","#929DE0","#7BA5CB","#AE9093","#66557B","#936370","#3C6D64","#84693F","#C19744","#E77AA1","#5D555F","#4078A2","#3FAFAF","#698B99","#486A4C","#7EA48B","#B57E57","#8C72AA","#609050","#56B379","#489F8B","#714D4A","#9A8867","#93B66B","#7DA93F","#877424","#C75D56","#B774AC","#7B7A3E","#73581C","#398EA3","#964734","#DF8D89","#AF97CC","#96951F","#A37791","#7C4D2E","#78865F","#216B74","#935524","#6FAFB6","#75AB76","#A48B50","#D28DD0","#BE9AAF","#AD8D22","#D89576","#964860","#9B9A61","#4DAADB","#A9628D","#98943B","#486366","#6D7E2B","#CF9A2F","#827A8B","#876A69","#495F23","#677F45","#805845","#A2544D","#8C5157","#6B6C9E","#236443","#919B82","#CC8E55","#3E8555","#A08A7A","#767870","#6D9643","#87658F","#3BB069","#6A5D42","#586249","#1F7769","#6DAF8E","#8FA7BE","#B7A82C","#A09DA0","#7D8AA6","#78A3E0","#719186","#765771","#A37EA7","#8E8CBC","#A76840","#49934B","#A27C62","#3DA27B","#A9AC53","#6685B4","#5F728A","#CB6B4A","#9F8DD3","#B7A66E","#A998B3","#85A362","#595146"],"series":[{"name":"female","type":"bar","data":[1,1,0,0,0,0,2,0,0,1,0,0,1,1],"stack":"total "},{"name":"male","type":"bar","data":[1,0,1,1,1,1,2,1,1,1,1,1,0,0],"stack":"total "}],"tooltip":{"trigger":"axis"},"legend":{"data":[{"name":"female"},{"name":"male"}]},"grid":{"y2":"15.0%"},"xAxis":{"type":"category","axisTick":{"interval":0},"axisLabel":{"textStyle":{"fontSize":12},"rotate":30},"data":["BEE","BLAIR","CAIRO","CRAIG","GOTHENBURG","LINCOLN","OMAHA","ORD","OSHKOSH","PIERCE","SAINT EDWARD","WILLOW ISLAND","WOOD LAKE","WYNOT"]},"yAxis":{"type":"value"}}"""
