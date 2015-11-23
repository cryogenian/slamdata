module Model.ChartOptions where

import Prelude

import Data.Argonaut (JArray(), JCursor())
import Data.Map as M
import ECharts (Option())
import Model.ChartAxis (analyzeJArray, Axis())
import Model.ChartConfiguration (ChartConfiguration())
import Model.ChartType (ChartType(..))
import Model.Port (Port(..))
import Model.ChartOptions.Pie (buildPie)
import Model.ChartOptions.Bar (buildBar)
import Model.ChartOptions.Line (buildLine)

buildOptionsPort :: ChartType -> JArray -> ChartConfiguration -> Port
buildOptionsPort ty jarr conf =
  ChartOptions $ buildOptions ty (analyzeJArray jarr) conf

buildOptions :: ChartType -> M.Map JCursor Axis -> ChartConfiguration -> Option
buildOptions Pie mp conf = buildPie mp conf
buildOptions Bar mp conf = buildBar mp conf
buildOptions Line mp conf = buildLine mp conf
