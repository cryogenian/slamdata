module Model.ChartOptions (buildOptions) where

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

buildOptions :: ChartType -> JArray -> ChartConfiguration -> Option
buildOptions ty jarr conf = buildOptions_ ty (analyzeJArray jarr) conf

buildOptions_ :: ChartType -> M.Map JCursor Axis -> ChartConfiguration -> Option
buildOptions_ Pie mp conf = buildPie mp conf
buildOptions_ Bar mp conf = buildBar mp conf
buildOptions_ Line mp conf = buildLine mp conf
