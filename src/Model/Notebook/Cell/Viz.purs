module Model.Notebook.Cell.Viz where

import Data.Maybe (Maybe(..), maybe)
import ECharts.Chart
import ECharts.Options

import Data.Argonaut.JCursor (JCursor())
import Data.Map (Map(), empty, keys)
import Model.Notebook.ECharts (Semanthic(..), Axis(..))
import Data.Argonaut.JCursor (JCursor())
import Optic.Core (Lens(), lens)
import qualified Data.Set as S 
import Data.Argonaut.Core (JArray())
import Data.Array (findIndex)

data ChartType
  = Pie
  | Line
  | Bar

chartType2str :: ChartType -> String
chartType2str Pie = "Pie"
chartType2str Line = "Line"
chartType2str Bar = "Bar"

instance chartTypeEq :: Eq ChartType where
  (==) Pie Pie = true
  (==) Line Line = true
  (==) Bar Bar = true
  (==) _ _ = false
  (/=) a b = not $ a == b

instance chartTypeOrd :: Ord ChartType where
  compare Pie Pie = EQ
  compare Line Line = EQ
  compare Bar Bar = EQ 
  compare Pie _ = LT
  compare _ Pie = GT
  compare Line _ = LT
  compare _ Line = GT
  

str2chartType :: String -> Maybe ChartType
str2chartType str = case str of
  "pie" -> pure Pie
  "line" -> pure Line
  "bar" -> pure Bar
  _ -> Nothing

type ChartOptions =
  { roseType :: String
  , donutRatio :: Number
  , minimalAngle :: Number
  , smooth :: Boolean
  , symbolSize :: Number
  , barGap :: Number
  , xAxisPosition :: String
  , yAxisPosition :: String
  }

initialChartOptions :: ChartOptions
initialChartOptions =
  { roseType: "none"
  , donutRatio: 0
  , minimalAngle: 0
  , smooth: false
  , symbolSize: 0
  , barGap: 30
  , yAxisPosition: "left"
  , xAxisPosition: "bottom"
  }

_roseType :: forall a b r. Lens {roseType :: a | r} {roseType :: b | r} a b 
_roseType = lens _.roseType _{roseType = _}

_donutRatio :: forall a b r. Lens {donutRatio :: a | r} {donutRatio :: b |r} a b 
_donutRatio = lens _.donutRatio _{donutRatio = _} 

_minimalAngle :: forall a b r. Lens {minimalAngle :: a | r} {minimalAngle :: b|r} a b
_minimalAngle = lens _.minimalAngle _{minimalAngle = _}

_smooth :: forall a b r. Lens {smooth :: a | r} {smooth :: b | r} a b 
_smooth = lens _.smooth _{smooth = _}

_symbolSize :: forall a b r. Lens {symbolSize :: a | r} {symbolSize :: b | r} a b 
_symbolSize = lens _.symbolSize _{symbolSize = _}

_barGap :: forall a b r. Lens {barGap :: a | r} {barGap :: b | r} a b 
_barGap = lens _.barGap _{barGap = _}

_xAxisPosition :: forall a b r. Lens {xAxisPosition :: a | r} {xAxisPosition :: b | r} a b 
_xAxisPosition = lens _.xAxisPosition _{xAxisPosition = _} 

_yAxisPosition :: forall a b r. Lens {yAxisPosition :: a | r} {yAxisPosition :: b |r} a b 
_yAxisPosition = lens _.yAxisPosition _{yAxisPosition = _}

type VizRec =
  { output :: Option
  , sample :: Map JCursor Axis
  , all :: Map JCursor Axis 
  , error :: String
  , xs :: [JCursor]
  , ys :: [JCursor]
  , xCursor :: Maybe JCursor
  , yCursor :: Maybe JCursor
  , selectedFlag :: Boolean
  , availableChartTypes :: S.Set ChartType
  , chartType :: ChartType
  , chartOptions :: ChartOptions
  }

initialVizRec :: VizRec
initialVizRec =
  { output: Option optionDefault
  , sample: empty
  , all: empty
  , error: ""
  , xCursor: Nothing
  , yCursor: Nothing
  , xs: [ ]
  , ys: [ ]
  , selectedFlag: false
  , availableChartTypes: S.empty
  , chartType: Pie
  , chartOptions: initialChartOptions
  }

ixx :: VizRec -> Number
ixx r =
  maybe (-1) (\x -> findIndex (\y -> show y == show x) (keys r.all))  r.xCursor

ixy :: VizRec -> Number
ixy r =
  maybe (-1) (\x -> findIndex (\y -> show y == show x) (keys r.all)) r.yCursor


_output :: forall a b r. Lens {output :: a | r} {output :: b | r} a b 
_output = lens _.output _{output = _} 

_error :: forall a b r. Lens {error :: a | r} {error :: b |r} a b 
_error = lens _.error _{error = _}

_sample :: forall a b r. Lens {sample :: a |r} {sample :: b | r} a b 
_sample = lens _.sample _{sample = _}

_all :: forall a b r. Lens {all :: a | r} {all :: b |r} a b 
_all = lens _.all _{all = _}

_xs :: forall a b r. Lens {xs :: a | r} {xs :: b | r} a b 
_xs = lens _.xs _{xs = _} 

_ys :: forall a b r. Lens {ys :: a | r } {ys :: b | r} a b 
_ys = lens _.ys _{ys = _} 

_xCursor :: forall a b r. Lens {xCursor :: a | r} {xCursor :: b | r} a b 
_xCursor = lens _.xCursor _{xCursor = _} 

_yCursor :: forall a b r. Lens {yCursor :: a | r} {yCursor :: b | r} a b 
_yCursor = lens _.yCursor _{yCursor = _}

_selectedFlag :: forall a b r. Lens {selectedFlag :: a | r} {selectedFlag :: b |r} a b
_selectedFlag = lens _.selectedFlag _{selectedFlag = _}

_chartType :: forall a b r. Lens {chartType :: a | r} {chartType :: b | r} a b 
_chartType = lens _.chartType _{chartType = _}

_availableChartTypes :: forall a b r. Lens {availableChartTypes :: a | r} {availableChartTypes :: b | r} a b 
_availableChartTypes = lens _.availableChartTypes _{availableChartTypes = _}

_chartOptions :: forall a b r. Lens {chartOptions :: a | r} {chartOptions :: b |r} a b 
_chartOptions = lens _.chartOptions _{chartOptions = _}


