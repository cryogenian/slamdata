module Model.Notebook.Cell.Viz where

import Data.Maybe (Maybe(..), maybe, fromMaybe)
import ECharts.Chart
import ECharts.Options

import Data.Argonaut.JCursor (JCursor())
import Data.Map (Map(), empty, keys)
import Model.Notebook.ECharts (Semanthic(..), Axis(..), dependsOn)
import Data.Argonaut.JCursor (JCursor())
import Optic.Core (Lens(), lens)
import qualified Data.Set as S 
import Data.Argonaut.Core (JArray())
import Data.Array (findIndex, filter, head, sort, reverse, length)
import Data.Foldable (foldl)




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


type Selection a =
  { variants :: [a]
  , selection :: Maybe a
  }

_variants :: forall a b r. Lens {variants :: a | r} {variants :: b |r} a b 
_variants = lens _.variants _{variants = _}

_selection :: forall a b r. Lens {selection :: a | r} {selection :: b |r} a b 
_selection = lens _.selection _{selection = _} 

initialSelection :: forall a. Selection a
initialSelection = {variants: [], selection: Nothing}

newSelection :: forall a. [a] -> Selection a
newSelection as = {variants: as, selection: Nothing}

except :: forall a. (Eq a) => Selection a -> Selection a -> Selection a
except sel sel' = sel{variants = except' sel.variants sel'}

except' :: forall a. (Eq a) => [a] -> Selection a -> [a]
except' lst sel = filter (\x -> Just x /= sel.selection) lst

type JSelection = Selection JCursor 

depends :: JSelection -> [JCursor] -> [JCursor]
depends sel lst = maybe lst go sel.selection
  where
  go y = filter (dependsOn y) lst

data Aggregation
  = Maximum 
  | Minimum  
  | Average 
  | Sum
  | Product

aggregation2str :: Aggregation -> String
aggregation2str Maximum = "⋀"
aggregation2str Minimum = "⋁"
aggregation2str Average = "μ"
aggregation2str Sum = "Σ"
aggregation2str Product = "Π"

allAggregation :: [Aggregation]
allAggregation = [Maximum, Minimum, Average, Sum, Product]

aggregationDefault :: Aggregation
aggregationDefault = Sum

runAggregation :: Aggregation -> [Number] -> Number
runAggregation Maximum nums = fromMaybe 0 $ head $ reverse (sort nums)
runAggregation Minimum nums = fromMaybe 0 $ head (sort nums)
runAggregation Average nums = (foldl (+) 0 nums) / (length nums)
runAggregation Sum nums = foldl (+) 0 nums
runAggregation Product nums = foldl (*) 1 nums 


type PieConfiguration =
  { cats :: JSelection
  , firstMeasures :: JSelection
  , firstSeries :: JSelection
  , secondSeries :: JSelection
  , firstAggregation :: Aggregation
  }

initialPieConfiguration :: PieConfiguration
initialPieConfiguration =
  { cats: initialSelection
  , firstMeasures: initialSelection
  , firstSeries: initialSelection
  , secondSeries: initialSelection
  , firstAggregation: aggregationDefault
  }

type LineConfiguration =
  { dims :: JSelection
  , firstMeasures :: JSelection
  , secondMeasures :: JSelection
  , firstSeries :: JSelection
  , secondSeries :: JSelection
  , firstAggregation :: Aggregation
  , secondAggregation :: Aggregation
  }

initialLineConfiguration :: LineConfiguration
initialLineConfiguration =
  { dims: initialSelection
  , firstMeasures: initialSelection
  , secondMeasures: initialSelection
  , firstSeries: initialSelection
  , secondSeries: initialSelection
  , firstAggregation: aggregationDefault
  , secondAggregation: aggregationDefault
  }

type BarConfiguration = PieConfiguration

initialBarConfiguration :: BarConfiguration
initialBarConfiguration = initialPieConfiguration 

_cats :: forall a b r. Lens {cats::a|r} {cats::b|r} a b 
_cats = lens _.cats _{cats = _}

_dims :: forall a b r. Lens {dims::a|r} {dims::b|r} a b
_dims = lens _.dims _{dims = _} 

_firstMeasures :: forall a b r. Lens {firstMeasures :: a |r} {firstMeasures :: b |r} a b 
_firstMeasures = lens _.firstMeasures _{firstMeasures = _}

_secondMeasures :: forall a b r. Lens {secondMeasures :: a |r} {secondMeasures :: b |r} a b
_secondMeasures = lens _.secondMeasures _{secondMeasures = _}

_firstSeries :: forall a b r. Lens {firstSeries :: a |r} {firstSeries :: b |r} a b 
_firstSeries = lens _.firstSeries _{firstSeries = _} 

_secondSeries :: forall a b r. Lens {secondSeries :: a|r} {secondSeries :: b|r} a b 
_secondSeries = lens _.secondSeries _{secondSeries = _}

--_firstAggregation :: forall a b r. Lens {firstAggregation :: a |r} {firstAggregation :: b | r} a b 
_firstAggregation = lens _.firstAggregation _{firstAggregation = _} 

--_secondAggregation :: forall a b r. Lens {secondAggregation :: a } {secondAggregation :: b | r} a b
_secondAggregation = lens _.secondAggregation _{secondAggregation = _} 



type VizRec =
  { output :: Option
  , all :: Map JCursor Axis
  , sample :: Map JCursor Axis
  , error :: String
  , selectedFlag :: Boolean
  , availableChartTypes :: S.Set ChartType
  , chartType :: ChartType
    -- product because we don't need to drop selection when user choice
    -- another type of chart
  , pieConfiguration :: PieConfiguration
  , lineConfiguration :: LineConfiguration
  , barConfiguration :: BarConfiguration 
  }

initialVizRec :: VizRec
initialVizRec =
  { output: Option optionDefault
  , all: empty
  , sample: empty
  , error: ""
  , selectedFlag: false
  , availableChartTypes: S.empty
  , chartType: Pie
  , pieConfiguration: initialPieConfiguration
  , lineConfiguration: initialLineConfiguration
  , barConfiguration: initialBarConfiguration
  }

_output :: forall a b r. Lens {output :: a | r} {output :: b | r} a b 
_output = lens _.output _{output = _} 

_error :: forall a b r. Lens {error :: a | r} {error :: b |r} a b 
_error = lens _.error _{error = _}

_sample :: forall a b r. Lens {sample :: a |r} {sample :: b | r} a b 
_sample = lens _.sample _{sample = _}

_all :: forall a b r. Lens {all :: a | r} {all :: b |r} a b 
_all = lens _.all _{all = _}


_selectedFlag :: forall a b r. Lens {selectedFlag :: a | r} {selectedFlag :: b |r} a b
_selectedFlag = lens _.selectedFlag _{selectedFlag = _}

_chartType :: forall a b r. Lens {chartType :: a | r} {chartType :: b | r} a b 
_chartType = lens _.chartType _{chartType = _}

_availableChartTypes :: forall a b r. Lens {availableChartTypes :: a | r} {availableChartTypes :: b | r} a b 
_availableChartTypes = lens _.availableChartTypes _{availableChartTypes = _}


_pieConfiguration :: forall a b r. Lens {pieConfiguration :: a |r} {pieConfiguration :: b|r} a b 
_pieConfiguration = lens _.pieConfiguration _{pieConfiguration = _} 

_lineConfiguration :: forall a b r. Lens {lineConfiguration :: a |r} {lineConfiguration :: b|r} a b 
_lineConfiguration = lens _.lineConfiguration _{lineConfiguration = _} 

_barConfiguration :: forall a b r. Lens {barConfiguration :: a |r} {barConfiguration :: b |r} a b 
_barConfiguration = lens _.barConfiguration _{barConfiguration = _}
