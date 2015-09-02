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

module Model.Notebook.Cell.Viz where

import Prelude
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.Int (toNumber)
import ECharts.Chart
import ECharts.Options

import Data.Argonaut.JCursor (JCursor())
import Data.Map (Map(), empty, keys)
import Model.Notebook.ECharts (Semanthic(..), Axis(..), dependsOn)
import Data.Argonaut.JCursor (JCursor())
import Optic.Core 
import qualified Data.Set as S 
import Data.Argonaut.Core (JArray())
import Data.Array (findIndex, filter, head, sort, reverse, length)
import Data.Foldable (foldl)
import Data.Argonaut.Decode (DecodeJson, decodeJson)
import Data.Argonaut.Encode (EncodeJson)
import Data.Argonaut.Combinators ((~>), (:=), (.?))
import Data.Argonaut.Core (fromString, Json(), JArray(), jsonEmptyObject, toString)
import Data.Either
import Control.Alt ((<|>))
import Data.Selection

data ChartType
  = Pie
  | Line
  | Bar

chartType2str :: ChartType -> String
chartType2str Pie = "Pie"
chartType2str Line = "Line"
chartType2str Bar = "Bar"

instance chartTypeEq :: Eq ChartType where
  eq Pie Pie = true
  eq Line Line = true
  eq Bar Bar = true
  eq _ _ = false

instance chartTypeOrd :: Ord ChartType where
  compare Pie Pie = EQ
  compare Line Line = EQ
  compare Bar Bar = EQ 
  compare Pie _ = LT
  compare _ Pie = GT
  compare Line _ = LT
  compare _ Line = GT

instance encodeJsonChartType :: EncodeJson ChartType where
  encodeJson Pie = fromString "pie"
  encodeJson Line = fromString "line"
  encodeJson Bar = fromString "bar"

instance decodeJson :: DecodeJson ChartType where
  decodeJson json = maybe (Left "incorrect chart type string") Right do
    str <- toString json 
    str2chartType str

str2chartType :: String -> Maybe ChartType
str2chartType str = case str of
  "pie" -> pure Pie
  "line" -> pure Line
  "bar" -> pure Bar
  _ -> Nothing

type JSelection = Selection JCursor 

depends :: JSelection -> Array JCursor -> Array JCursor
depends sel lst = maybe lst go (sel ^._selection)
  where
  go y = filter (dependsOn y) lst

data Aggregation
  = Maximum 
  | Minimum  
  | Average 
  | Sum
  | Product


instance eqAggregation :: Eq Aggregation where
  eq Maximum Maximum = true
  eq Minimum Minimum = true
  eq Average Average = true
  eq Sum Sum = true
  eq Product Product = true
  eq _ _ = false


allAggregation :: Array Aggregation
allAggregation = [ Maximum
                 , Minimum
                 , Average
                 , Sum
                 , Product
                 ]

aggregation2str :: Aggregation -> String
aggregation2str Maximum = "⋀"
aggregation2str Minimum = "⋁"
aggregation2str Average = "μ"
aggregation2str Sum = "Σ"
aggregation2str Product = "Π"

str2aggregation :: String -> Maybe Aggregation
str2aggregation "⋀" = pure Maximum
str2aggregation "⋁" = pure Minimum
str2aggregation "μ" = pure Average
str2aggregation "Σ" = pure Sum
str2aggregation "Π" = pure Product
str2aggregation _ = Nothing

allAggregations :: Array Aggregation
allAggregations = [Maximum, Minimum, Average, Sum, Product]

aggregationDefault :: Aggregation
aggregationDefault = Sum

runAggregation :: Aggregation -> Array Number -> Number
runAggregation Maximum nums = fromMaybe zero $ head $ reverse (sort nums)
runAggregation Minimum nums = fromMaybe zero $ head (sort nums)
runAggregation Average nums = (foldl (+) zero nums) / (if length nums == 0
                                                       then 1.0
                                                       else toNumber $ length nums)
runAggregation Sum nums = foldl (+) zero nums
runAggregation Product nums = foldl (*) one nums

instance encodeJsonAggregateion :: EncodeJson Aggregation where 
  encodeJson = fromString <<< aggregation2str

instance decodeJsonAggregation :: DecodeJson Aggregation where
  decodeJson json = maybe (Left "incorrect aggregation string") Right do
    str <- toString json
    case str of
      "⋀" -> pure Maximum
      "⋁" -> pure Minimum
      "μ" -> pure Average
      "Σ" -> pure Sum
      "Π" -> pure Product
      _ -> Nothing 


newtype PieConfiguration = PieConfiguration
  { cats :: JSelection
  , firstMeasures :: JSelection
  , firstSeries :: JSelection
  , secondSeries :: JSelection
  , firstAggregation :: Aggregation
  }

initialPieConfiguration :: PieConfiguration
initialPieConfiguration = PieConfiguration
  { cats: initialSelection
  , firstMeasures: initialSelection
  , firstSeries: initialSelection
  , secondSeries: initialSelection
  , firstAggregation: aggregationDefault
  }

instance encodeJsonPieConfiguration :: EncodeJson PieConfiguration where
  encodeJson (PieConfiguration p) = "cats" := p.cats
                                    ~> "firstMeasures" := p.firstMeasures
                                    ~> "firstSeries" := p.firstSeries
                                    ~> "secondSeries" := p.secondSeries
                                    ~> "firstAggregation" := p.firstAggregation
                                    ~> jsonEmptyObject

instance decodeJsonPieConfiguration :: DecodeJson PieConfiguration where
  decodeJson json = do
    obj <- decodeJson json
    r <- { cats: _
         , firstMeasures: _
         , firstSeries: _
         , secondSeries: _
         , firstAggregation: _} <$>
         (obj .? "cats") <*>
         (obj .? "firstMeasures") <*>
         (obj .? "firstSeries") <*>
         (obj .? "secondSeries") <*>
         ((obj .? "firstAggregation") <|> pure Sum)
    pure $ PieConfiguration r

newtype LineConfiguration = LineConfiguration
  { dims :: JSelection
  , firstMeasures :: JSelection
  , secondMeasures :: JSelection
  , firstSeries :: JSelection
  , secondSeries :: JSelection
  , firstAggregation :: Aggregation
  , secondAggregation :: Aggregation
  }

initialLineConfiguration :: LineConfiguration
initialLineConfiguration = LineConfiguration
  { dims: initialSelection
  , firstMeasures: initialSelection
  , secondMeasures: initialSelection
  , firstSeries: initialSelection
  , secondSeries: initialSelection
  , firstAggregation: aggregationDefault
  , secondAggregation: aggregationDefault
  }

instance encodeJsonLineConfiguration :: EncodeJson LineConfiguration where
  encodeJson (LineConfiguration p) = "dims" := p.dims
                                     ~> "firstMeasures" := p.firstMeasures
                                     ~> "secondMeasures" := p.secondMeasures
                                     ~> "firstSeries" := p.firstSeries
                                     ~> "secondSeries" := p.secondSeries
                                     ~> "firstAggregation" := p.firstAggregation
                                     ~> "secondAggregation" := p.secondAggregation
                                     ~> jsonEmptyObject

instance decodeJsonLineConfiguration :: DecodeJson LineConfiguration where
  decodeJson json = do
    obj <- decodeJson json
    r <- { dims: _
         , firstMeasures: _
         , secondMeasures: _
         , firstSeries: _
         , secondSeries: _
         , firstAggregation: _
         , secondAggregation: _} <$>
         (obj .? "dims") <*>
         (obj .? "firstMeasures") <*>
         (obj .? "secondMeasures") <*> 
         (obj .? "firstSeries") <*>
         (obj .? "secondSeries") <*>
         ((obj .? "firstAggregation") <|> pure Sum) <*>
         ((obj .? "secondAggregation") <|> pure Sum)
    pure $ LineConfiguration r

newtype BarConfiguration = BarConfiguration
  { cats :: JSelection
  , firstMeasures :: JSelection
  , firstSeries :: JSelection
  , secondSeries :: JSelection
  , firstAggregation :: Aggregation
  }

initialBarConfiguration :: BarConfiguration
initialBarConfiguration = BarConfiguration 
  { cats: initialSelection
  , firstMeasures: initialSelection
  , firstSeries: initialSelection
  , secondSeries: initialSelection
  , firstAggregation: aggregationDefault
  }

instance encodeBarConfiguration :: EncodeJson BarConfiguration where
  encodeJson (BarConfiguration p) = "cats" := p.cats
                                   ~> "firstMeasures" := p.firstMeasures
                                   ~> "firstSeries" := p.firstSeries
                                   ~> "secondSeries" := p.secondSeries
                                   ~> "firstAggregation" := p.firstAggregation
                                   ~> jsonEmptyObject

instance decodeJsonBarConfiguration :: DecodeJson BarConfiguration where
  decodeJson json = do
    obj <- decodeJson json
    r <- { cats: _
         , firstMeasures: _
         , firstSeries: _
         , secondSeries: _
         , firstAggregation: _} <$>
         (obj .? "cats") <*>
         (obj .? "firstMeasures") <*>
         (obj .? "firstSeries") <*>
         (obj .? "secondSeries") <*>
         ((obj .? "firstAggregation") <|> pure Sum)
    pure $ BarConfiguration r


_BarConfiguration :: LensP BarConfiguration _
_BarConfiguration = lens (\(BarConfiguration obj) -> obj) (const BarConfiguration)

_LineConfiguraion :: LensP LineConfiguration _
_LineConfiguraion = lens (\(LineConfiguration obj) -> obj) (const LineConfiguration)

_PieConfiguration :: LensP PieConfiguration _
_PieConfiguration = lens (\(PieConfiguration obj) -> obj) (const PieConfiguration)
  
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



newtype VizRec = VizRec
  { output :: Option
  , all :: Map JCursor Axis
  , sample :: Map JCursor Axis
  , error :: String
  , availableChartTypes :: S.Set ChartType
  , chartType :: ChartType
    -- product because we don't need to drop selection when user choice
    -- another type of chart
  , pieConfiguration :: PieConfiguration
  , lineConfiguration :: LineConfiguration
  , barConfiguration :: BarConfiguration
  , chartHeight :: Int
  , chartWidth :: Int
  }

initialVizRec :: VizRec
initialVizRec = VizRec
  { output: Option optionDefault
  , all: empty
  , sample: empty
  , error: ""
  , availableChartTypes: S.empty
  , chartType: Pie
  , pieConfiguration: initialPieConfiguration
  , lineConfiguration: initialLineConfiguration
  , barConfiguration: initialBarConfiguration
  , chartHeight: 400
  , chartWidth: 600
  }

instance encodeJsonVizRec :: EncodeJson VizRec where
  encodeJson (VizRec r) = "sample" := r.sample
                          ~> "error" := r.error
                          ~> "availableChartTypes" := (S.toList r.availableChartTypes)
                          ~> "chartType" := r.chartType
                          ~> "pie" := r.pieConfiguration
                          ~> "line" := r.lineConfiguration
                          ~> "bar" := r.barConfiguration
                          ~> "height" := r.chartHeight
                          ~> "width" := r.chartWidth
                          ~> "output" := r.output
                          ~> jsonEmptyObject

instance decodeJsonVizRec :: DecodeJson VizRec where
  decodeJson json = do
    obj <- decodeJson json
    av <- obj .? "availableChartTypes"
    r <- { output: _
         , all: empty
         , sample: _
         , error: _
         , availableChartTypes: S.fromList av
         , chartType: _
         , pieConfiguration: _
         , lineConfiguration: _
         , barConfiguration: _
         , chartHeight: _
         , chartWidth: _} <$>
         ((obj .? "output") <|> (pure $ Option optionDefault)) <*>
         (obj .? "sample") <*>
         (obj .? "error") <*>
         (obj .? "chartType") <*>
         (obj .? "pie") <*>
         (obj .? "line") <*>
         (obj .? "bar") <*>
         ((obj .? "height") <|> pure 400) <*>
         ((obj .? "width") <|> pure 600)
    pure $ VizRec r
    


_VizRec :: LensP VizRec _
_VizRec = lens (\(VizRec o) -> o) (const VizRec)

_output :: LensP VizRec Option
_output = _VizRec <<< lens _.output _{output = _} 

_error :: LensP VizRec String 
_error = _VizRec <<< lens _.error _{error = _}

_sample :: LensP VizRec (Map JCursor Axis)
_sample = _VizRec <<< lens _.sample _{sample = _}

_all :: LensP VizRec (Map JCursor Axis) 
_all = _VizRec <<< lens _.all _{all = _}

_chartType :: LensP VizRec ChartType
_chartType = _VizRec <<< lens _.chartType _{chartType = _}

_chartHeight :: LensP VizRec Int
_chartHeight = _VizRec <<< lens _.chartHeight _{chartHeight = _}

_chartWidth :: LensP VizRec Int
_chartWidth = _VizRec <<< lens _.chartWidth _{chartWidth = _}

_availableChartTypes :: LensP VizRec (S.Set ChartType)
_availableChartTypes = _VizRec <<< lens _.availableChartTypes _{availableChartTypes = _}


_pieConfiguration :: LensP VizRec _ 
_pieConfiguration = _VizRec <<<
                    (lens _.pieConfiguration _{pieConfiguration = _} ) <<<
                    _PieConfiguration

_lineConfiguration :: LensP VizRec _ 
_lineConfiguration = _VizRec <<<
                     (lens _.lineConfiguration _{lineConfiguration = _} ) <<<
                     _LineConfiguraion

_barConfiguration :: LensP VizRec _ 
_barConfiguration = _VizRec <<<
                    (lens _.barConfiguration _{barConfiguration = _}) <<<
                    _BarConfiguration
