module Model.Notebook.Cell.Viz where

import Data.Maybe (Maybe(..))
import ECharts.Chart
import ECharts.Options

import Data.Argonaut.JCursor (JCursor())
import Data.Map (Map(), empty)
import Model.Notebook.ECharts (Semanthic(..), Axis(..))
import Data.Argonaut.JCursor (JCursor())
import Optic.Core (Lens(), lens)

import Data.Argonaut.Core (JArray()) 

data ChartType
  = Pie
  | Line
  | Bar
  

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
  , chartType :: ChartType
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
  , chartType: Pie
  }

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
