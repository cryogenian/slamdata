module Model.Notebook.Cell.Viz where

import Data.Maybe 
import Data.Monoid.First 
import ECharts.Chart
import ECharts.Options

import Optic.Core (Lens(), lens)

import Data.Argonaut.Core (JArray()) 

type VizRec =
  { output :: Option
  , miniatures :: [Option]
  , sample :: JArray
  , error :: First String
  , baseSelected :: Boolean
  , baseOptions :: [Option]
  }

initialVizRec :: VizRec
initialVizRec =
  { output: Option optionDefault
  , miniatures: [ ]
  , sample: [ ]
  , error: First Nothing
  , baseSelected: false
  , baseOptions: []
  }

miniatureLabel :: String
miniatureLabel = "miniature"

baseOptsLabel :: String
baseOptsLabel = "baseOpts"

resultLabel :: String
resultLabel = "result"

_output :: forall a b r. Lens {output :: a | r} {output :: b | r} a b 
_output = lens _.output _{output = _} 

_miniatures :: forall a b r. Lens {miniatures :: a | r} {miniatures :: b | r} a b 
_miniatures = lens _.miniatures _{miniatures = _}

_error :: forall a b r. Lens {error :: a | r} {error :: b |r} a b 
_error = lens _.error _{error = _}

_baseSelected :: forall a b r. Lens {baseSelected :: a | r} {baseSelected :: b |r} a b
_baseSelected = lens _.baseSelected _{baseSelected = _}

_baseOptions :: forall a b r. Lens {baseOptions :: a | r} {baseOptions :: b | r} a b 
_baseOptions = lens _.baseOptions _{baseOptions = _}
