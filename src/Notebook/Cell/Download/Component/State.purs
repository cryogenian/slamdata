module Notebook.Cell.Download.Component.State where

import Prelude

import Control.Bind ((>=>))
import Data.Argonaut (Json(), (:=), (~>), (.?), decodeJson, jsonEmptyObject)
import Data.Either
import Data.Lens (LensP(), lens)
import Model.Resource (Resource(), root)
import Model.Download as D

type State =
  { compress :: Boolean
  , options :: Either D.CSVOptions D.JSONOptions
  , source :: Resource
  }

initialState :: State
initialState =
  { compress: false
  , options: Left D.initialCSVOptions
  , source: root
  }


_compress :: forall a r. LensP {compress :: a|r} a
_compress = lens _.compress _{compress = _}

_options :: forall a r. LensP {options :: a|r} a
_options = lens _.options _{options = _}

_source :: forall a r. LensP {source :: a|r} a
_source = lens _.source _{source = _}

encode :: State -> Json
encode s
   = "compress" := s.compress
  ~> "options" := s.options
  ~> "source" := s.source
  ~> jsonEmptyObject

decode :: Json -> Either String State
decode = decodeJson >=> \obj ->
  { compress: _, options: _, source: _ }
    <$> obj .? "compress"
    <*> obj .? "options"
    <*> obj .? "source"
