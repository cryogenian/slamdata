{-
Copyright 2016 SlamData, Inc.

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

module SlamData.Notebook.Card.Download.Component.State where

import SlamData.Prelude

import Data.Argonaut (Json, (:=), (~>), (.?), decodeJson, jsonEmptyObject)
import Data.Lens (LensP, lens)
import Data.Path.Pathy as Pathy

import SlamData.Download.Model as D

import Utils.Path as PU

type State =
  { compress :: Boolean
  , options :: Either D.CSVOptions D.JSONOptions
  , source :: Maybe PU.FilePath
  }

initialState :: State
initialState =
  { compress: false
  , options: Left D.initialCSVOptions
  , source: Nothing
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
  ~> "source" := (Pathy.printPath <$> s.source)
  ~> jsonEmptyObject

decode :: Json -> Either String State
decode = decodeJson >=> \obj -> do
  compress ← obj .? "compress"
  options ← obj .? "options"
  source ← traverse parsePath =<< obj .? "source"
  pure { compress, options, source }

parsePath ∷ String → Either String PU.FilePath
parsePath =
  maybe (Left "could not parse source file path") Right <<< PU.parseFilePath
