module SlamData.Notebook.Card.DownloadOptions.Component.State where

import SlamData.Prelude

import Data.Argonaut (Json, (:=), (~>), (.?), decodeJson, jsonEmptyObject)
import Data.Lens (LensP, lens)
import Data.Path.Pathy as Pathy
import SlamData.Download.Model as D

import Utils.Path as PU

type State =
  { compress ∷ Boolean
  , options ∷ Either D.CSVOptions D.JSONOptions
  , source ∷ Maybe PU.FilePath
  }

initialState ∷ State
initialState =
  { compress: false
  , options: Left D.initialCSVOptions
  , source: Nothing
  }

_compress ∷ ∀ a r. LensP {compress ∷ a|r} a
_compress = lens (_.compress) (_{compress = _ })

_options ∷ ∀ a r. LensP {options ∷ a|r} a
_options = lens (_.options) (_{options = _})

_source ∷ ∀ a r. LensP {source ∷ a|r} a
_source = lens (_.source) (_{source = _})


encode :: State -> Json
encode s
   = "compress" := s.compress
  ~> "options" := s.options
  ~> "source" := (Pathy.printPath <$> s.source)
  ~> jsonEmptyObject

decode :: Json → Either String State
decode = decodeJson >=> \obj → do
  compress ← obj .? "compress"
  options ← obj .? "options"
  source ← traverse parsePath =<< obj .? "source"
  pure { compress, options, source }

parsePath ∷ String → Either String PU.FilePath
parsePath =
  maybe (Left "could not parse source file path") Right ∘ PU.parseFilePath
