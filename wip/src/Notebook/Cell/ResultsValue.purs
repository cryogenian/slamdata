module Notebook.Cell.ResultsValue (ResultsValue(..), _SlamDown) where

import Data.Lens (PrismP(), prism')
import Data.Maybe (Maybe(..))

import Text.Markdown.SlamDown (SlamDown())

data ResultsValue
  = SlamDown SlamDown

_SlamDown :: PrismP ResultsValue SlamDown
_SlamDown = prism' SlamDown \v -> case v of
  SlamDown sd -> Just sd
  _ -> Nothing
