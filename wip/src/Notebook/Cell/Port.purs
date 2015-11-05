module Notebook.Cell.Port (Port(..), _SlamDown) where

import Data.Lens (PrismP(), prism')
import Data.Maybe (Maybe(..))
import Data.StrMap (StrMap())

import Text.Markdown.SlamDown (SlamDown())
import Text.Markdown.SlamDown.Html (FormFieldValue())

data Port
  = SlamDown SlamDown
  | VarMap (StrMap FormFieldValue)

_SlamDown :: PrismP Port SlamDown
_SlamDown = prism' SlamDown \v -> case v of
  SlamDown sd -> Just sd
  _ -> Nothing
