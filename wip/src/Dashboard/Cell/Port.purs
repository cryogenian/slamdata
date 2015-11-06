module Notebook.Cell.Port (Port(..)) where

import Data.StrMap (StrMap())

import Text.Markdown.SlamDown (SlamDown())
import Text.Markdown.SlamDown.Html (FormFieldValue())

data Port
  = SlamDown SlamDown
  | VarMap (StrMap FormFieldValue)
  | Closed

