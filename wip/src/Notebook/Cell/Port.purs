module Notebook.Cell.Port (Port(..)) where

import Data.StrMap (StrMap())

import Text.Markdown.SlamDown.Html (FormFieldValue())

data Port
  = VarMap (StrMap FormFieldValue)
  | Closed

