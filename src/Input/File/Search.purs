module Input.File.Search
  ( SearchInput(..)
  , inputSearch
  ) where

import Control.Timer (Timeout())
import Data.Either (either)
import Data.Maybe (Maybe(..))
import Model.File.Search (Search())
import Text.SlamSearch (mkQuery)
import Text.SlamSearch.Printer (strQuery)

data SearchInput
  = SearchValidation Boolean
  | SearchSet String
  | SearchTimeout Timeout

inputSearch :: Search -> SearchInput -> Search
inputSearch state input = case input of

  SearchValidation v ->
    state { valid = v }

  SearchSet s ->
    state { value = s }

  SearchTimeout t ->
    state { timeout = Just t }
