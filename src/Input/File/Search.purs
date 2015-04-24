module Input.File.Search
  ( SearchInput(..)
  , inputSearch
  ) where

import Control.Timer (Timeout())
import Data.Either (either)
import Data.Maybe (Maybe(..))
import Model.Search (Search())
import Text.SlamSearch (mkQuery)
import Text.SlamSearch.Printer (strQuery)

data SearchInput
  = SearchValidation Boolean
  | SearchSet String
  | SearchTimeout Timeout
  | SearchNextValue String

inputSearch :: Search -> SearchInput -> Search
inputSearch state input = case input of

  SearchValidation v ->
    state { valid = v }

  SearchSet s ->
    let nq = either (const "") id (strQuery <$> mkQuery state.nextValue)
    in if (s /= "" && nq /= "" &&  nq == s)
       then state { value = state.nextValue }
       else state { value = s, nextValue = s }

  SearchTimeout t ->
    state { timeout = Just t }

  SearchNextValue next ->
    state { nextValue = next }
