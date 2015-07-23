module Model.File.Search
  ( Search()
  , SearchRec()
  , initialSearch
  , _valid
  , _focused
  , _value
  , _loading
  , _timeout
  ) where

import Prelude
import Control.Timer (Timeout())
import Data.Maybe (Maybe(..))
import Data.These (These(..))

import Optic.Core 

-- | State of search field
newtype Search = Search SearchRec

type SearchRec =
  { valid :: Boolean
  , focused :: Boolean
  , value :: These String String
  , loading :: Boolean
  , timeout :: Maybe Timeout
  }

initialSearch :: String -> Search
initialSearch value = Search
  { valid: true
  , value: This value
  , focused: false
  , loading: true
  , timeout: Nothing
  }

_Search :: LensP Search SearchRec
_Search  = lens (\(Search obj) -> obj) (const Search)

_valid :: LensP Search Boolean
_valid = _Search .. lens _.valid _{valid = _}

_focused :: LensP Search Boolean
_focused = _Search .. lens _.focused _{focused = _}

_value :: LensP Search (These String String)
_value = _Search .. lens _.value _{value = _}

_loading :: LensP Search Boolean
_loading = _Search .. lens _.loading _{loading = _}

_timeout :: LensP Search (Maybe Timeout)
_timeout = _Search .. lens _.timeout _{timeout = _}
