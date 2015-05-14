module Model.File.Search where

import Data.Maybe
import Control.Timer

-- | State of search field
type Search = {
  valid :: Boolean,
  focused :: Boolean,
  value :: String,
  -- if _value_ has been changed but path hasn't been setted
  timeout :: Maybe Timeout,
  loading :: Boolean
  }

initialSearch :: Search
initialSearch = {
  valid : true,
  value : "",
  focused: false,
  timeout : Nothing,
  loading: false
  }
