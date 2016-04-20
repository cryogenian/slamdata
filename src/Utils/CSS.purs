module Utils.CSS where

import Prelude
import CSS (CSS)
import CSS.Property (class Val, Value, value)
import CSS.String (fromString)
import CSS.Stylesheet (key)

transition :: String -> CSS
transition = key (fromString "transition")
