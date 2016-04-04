module Utils.CSS where

import Prelude
import CSS (CSS)
import CSS.Property (class Val, Value, value)
import CSS.String (fromString)
import CSS.Stylesheet (key)

--newtype Transition = Transition Value
--
--instance valTransition :: Val Transition where
--  value (Transition v) = fromString v

transition :: String -> CSS
transition = key (fromString "transition")
