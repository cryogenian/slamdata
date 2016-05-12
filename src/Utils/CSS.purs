module Utils.CSS where

import Prelude
import CSS (CSS)
import CSS.String (fromString)
import CSS.Stylesheet (key)

transition :: String -> CSS
transition = key (fromString "transition")

calc :: String -> String
calc s = "calc(" ++ s ++ ")"

width :: String -> CSS
width = key (fromString "width")

transform :: String -> CSS
transform = key (fromString "transform")

translate3d :: String -> String -> String -> String
translate3d x y z = "translate3d(" ++ x ++ "," ++ y ++ "," ++ z ++ ")"
