module Halide.Logo where

import Halogen.HTML (a, text, HTML(..)) 
import Halogen.HTML.Attributes 
import Config

view :: forall a u i. a -> HTML u i
view _ =
  a ((class_ $ className "navbar-brand") <>
     href Config.slamDataHome) [
    text "SlamData"
    ]
