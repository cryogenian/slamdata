module Dashboard.Navbar where

import Prelude

import Dashboard.Common (Slam())
import Halogen
import Halogen.HTML as H
import Halogen.HTML.Elements as H

newtype StateP = StateP Int
data QueryP a = QueryP a

initialState :: StateP
initialState = StateP 0

comp :: Component StateP QueryP Slam
comp = component render eval

render :: StateP -> ComponentHTML QueryP
render state = H.p_ [ H.text "Navbar" ]

eval :: Eval QueryP StateP QueryP Slam
eval (QueryP next) = pure next
