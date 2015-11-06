module Dashboard.Navbar.Component where

import Prelude

import Notebook.Common (Slam())
import Halogen
import Halogen.HTML as H
import Halogen.HTML.Elements as H
import Halogen.Themes.Bootstrap3 as B
import Halogen.HTML.Properties as P

newtype StateP = StateP Int
data QueryP a = QueryP a

initialState :: StateP
initialState = StateP 0

comp :: Component StateP QueryP Slam
comp = component render eval

render :: StateP -> ComponentHTML QueryP
render state =
  H.div [ P.classes [ B.clearfix ] ]  [ H.p_ [ H.text "Navbar" ] ]

eval :: Eval QueryP StateP QueryP Slam
eval (QueryP next) = pure next
