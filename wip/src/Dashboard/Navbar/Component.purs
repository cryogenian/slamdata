{-
Copyright 2015 SlamData, Inc.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
-}

module Dashboard.Navbar.Component where

import Prelude

import Notebook.Common (Slam())
import Halogen
import Halogen.HTML as H
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
