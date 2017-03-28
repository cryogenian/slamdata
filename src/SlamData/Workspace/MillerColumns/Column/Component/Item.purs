{-
Copyright 2016 SlamData, Inc.

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

module SlamData.Workspace.MillerColumns.Column.Component.Item where

import SlamData.Prelude

import Halogen.Component.Proxy (ProxyQ)

type Query a o = ProxyQ (Const Void) State (Message' a o)

data Message a = RaisePopulate a

type Message' a o = Either (Message a) o

data State = Selected | Deselected

derive instance eqState ∷ Eq State
derive instance ordState ∷ Ord State
