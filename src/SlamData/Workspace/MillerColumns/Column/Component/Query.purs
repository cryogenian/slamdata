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

module SlamData.Workspace.MillerColumns.Column.Component.Query
  ( Query(..)
  , Query'
  , Message(..)
  , Message'
  ) where

import SlamData.Prelude

import DOM.Node.Types (Element)

import Halogen.Component.Proxy (ProxyQ)

import SlamData.Workspace.MillerColumns.Column.Component.Item as Item

data Query a i o b
  = Init b
  | SetSelection (Maybe a) b
  | GetSelection (Maybe a → b)
  | Deselect b
  | HandleFilterChange String b
  | UpdateFilter String b
  | HandleScroll Element b
  | HandleMessage i (Item.Message' a o) b

type Query' a i o = ProxyQ (Query a i o) (Maybe a) (Message' a i o)

data Message a i
  = Initialized
  | Selected i a
  | Deselected

type Message' a i o = Either (Message a i) o
