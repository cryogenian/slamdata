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

module SlamData.Workspace.Card.BuildChart.Graph.Component.ChildSlot where

import SlamData.Prelude

import SlamData.Workspace.Card.BuildChart.DimensionPicker.Component as DP
import SlamData.Workspace.Card.BuildChart.DimensionPicker.JCursor (JCursorNode)

type ChildSlot = Unit

type ChildState = DP.StateP JCursorNode

type ChildQuery = DP.QueryP JCursorNode
