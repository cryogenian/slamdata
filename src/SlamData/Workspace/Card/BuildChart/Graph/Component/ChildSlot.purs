module SlamData.Workspace.Card.BuildChart.Graph.Component.ChildSlot where

import SlamData.Prelude

import SlamData.Workspace.Card.BuildChart.DimensionPicker.Component as DP
import SlamData.Workspace.Card.BuildChart.DimensionPicker.JCursor (JCursorNode)

type ChildSlot = Unit

type ChildState = DP.StateP JCursorNode

type ChildQuery = DP.QueryP JCursorNode
