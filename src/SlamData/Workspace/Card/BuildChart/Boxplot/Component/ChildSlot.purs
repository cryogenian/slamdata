module SlamData.Workspace.Card.BuildChart.Boxplot.Component.ChildSlot where

import SlamData.Prelude

import Data.Argonaut (JCursor)

import SlamData.Workspace.Card.BuildChart.DimensionPicker.Component as DP

type ChildSlot = Unit

type ChildState = DP.StateP JCursor

type ChildQuery = DP.QueryP JCursor
