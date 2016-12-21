module SlamData.Workspace.Card.SetupFormInput.TextLike.Component.Query where

import SlamData.Prelude

import Data.Argonaut (JCursor)

import Halogen as H

import SlamData.Workspace.Card.Common.EvalQuery (CardEvalQuery)
import SlamData.Workspace.Card.BuildChart.Inputs (SelectAction)
import SlamData.Workspace.Card.SetupFormInput.TextLike.Component.ChildSlot (ChildQuery, ChildSlot)

data Selection f
  = Value (f JCursor)

data Query a
  = Select (Selection SelectAction) a
  | UpdateName String a

type QueryC = CardEvalQuery ⨁ Query
type QueryP = QueryC ⨁ H.ChildF ChildSlot ChildQuery
