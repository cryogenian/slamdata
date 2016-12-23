module SlamData.Workspace.Card.FormInput.Component.Query where

import SlamData.Prelude

import Halogen (ChildF)

import SlamData.Workspace.Card.FormInput.Component.ChildSlot (ChildQuery, ChildSlot)
import SlamData.Workspace.Card.Common.EvalQuery (CardEvalQuery)

type QueryP = CardEvalQuery ⨁ ChildF ChildSlot ChildQuery
