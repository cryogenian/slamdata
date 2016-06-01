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

module SlamData.Workspace.Card.JTable.Component
  ( jtableComponent
  , queryShouldRun
  , module SlamData.Workspace.Card.JTable.Component.Query
  , module JTS
  ) where

import SlamData.Prelude

import Control.Monad.Eff.Exception as Exn
import Control.Monad.Error.Class (throwError)

import Data.Argonaut.Core as JSON
import Data.Int as Int
import Data.Lens ((.~), (?~))

import Halogen as H

import SlamData.Effects (Slam)
import SlamData.Quasar.Query as Quasar
import SlamData.Workspace.Card.CardType as Ct
import SlamData.Workspace.Card.Common.EvalQuery as CEQ
import SlamData.Workspace.Card.Component (CardQueryP, CardStateP, makeCardComponent, makeQueryPrism, _JTableState, _JTableQuery)
import SlamData.Workspace.Card.Eval as Eval
import SlamData.Workspace.Card.JTable.Component.Query (QueryP, PageStep(..), Query(..))
import SlamData.Workspace.Card.JTable.Component.Render (render)
import SlamData.Workspace.Card.JTable.Component.State as JTS
import SlamData.Workspace.Card.JTable.Model as Model
import SlamData.Workspace.Card.Port as Port

jtableComponent ∷ H.Component CardStateP CardQueryP Slam
jtableComponent = makeCardComponent
  { cardType: Ct.JTable
  , component: H.component { render, eval: coproduct evalCard evalJTable }
  , initialState: JTS.initialState
  , _State: _JTableState
  , _Query: makeQueryPrism _JTableQuery
  }

queryShouldRun ∷ ∀ a. QueryP a → Boolean
queryShouldRun = coproduct (const false) pred
  where
  pred (StepPage _ _) = true
  pred (ChangePageSize _ _) = true
  pred _ = false

-- | Evaluates generic card queries.
evalCard ∷ Natural CEQ.CardEvalQuery (H.ComponentDSL JTS.State QueryP Slam)
evalCard (CEQ.NotifyRunCard next) = pure next
evalCard (CEQ.NotifyStopCard next) = pure next
evalCard (CEQ.EvalCard input output next) = do
-- TODO: check this -js
  for_ output \port →
    CEQ.runCardEvalT $ runTable port $> port
  pure next
--  k <$> CEQ.runCardEvalT do
--    port ← Eval.evalCard input Eval.Pass
--    runTable port $> port
evalCard (CEQ.SetupCard _ next) = pure next
evalCard (CEQ.Save k) =
  pure ∘ k =<< H.gets (Model.encode ∘ JTS.toModel)
evalCard (CEQ.Load json next) = do
  either (const (pure unit)) H.set $ JTS.fromModel <$> Model.decode json
  pure next
evalCard (CEQ.SetCanceler _ next) = pure next
evalCard (CEQ.SetDimensions _ next) = pure next

runTable
  ∷ Port.Port
  → CEQ.CardEvalT (H.ComponentDSL JTS.State QueryP Slam) Unit
runTable = case _ of
  Port.TaggedResource { tag, resource } → do
    oldInput ← lift $ H.gets _.input
    when (((oldInput <#> _.resource) ≠ pure resource) || ((oldInput >>= _.tag) ≠ tag))
      $ lift $ resetState

    size ←
      lift (Quasar.count resource)
        >>= either (throwError ∘ Exn.message) pure

    lift $ H.modify $ JTS._input ?~ { resource, size, tag }
    p ← lift $ H.gets JTS.pendingPageInfo

    items ←
      lift (Quasar.sample resource ((p.page - 1) * p.pageSize) p.pageSize)
        >>= either (throwError ∘ Exn.message) pure

    lift $
      H.modify
        $ (JTS._isEnteringPageSize .~ false)
        ∘ (JTS._result ?~
             { json: JSON.fromArray items
             , page: p.page
             , pageSize: p.pageSize
             })

  Port.Blocked → lift $ resetState

  _ → throwError "Expected a TaggedResource input"

-- | Resets the state while preserving settings like page size.
resetState ∷ H.ComponentDSL JTS.State QueryP Slam Unit
resetState = H.modify (JTS._result .~ Nothing)

-- | Evaluates jtable-specific card queries.
evalJTable ∷ Natural Query (H.ComponentDSL JTS.State QueryP Slam)
evalJTable (StepPage step next) =
  H.modify (JTS.stepPage step) $> next
evalJTable (ChangePageSize pageSize next) =
  maybe (pure unit) (H.modify ∘ JTS.resizePage) (Int.fromString pageSize) $> next
evalJTable (StartEnterCustomPageSize next) =
  H.modify (JTS._isEnteringPageSize .~ true) $> next
evalJTable (SetCustomPageSize size next) =
  H.modify (JTS.setPageSize size) $> next
evalJTable (SetCustomPage page next) =
  H.modify (JTS.setPage page) $> next
