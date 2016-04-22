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

module SlamData.Notebook.Card.JTable.Component
  ( jtableComponent
  , queryShouldRun
  , module SlamData.Notebook.Card.JTable.Component.Query
  , module SlamData.Notebook.Card.JTable.Component.State
  ) where

import SlamData.Prelude

import Control.Monad.Eff.Exception (message)
import Control.Monad.Except.Trans (ExceptT(..), runExceptT)

import Data.Argonaut.Core as JSON
import Data.Int as Int
import Data.Lens ((.~), (?~))

import Halogen as H

import SlamData.Effects (Slam)
import SlamData.Notebook.Card.CardType as Ct
import SlamData.Notebook.Card.Common.EvalQuery (CardEvalQuery(..), CardEvalResult)
import SlamData.Notebook.Card.Component (CardQueryP, CardStateP, makeCardComponent, makeQueryPrism, _JTableState, _JTableQuery)
import SlamData.Notebook.Card.JTable.Component.Query (QueryP, PageStep(..), Query(..))
import SlamData.Notebook.Card.JTable.Component.Render (render)
import SlamData.Notebook.Card.JTable.Component.State (Input, PageInfo, State, _input, _isEnteringPageSize, _page, _pageSize, _resource, _result, _size, currentPageInfo, fromModel, initialState, pendingPageInfo, resizePage, setPage, setPageSize, stepPage, toModel)
import SlamData.Notebook.Card.JTable.Model as Model
import SlamData.Notebook.Card.Port (Port(..))
import SlamData.Quasar.Query as Quasar

jtableComponent ∷ H.Component CardStateP CardQueryP Slam
jtableComponent = makeCardComponent
  { cardType: Ct.JTable
  , component: H.component { render, eval: coproduct evalCard evalJTable }
  , initialState: initialState
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
evalCard ∷ Natural CardEvalQuery (H.ComponentDSL State QueryP Slam)
evalCard (NotifyRunCard next) =
  pure next
evalCard (NotifyStopCard next) =
  pure next
evalCard (EvalCard value k) =
  case value.inputPort of
    Just (TaggedResource { tag, resource }) → do
      done ← runExceptT do
        oldInput ← lift $ H.gets _.input

        when (((oldInput <#> _.resource) ≠ pure resource)
              || ((oldInput >>= _.tag) ≠ tag))
          $ lift $ H.set initialState

        size ← ExceptT $ Quasar.count resource

        lift $ H.modify $ _input ?~ { resource, size, tag }

        p ← lift $ H.gets pendingPageInfo

        items ← ExceptT
          $ Quasar.sample
              resource
              ((p.page - 1) * p.pageSize)
              p.pageSize

        lift $ H.modify
          $ (_isEnteringPageSize .~ false)
          ∘ (_result ?~
                { json: JSON.fromArray items
                , page: p.page
                , pageSize: p.pageSize
                })
      case done of
        Left err → pure $ k $ error (message err)
        Right _ → pure $ k (result value.inputPort)

    Just Blocked → do
      H.set initialState
      pure $ k (result Nothing)

    _ → pure $ k (error "expected a Resource input")

  where
  result ∷ Maybe Port → CardEvalResult
  result = { output: _, messages: [] }

  error ∷ String → CardEvalResult
  error msg =
    { output: Nothing
    , messages: [Left $ "An internal error occurred: " ⊕ msg]
    }
evalCard (SetupCard _ next) = pure next
evalCard (Save k) =
  pure ∘ k =<< H.gets (Model.encode ∘ toModel)
evalCard (Load json next) = do
  either (const (pure unit)) H.set $ fromModel <$> Model.decode json
  pure next
evalCard (SetCanceler _ next) = pure next

-- | Evaluates jtable-specific card queries.
evalJTable ∷ Natural Query (H.ComponentDSL State QueryP Slam)
evalJTable (StepPage step next) =
  H.modify (stepPage step) $> next
evalJTable (ChangePageSize pageSize next) =
  maybe (pure unit) (H.modify ∘ resizePage) (Int.fromString pageSize) $> next
evalJTable (StartEnterCustomPageSize next) =
  H.modify (_isEnteringPageSize .~ true) $> next
evalJTable (SetCustomPageSize size next) =
  H.modify (setPageSize size) $> next
evalJTable (SetCustomPage page next) =
  H.modify (setPage page) $> next
