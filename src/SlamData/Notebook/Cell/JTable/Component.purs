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

module SlamData.Notebook.Cell.JTable.Component
  ( jtableComponent
  , queryShouldRun
  , module SlamData.Notebook.Cell.JTable.Component.Query
  , module SlamData.Notebook.Cell.JTable.Component.State
  ) where

import SlamData.Prelude

import Data.Argonaut.Core as JSON
import Data.Int as Int
import Data.Lens ((.~), (?~))

import Halogen as H

import Quasar.Aff as Quasar
import Quasar.Auth as Auth

import SlamData.Effects (Slam)
import SlamData.Notebook.Cell.Common.EvalQuery (CellEvalQuery(..), CellEvalResult)
import SlamData.Notebook.Cell.Component (CellQueryP, CellStateP, makeResultsCellComponent, makeQueryPrism, _JTableState, _JTableQuery)
import SlamData.Notebook.Cell.JTable.Component.Query (QueryP, PageStep(..), Query(..))
import SlamData.Notebook.Cell.JTable.Component.Render (render)
import SlamData.Notebook.Cell.JTable.Component.State (Input, PageInfo, State, _input, _isEnteringPageSize, _page, _pageSize, _resource, _result, _size, currentPageInfo, fromModel, initialState, pendingPageInfo, resizePage, setPage, setPageSize, stepPage, toModel)
import SlamData.Notebook.Cell.JTable.Model as Model
import SlamData.Notebook.Cell.Port (Port(..))

jtableComponent :: H.Component CellStateP CellQueryP Slam
jtableComponent = makeResultsCellComponent
  { component: H.component { render, eval: coproduct evalCell evalJTable }
  , initialState: initialState
  , _State: _JTableState
  , _Query: makeQueryPrism _JTableQuery
  }

queryShouldRun :: forall a. QueryP a -> Boolean
queryShouldRun = coproduct (const false) pred
  where
  pred (StepPage _ _) = true
  pred (ChangePageSize _ _) = true
  pred _ = false

-- | Evaluates generic cell queries.
evalCell :: Natural CellEvalQuery (H.ComponentDSL State QueryP Slam)
evalCell (NotifyRunCell next) = pure next
evalCell (EvalCell value k) =
  case value.inputPort of
    Just (TaggedResource { tag, resource }) -> do
      size <- H.fromAff $ Auth.authed (Quasar.count resource)
      oldInput <- H.gets _.input
      when    (((oldInput <#> _.resource) /= pure resource)
            || ((oldInput >>= _.tag) /= tag))
        $ H.set initialState
      H.modify $ _input ?~ { resource, size, tag }
      p <- H.gets pendingPageInfo
      items <- H.fromAff $ Auth.authed $ Quasar.sample resource ((p.page - 1) * p.pageSize) p.pageSize
      H.modify
        $ (_isEnteringPageSize .~ false)
        <<< (_result ?~
              { json: JSON.fromArray items
              , page: p.page
              , pageSize: p.pageSize
              })
      pure $ k (result value.inputPort)
    Just Blocked -> do
      H.set initialState
      pure $ k (result Nothing)
    _ -> pure $ k (error "expected a Resource input")
  where
  result :: Maybe Port -> CellEvalResult
  result = { output: _, messages: [] }
  error :: String -> CellEvalResult
  error msg =
    { output: Nothing
    , messages: [Left $ "An internal error occurred: " ++ msg]
    }
evalCell (SetupCell _ next) = pure next
evalCell (Save k) =
  pure <<< k =<< H.gets (Model.encode <<< toModel)
evalCell (Load json next) = do
  either (const (pure unit)) H.set $ fromModel <$> Model.decode json
  pure next
evalCell (SetCanceler _ next) = pure next

-- | Evaluates jtable-specific cell queries.
evalJTable :: Natural Query (H.ComponentDSL State QueryP Slam)
evalJTable (StepPage step next) =
  H.modify (stepPage step) $> next
evalJTable (ChangePageSize pageSize next) =
  maybe (pure unit) (H.modify <<< resizePage) (Int.fromString pageSize) $> next
evalJTable (StartEnterCustomPageSize next) =
  H.modify (_isEnteringPageSize .~ true) $> next
evalJTable (SetCustomPageSize size next) =
  H.modify (setPageSize size) $> next
evalJTable (SetCustomPage page next) =
  H.modify (setPage page) $> next
