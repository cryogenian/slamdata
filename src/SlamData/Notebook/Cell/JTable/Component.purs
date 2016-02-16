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

import Prelude

import Control.Bind ((=<<))
import Control.Monad (when)

import Data.Argonaut.Core as JSON
import Data.Either (Either(..), either)
import Data.Functor (($>))
import Data.Functor.Aff (liftAff)
import Data.Functor.Coproduct (coproduct)
import Data.Int as Int
import Data.Lens ((.~), (?~), preview)
import Data.Maybe (Maybe(..), maybe)

import Halogen

import Quasar.Aff as Quasar
import Quasar.Auth as Auth

import SlamData.Notebook.Cell.Common.EvalQuery (CellEvalQuery(..), CellEvalResult())
import SlamData.Notebook.Cell.Component (CellQueryP(), CellStateP(), makeResultsCellComponent, makeQueryPrism, _JTableState, _JTableQuery)
import SlamData.Notebook.Cell.JTable.Component.Query
import SlamData.Notebook.Cell.JTable.Component.Render (render)
import SlamData.Notebook.Cell.JTable.Component.State
import SlamData.Notebook.Cell.JTable.Model as Model
import SlamData.Notebook.Cell.Port (_Resource, _ResourceTag)
import SlamData.Effects (Slam())

jtableComponent :: Component CellStateP CellQueryP Slam
jtableComponent = makeResultsCellComponent
  { component: component render (coproduct evalCell evalJTable)
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
evalCell :: Natural CellEvalQuery (ComponentDSL State QueryP Slam)
evalCell (NotifyRunCell next) = pure next
evalCell (EvalCell value k) =
  case preview _Resource =<< value.inputPort of
    Just resource -> do
      size <- liftAff $ Auth.authed $ Quasar.count resource
      oldInput <- gets _.input
      let tag = preview _ResourceTag =<< value.inputPort
      when    (((oldInput <#> _.resource) /= pure resource)
            || ((oldInput >>= _.tag) /= tag))
        $ set initialState
      modify $ _input ?~ { resource, size, tag }
      p <- gets pendingPageInfo
      items <- liftAff $ Auth.authed $ Quasar.sample resource ((p.page - 1) * p.pageSize) p.pageSize
      modify
        $ (_isEnteringPageSize .~ false)
        <<< (_result ?~
              { json: JSON.fromArray items
              , page: p.page
              , pageSize: p.pageSize
              })
      pure $ k { output: value.inputPort, messages: [] }
    Nothing -> pure $ k (error "expected a Resource input")
  where
  error :: String -> CellEvalResult
  error msg =
    { output: Nothing
    , messages: [Left $ "An internal error occurred: " ++ msg]
    }
evalCell (SetupCell _ next) = pure next
evalCell (Save k) =
  pure <<< k =<< gets (Model.encode <<< toModel)
evalCell (Load json next) = do
  either (const (pure unit)) set $ fromModel <$> Model.decode json
  pure next
evalCell (SetCanceler _ next) = pure next

-- | Evaluates jtable-specific cell queries.
evalJTable :: Natural Query (ComponentDSL State QueryP Slam)
evalJTable (StepPage step next) =
  modify (stepPage step) $> next
evalJTable (ChangePageSize pageSize next) =
  maybe (pure unit) (modify <<< resizePage) (Int.fromString pageSize) $> next
evalJTable (StartEnterCustomPageSize next) =
  modify (_isEnteringPageSize .~ true) $> next
evalJTable (SetCustomPageSize size next) =
  modify (setPageSize size) $> next
evalJTable (SetCustomPage page next) =
  modify (setPage page) $> next
