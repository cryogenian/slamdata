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

module SlamData.Workspace.Card.BuildChart.Common.Eval
  ( buildChartEval
  , buildChartEval'
  , type (>>)
  ) where

import SlamData.Prelude

import Control.Monad.State (class MonadState, get, put)
import Control.Monad.Throw (class MonadThrow)

import Data.Argonaut (Json)
import Data.Array as A
import Data.Map as M

import ECharts.Monad (DSL)
import ECharts.Types.Phantom (OptionI)

import SlamData.Quasar.Class (class QuasarDSL)
import SlamData.Quasar.Query as QQ
import SlamData.Workspace.Card.BuildChart.Axis (Axes, buildAxes)
import SlamData.Workspace.Card.CardType.ChartType (ChartType)
import SlamData.Workspace.Card.Eval.Monad as CEM
import SlamData.Workspace.Card.Port as Port

infixr 3 type M.Map as >>

buildChartEval
  ∷ ∀ m p
  . ( MonadState CEM.CardState m
    , MonadThrow CEM.CardError m
    , QuasarDSL m
    )
  ⇒ ChartType
  → (Axes → p → Array Json → DSL OptionI)
  → Port.TaggedResourcePort
  → Maybe p
  → m Port.Port
buildChartEval chartType build taggedResource =
  flip buildChartEval' taggedResource
    \axes model records →
      Port.ChartInstructions
        { options: build axes model records
        , chartType
        , taggedResource
        }

buildChartEval'
  ∷ ∀ m p
  . ( MonadState CEM.CardState m
    , MonadThrow CEM.CardError m
    , QuasarDSL m
    )
  ⇒ (Axes → p → Array Json → Port.Port)
  → Port.TaggedResourcePort
  → Maybe p
  → m Port.Port
buildChartEval' build tr model = do
  records × axes ← analyze tr =<< get
  put (Just (CEM.Analysis { taggedResource: tr, records, axes }))
  case model of
    Just ch → pure $ build axes ch records
    Nothing → CEM.throw "Please select axis to aggregate."

analyze
  ∷ ∀ m
  . ( MonadThrow CEM.CardError m
    , QuasarDSL m
    )
  ⇒ Port.TaggedResourcePort
  → CEM.CardState
  → m (Array Json × Axes)
analyze tr (Just (CEM.Analysis st))
  | Port.eqTaggedResourcePort st.taggedResource tr =
      pure (st.records × st.axes)
analyze tr _ = do
  records ← CEM.liftQ (QQ.all tr.resource)
  let axes = buildAxes (A.take 100 records)
  pure (records × axes)
