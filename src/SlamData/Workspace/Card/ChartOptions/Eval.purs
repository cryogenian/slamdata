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

module SlamData.Workspace.Card.ChartOptions.Eval where

import SlamData.Prelude

import Control.Monad.Aff.Free (class Affable)
import Control.Monad.Eff.Exception as Exn
import Control.Monad.Error.Class as EC

import Data.Argonaut (JCursor)
import Data.Array (cons, length, null)
import Data.Lens ((^?))
import Data.Lens as Lens
import Data.Set as Set
import Data.Map as Map

import SlamData.Effects (SlamDataEffects)
import SlamData.Quasar.Query as QQ
import SlamData.Workspace.Card.Eval.CardEvalT as CET
import SlamData.Workspace.Card.Port as Port
import SlamData.Workspace.Card.ChartOptions.Model as ChartOptions
import SlamData.Workspace.Card.Chart.ChartType (ChartType(..))
import SlamData.Workspace.Card.Chart.Axis (Axis, analyzeJArray)
import SlamData.Workspace.Card.Chart.Axis as Ax

eval
  ∷ ∀ m
  . (Monad m, Affable SlamDataEffects m)
  ⇒ CET.CardEvalInput
  → ChartOptions.Model
  → CET.CardEvalT m Port.ChartPort
eval info model = do
  resource ←
    info.input
      ^? Lens._Just ∘ Port._Resource
      # maybe (EC.throwError "Expected Resource input") pure

  numRecords ←
    QQ.count resource
      # lift
      >>= either (EC.throwError ∘ Exn.message) pure

  when (numRecords > 10000)
    $ EC.throwError
      $ "The 10000 record limit for visualizations has been exceeded - the current dataset contains "
      ⊕ show numRecords
      ⊕ " records. "
      ⊕ "Please consider using a 'limit' or 'group by' clause in the query to reduce the result size."

  recordSample ←
    QQ.sample resource 0 20
      # lift
      >>= either (const $ EC.throwError "Error getting input resource") pure

  when (null recordSample)
    $ EC.throwError "Input resource is empty"

  let
    sample = analyzeJArray recordSample
    axes = getAxes sample
    available =
      if null axes.value
      then []
      else if not $ null axes.category
           then [ Pie, Bar, Line ]
           else if (null axes.time) ∧ (length axes.value < 2)
                then []
                else [ Line ]

  when (null available)
    $ EC.throwError "There is no available chart types for this data"

  let
    availableChartTypes = foldMap Set.singleton available

  -- Commented till this it would be clear how this should work exactly
--  when (isNothing model.chartConfig)
--    $ EC.throwError "Please select axes"

  pure
    { options: model.options
    , chartConfig: model.chartConfig
    , resource
    , availableChartTypes
    , axes
    }
  where
  getAxes
    ∷ Map.Map JCursor Axis
    → {category ∷ Array JCursor, value ∷ Array JCursor, time ∷ Array JCursor}
  getAxes sample =
    foldl foldFn {category: [], value: [], time: []} $ Map.toList sample

  foldFn
    ∷ {category ∷ Array JCursor, value ∷ Array JCursor, time ∷ Array JCursor}
    → JCursor × Axis
    → {category ∷ Array JCursor, value ∷ Array JCursor, time ∷ Array JCursor}
  foldFn accum (cursor × axis)
    | Ax.isCatAxis axis = accum { category = cons cursor accum.category }
    | Ax.isValAxis axis = accum { value = cons cursor accum.value }
    | Ax.isTimeAxis axis = accum { time = cons cursor accum.time }
    | otherwise = accum
