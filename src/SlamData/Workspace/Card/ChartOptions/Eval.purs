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

import Data.Lens ((^?))
import Data.Lens as Lens

import SlamData.Effects (SlamDataEffects)
import SlamData.Quasar.Query as QQ
import SlamData.Workspace.Card.Eval.CardEvalT as CET
import SlamData.Workspace.Card.Port as Port
import SlamData.Workspace.Card.ChartOptions.Model as ChartOptions

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

  when (numRecords > 10000) $
    EC.throwError
      $ "The 10000 record limit for visualizations has been exceeded - the current dataset contains " ⊕ show numRecords ⊕ " records. "
      ⊕ "Please consider using a 'limit' or 'group by' clause in the query to reduce the result size."

  pure
    { options: model.options
    , chartConfig: model.chartConfig
    , resource: resource
    }
