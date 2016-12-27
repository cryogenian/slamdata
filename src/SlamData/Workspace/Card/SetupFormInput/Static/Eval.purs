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

module SlamData.Workspace.Card.SetupFormInput.Static.Eval
  ( eval
  , module SlamData.Workspace.Card.SetupFormInput.Static.Model
  ) where

import SlamData.Prelude

import Control.Monad.State (class MonadState, get, put)
import Control.Monad.Throw (class MonadThrow)

import Data.Array as Arr

import SlamData.Quasar.Class (class QuasarDSL)
import SlamData.Workspace.Card.BuildChart.Semantics as Sem
import SlamData.Workspace.Card.BuildChart.Common.Eval as BCE
import SlamData.Workspace.Card.Port as Port
import SlamData.Workspace.Card.Eval.Monad as CEM
import SlamData.Workspace.Card.SetupFormInput.Static.Model (Model)

eval
  ∷ ∀ m
  . ( MonadState CEM.CardState m
    , MonadThrow CEM.CardError m
    , QuasarDSL m
    )
  ⇒ Model
  → Port.TaggedResourcePort
  → m Port.Port
eval m tr = do
  records × axes ← BCE.analyze tr =<< get
  put (Just (CEM.Analysis {taggedResource: tr, records, axes}))
  case m of
    Nothing →
      CEM.throw "Please select axis."
    Just conf → case Arr.head records >>= flip Sem.getMaybeString conf.value of
      Nothing →
        CEM.throw $ show conf.value <> " axis is not presented in this resource"
      Just value →
        pure
          $ Port.Metric
          $ { value
            , label: Nothing
            , taggedResource: tr
            }
