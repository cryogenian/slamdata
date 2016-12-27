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

module SlamData.Workspace.Card.SetupFormInput.TextLike.Eval
  ( eval
  , module SlamData.Workspace.Card.SetupFormInput.TextLike.Model
  ) where

import Control.Monad.State (class MonadState)
import Control.Monad.Throw (class MonadThrow)

import SlamData.Quasar.Class (class QuasarDSL)
import SlamData.Workspace.Card.CardType.FormInputType (FormInputType)
import SlamData.Workspace.Card.Port as Port
import SlamData.Workspace.Card.Eval.Monad as CEM
import SlamData.Workspace.Card.BuildChart.Common.Eval as BCE
import SlamData.Workspace.Card.SetupFormInput.TextLike.Model (Model)

eval
  ∷ ∀ m
  . ( MonadState CEM.CardState m
    , MonadThrow CEM.CardError m
    , QuasarDSL m
    )
  ⇒ Model
  → Port.TaggedResourcePort
  → FormInputType
  → m Port.Port
eval m taggedResource formInputType =
  BCE.buildChartEval' buildFn taggedResource m
  where
  -- We need to store axes to display selects
  buildFn axes conf records =
    Port.SetupTextLikeFormInput
      { name: conf.name
      , taggedResource
      , formInputType
      , cursor: conf.value
      }
