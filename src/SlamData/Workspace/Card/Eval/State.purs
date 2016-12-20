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

module SlamData.Workspace.Card.Eval.State
  ( EvalState(..)
  , AnalysisR
  , _Analysis
  , _Axes
  , _Records
  , _Resource
  ) where

import SlamData.Prelude

import Data.Argonaut (Json)
import Data.Lens (Prism', prism', Traversal', wander)

import SlamData.Workspace.Card.Port (TaggedResourcePort)
import SlamData.Workspace.Card.BuildChart.Axis (Axes)

type AnalysisR =
  { taggedResource ∷ TaggedResourcePort
  , axes ∷ Axes
  , records ∷ Array Json
  }

data EvalState
  = Analysis AnalysisR

_Analysis ∷ Prism' EvalState AnalysisR
_Analysis = prism' Analysis case _ of
  Analysis x → Just x

_Axes ∷ Traversal' EvalState Axes
_Axes = wander \f s → case s of
  Analysis r@{ axes } → Analysis ∘ r { axes = _} <$> f axes

_Records ∷ Traversal' EvalState (Array Json)
_Records = wander \f s → case s of
  Analysis r@{ records } → Analysis ∘ r { records = _} <$> f records

_Resource ∷ Traversal' EvalState TaggedResourcePort
_Resource = wander \f s → case s of
  Analysis r@{ taggedResource } → Analysis ∘ r { taggedResource = _} <$> f taggedResource
