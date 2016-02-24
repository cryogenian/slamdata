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

module SlamData.Notebook.Cell.Port
  ( Port(..)
  , ChartPort()
  , module SlamData.Notebook.Cell.Port.VarMap
  , _SlamDown
  , _VarMap
  , _Resource
  , _ChartOptions
  , _ResourceTag
  , _Resource
  ) where

import Prelude

import Data.Lens (PrismP(), prism', TraversalP(), wander)
import Data.Maybe (Maybe(..))

import ECharts.Options as Ec

import SlamData.FileSystem.Resource as R
import SlamData.Notebook.Cell.Port.VarMap

import Text.Markdown.SlamDown (SlamDown())

type ChartPort = { options :: Ec.Option, width :: Int, height :: Int }

data Port
  = SlamDown SlamDown
  | VarMap VarMap
  | ChartOptions ChartPort
  | TaggedResource {tag :: Maybe String, resource :: R.Resource }
  | Blocked

_SlamDown :: PrismP Port SlamDown
_SlamDown = prism' SlamDown \p -> case p of
  SlamDown x -> Just x
  _ -> Nothing

_VarMap :: PrismP Port VarMap
_VarMap = prism' VarMap \p -> case p of
  VarMap x -> Just x
  _ -> Nothing

_ChartOptions :: PrismP Port ChartPort
_ChartOptions = prism' ChartOptions \p -> case p of
  ChartOptions o -> Just o
  _ -> Nothing

_ResourceTag :: TraversalP Port String
_ResourceTag = wander \f s -> case s of
  TaggedResource o@{tag = Just tag} ->
    (TaggedResource <<< o{tag = _} <<< Just) <$> f tag
  _ -> pure s

_Resource :: TraversalP Port R.Resource
_Resource = wander \f s -> case s of
  TaggedResource o -> (TaggedResource <<< o{resource = _}) <$> f o.resource
  _ -> pure s

_Blocked :: PrismP Port Unit
_Blocked = prism' (const Blocked) \p -> case p of
  Blocked -> Just unit
  _ -> Nothing
