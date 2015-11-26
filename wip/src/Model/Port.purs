{-
Copyright 2015 SlamData, Inc.

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

module Model.Port
  ( Port(..)
  , ChartPort()
  , _SlamDown
  , _VarMap
  , _Resource
  , _ChartOptions
  ) where

import Data.Lens (PrismP(), prism')
import Data.Maybe (Maybe(..))
import Data.StrMap (StrMap())
import ECharts.Options as Ec

import Model.Resource as R

import Text.Markdown.SlamDown (SlamDown())
import Text.Markdown.SlamDown.Html (FormFieldValue())

type ChartPort = { options :: Ec.Option, width :: Int, height :: Int }

data Port
  = SlamDown SlamDown
  | VarMap (StrMap FormFieldValue)
  | Resource R.Resource
  | ChartOptions ChartPort

_SlamDown :: PrismP Port SlamDown
_SlamDown = prism' SlamDown \p -> case p of
  SlamDown x -> Just x
  _ -> Nothing

_VarMap :: PrismP Port (StrMap FormFieldValue)
_VarMap = prism' VarMap \p -> case p of
  VarMap x -> Just x
  _ -> Nothing

_Resource :: PrismP Port R.Resource
_Resource = prism' Resource \p -> case p of
  Resource r -> Just r
  _ -> Nothing

_ChartOptions :: PrismP Port ChartPort
_ChartOptions = prism' ChartOptions \p -> case p of
  ChartOptions o -> Just o
  _ -> Nothing
