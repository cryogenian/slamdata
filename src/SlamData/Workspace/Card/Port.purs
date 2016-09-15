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

module SlamData.Workspace.Card.Port
  ( Port(..)
  , ChartPort
  , TaggedResourcePort
  , DownloadPort
  , MetricPort
  , tagPort
  , _SlamDown
  , _VarMap
  , _Resource
  , _ResourceAxes
  , _ResourceTag
  , _Chart
  , _DownloadOptions
  , _Draftboard
  , _CardError
  , _Blocked
  , _Metric
  , _ChartInstructions
  , module SlamData.Workspace.Card.Port.VarMap
  ) where

import SlamData.Prelude

import Data.Lens (PrismP, prism', TraversalP, wander)
import Data.Set as Set

import ECharts.Monad (DSL)
import ECharts.Types.Phantom (OptionI)

import SlamData.Workspace.Card.Port.VarMap (VarMap, URLVarMap, VarMapValue(..), parseVarMapValue, renderVarMapValue, emptyVarMap)
import SlamData.Workspace.Card.CardType.ChartType (ChartType)
import SlamData.Workspace.Card.Chart.Axis (Axes, printAxes)
import SlamData.Workspace.Card.Chart.Config as CC
import SlamData.Download.Model (DownloadOptions)
import Text.Markdown.SlamDown as SD
import Utils.Path as PU

type ChartPort =
  { resource ∷ PU.FilePath
  , availableChartTypes ∷ Set.Set ChartType
  , axes ∷ Axes
  , config ∷ Maybe CC.ChartConfig
  }

type DownloadPort =
  { resource ∷ PU.FilePath
  , compress ∷ Boolean
  , options ∷ DownloadOptions
  }

type TaggedResourcePort =
  { resource ∷ PU.FilePath
  , tag ∷ Maybe String
  , axes ∷ Axes
  }

type MetricPort =
  { label ∷ Maybe String
  , value ∷ String
  }

data Port
  = SlamDown (VarMap × (SD.SlamDownP VarMapValue))
  | VarMap VarMap
  | CardError String
  | Chart ChartPort
  | ChartInstructions (DSL OptionI) ChartType
  | TaggedResource TaggedResourcePort
  | DownloadOptions DownloadPort
  | Metric MetricPort
  | Draftboard
  | Blocked


tagPort ∷ Maybe Port → String
tagPort Nothing = "Nothing"
tagPort (Just p) = case p of
  SlamDown sd → "SlamDown: " ⊕ show sd
  VarMap vm → "VarMap: " ⊕ show vm
  CardError str → "CardError: " ⊕ show str
  Chart p → "Chart"
  TaggedResource p → "TaggedResource: " ⊕ show p.resource ⊕ " " ⊕ show p.tag
  DownloadOptions p → "DownloadOptions"
  Draftboard → "Draftboard"
  Blocked → "Blocked"
  ChartInstructions _ _ → "ChartInstructions"
  Metric _ → "Metric"

_SlamDown ∷ TraversalP Port (SD.SlamDownP VarMapValue)
_SlamDown = wander \f s → case s of
  SlamDown (vm × sd) → SlamDown ∘ (vm × _) <$> f sd
  _ → pure s

_VarMap ∷ TraversalP Port VarMap
_VarMap = wander \f s → case s of
  VarMap x → VarMap <$> f x
  SlamDown (vm × sd) → SlamDown ∘ (_ × sd) <$> f vm
  _ → pure s

_CardError ∷ PrismP Port String
_CardError = prism' CardError \p → case p of
  CardError x → Just x
  _ → Nothing

_Chart ∷ PrismP Port ChartPort
_Chart = prism' Chart \p → case p of
  Chart o → Just o
  _ → Nothing

_ResourceTag ∷ TraversalP Port String
_ResourceTag = wander \f s → case s of
  TaggedResource o@{tag: Just tag} →
    TaggedResource ∘ o{tag = _} ∘ Just <$> f tag
  s → pure s

_Resource ∷ TraversalP Port PU.FilePath
_Resource = wander \f s → case s of
  TaggedResource o → TaggedResource ∘ o{resource = _} <$> f o.resource
  _ → pure s

_ResourceAxes ∷ TraversalP Port Axes
_ResourceAxes = wander \f s → case s of
  TaggedResource o → TaggedResource ∘ o{axes = _} <$> f o.axes
  _ → pure s


_Blocked ∷ PrismP Port Unit
_Blocked = prism' (const Blocked) \p → case p of
  Blocked → Just unit
  _ → Nothing

_DownloadOptions ∷ PrismP Port DownloadPort
_DownloadOptions = prism' DownloadOptions \p → case p of
  DownloadOptions p' → Just p'
  _ → Nothing

_Draftboard ∷ PrismP Port Unit
_Draftboard = prism' (const Draftboard) \p → case p of
  Draftboard → Just unit
  _ → Nothing

_ChartInstructions ∷ TraversalP Port (DSL OptionI)
_ChartInstructions = wander \f s → case s of
  ChartInstructions opts chty → flip ChartInstructions chty <$> f opts
  _ → pure s

_Metric ∷ PrismP Port MetricPort
_Metric = prism' Metric case _ of
  Metric u → Just u
  _ → Nothing
