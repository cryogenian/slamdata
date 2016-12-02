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
  , TaggedResourcePort
  , DownloadPort
  , MetricPort
  , PivotTablePort
  , tagPort
  , _SlamDown
  , _VarMap
  , _Resource
  , _ResourceAxes
  , _ResourceTag
  , _DownloadOptions
  , _Draftboard
  , _CardError
  , _Blocked
  , _Metric
  , _ChartInstructions
  , _PivotTable
  , module SlamData.Workspace.Card.Port.VarMap
  ) where

import SlamData.Prelude

import Data.Argonaut (Json)
import Data.Lens (Prism', prism', Traversal', wander)

import ECharts.Monad (DSL)
import ECharts.Types.Phantom (OptionI)

import SlamData.Workspace.Card.Port.VarMap (VarMap, URLVarMap, VarMapValue(..), renderVarMapValue, emptyVarMap)
import SlamData.Workspace.Card.BuildChart.PivotTable.Model as PTM
import SlamData.Workspace.Card.CardType.ChartType (ChartType)
import SlamData.Workspace.Card.BuildChart.Axis (Axes)
import SlamData.Download.Model (DownloadOptions)
import Text.Markdown.SlamDown as SD
import Utils.Path as PU

type DownloadPort =
  { resource ∷ PU.FilePath
  , compress ∷ Boolean
  , options ∷ DownloadOptions
  }

type TaggedResourcePort =
  { resource ∷ PU.FilePath
  , tag ∷ Maybe String
  , axes ∷ Axes
  , varMap ∷ Maybe VarMap
  }

type MetricPort =
  { label ∷ Maybe String
  , value ∷ String
  }

type PivotTablePort =
  { records ∷ Array Json
  , options ∷ PTM.PivotTableR
  , taggedResource ∷ TaggedResourcePort
  }

data Port
  = SlamDown (VarMap × (SD.SlamDownP VarMapValue))
  | VarMap VarMap
  | CardError String
  | ChartInstructions TaggedResourcePort (DSL OptionI) ChartType
  | TaggedResource TaggedResourcePort
  | DownloadOptions DownloadPort
  | Metric TaggedResourcePort MetricPort
  | PivotTable PivotTablePort
  | Draftboard
  | Blocked


tagPort ∷ Maybe Port → String
tagPort Nothing = "Nothing"
tagPort (Just p) = case p of
  SlamDown sd → "SlamDown: " ⊕ show sd
  VarMap vm → "VarMap: " ⊕ show vm
  CardError str → "CardError: " ⊕ show str
  TaggedResource tr → "TaggedResource: " ⊕ show tr.resource ⊕ " " ⊕ show tr.tag
  DownloadOptions _ → "DownloadOptions"
  Draftboard → "Draftboard"
  Blocked → "Blocked"
  ChartInstructions _ _ _ → "ChartInstructions"
  Metric _ _ → "Metric"
  PivotTable _ → "PivotTable"

_SlamDown ∷ Traversal' Port (SD.SlamDownP VarMapValue)
_SlamDown = wander \f s → case s of
  SlamDown (vm × sd) → SlamDown ∘ (vm × _) <$> f sd
  _ → pure s

_VarMap ∷ Traversal' Port VarMap
_VarMap = wander \f s → case s of
  VarMap x → VarMap <$> f x
  SlamDown (vm × sd) → SlamDown ∘ (_ × sd) <$> f vm
  _ → pure s

_CardError ∷ Prism' Port String
_CardError = prism' CardError \p → case p of
  CardError x → Just x
  _ → Nothing

_ResourceTag ∷ Traversal' Port String
_ResourceTag = wander \f s → case s of
  TaggedResource o@{tag: Just tag} →
    TaggedResource ∘ o{tag = _} ∘ Just <$> f tag
  _ → pure s

_Resource ∷ Traversal' Port PU.FilePath
_Resource = wander \f s → case s of
  TaggedResource o → TaggedResource ∘ o{resource = _} <$> f o.resource
  _ → pure s

_ResourceAxes ∷ Traversal' Port Axes
_ResourceAxes = wander \f s → case s of
  TaggedResource o → TaggedResource ∘ o{axes = _} <$> f o.axes
  _ → pure s


_Blocked ∷ Prism' Port Unit
_Blocked = prism' (const Blocked) \p → case p of
  Blocked → Just unit
  _ → Nothing

_DownloadOptions ∷ Prism' Port DownloadPort
_DownloadOptions = prism' DownloadOptions \p → case p of
  DownloadOptions p' → Just p'
  _ → Nothing

_Draftboard ∷ Prism' Port Unit
_Draftboard = prism' (const Draftboard) \p → case p of
  Draftboard → Just unit
  _ → Nothing

_ChartInstructions ∷ Traversal' Port (DSL OptionI)
_ChartInstructions = wander \f s → case s of
  ChartInstructions tr opts chty → flip (ChartInstructions tr) chty <$> f opts
  _ → pure s

_Metric ∷ Traversal' Port MetricPort
_Metric = wander \f s → case s of
  Metric tr u → Metric tr <$> f u
  _ → pure s

_PivotTable ∷ Prism' Port PivotTablePort
_PivotTable = prism' PivotTable case _ of
  PivotTable u → Just u
  _ → Nothing
