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
  , ChartInstructionsPort
  , tagPort
  , eqTaggedResourcePort
  , _Initial
  , _Terminal
  , _SlamDown
  , _VarMap
  , _Resource
  , _ResourceTag
  , _DownloadOptions
  , _Draftboard
  , _CardError
  , _Metric
  , _ChartInstructions
  , _PivotTable
  , _TaggedResource
  , module SlamData.Workspace.Card.Port.VarMap
  ) where

import SlamData.Prelude

import Data.Lens (Prism', prism', Traversal', wander, Lens', lens)

import ECharts.Monad (DSL)
import ECharts.Types.Phantom (OptionI)

import SlamData.Workspace.Card.Port.VarMap (VarMap, URLVarMap, VarMapValue(..), renderVarMapValue, emptyVarMap)
import SlamData.Workspace.Card.BuildChart.PivotTable.Model as PTM
import SlamData.Workspace.Card.CardType.ChartType (ChartType)
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
  , varMap ∷ Maybe VarMap
  }

type MetricPort =
  { label ∷ Maybe String
  , value ∷ String
  , taggedResource ∷ TaggedResourcePort
  }

type PivotTablePort =
  { query ∷ String
  , options ∷ PTM.Model
  , taggedResource ∷ TaggedResourcePort
  }

type ChartInstructionsPort =
  { options ∷ DSL OptionI
  , chartType ∷ ChartType
  , taggedResource ∷ TaggedResourcePort
  }

data Port
  = Initial
  | Terminal
  | CardError String
  | VarMap VarMap
  | TaggedResource TaggedResourcePort
  | SlamDown (VarMap × (SD.SlamDownP VarMapValue))
  | ChartInstructions ChartInstructionsPort
  | DownloadOptions DownloadPort
  | Metric MetricPort
  | PivotTable PivotTablePort
  | Draftboard

tagPort ∷ Port → String
tagPort  = case _ of
  Initial → "Initial"
  Terminal → "Terminal"
  SlamDown sd → "SlamDown: " ⊕ show sd
  VarMap vm → "VarMap: " ⊕ show vm
  CardError str → "CardError: " ⊕ show str
  TaggedResource tr → "TaggedResource: " ⊕ show tr.resource ⊕ " " ⊕ show tr.tag
  DownloadOptions _ → "DownloadOptions"
  Draftboard → "Draftboard"
  ChartInstructions _ → "ChartInstructions"
  Metric _ → "Metric"
  PivotTable _ → "PivotTable"

eqTaggedResourcePort ∷ TaggedResourcePort → TaggedResourcePort → Boolean
eqTaggedResourcePort tr1 tr2 =
  tr1.resource ≡ tr2.resource
  && tr1.tag ≡ tr2.tag
  && tr1.varMap ≡ tr2.varMap

_Initial ∷ Prism' Port Unit
_Initial = prism' (const Initial) case _ of
  Initial → Just unit
  _ → Nothing

_Terminal ∷ Prism' Port Unit
_Terminal = prism' (const Terminal) case _ of
  Terminal → Just unit
  _ → Nothing

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
_ResourceTag = _TaggedResource ∘ _tag

_Resource ∷ Traversal' Port PU.FilePath
_Resource = _TaggedResource ∘ _resource

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
  ChartInstructions o → ChartInstructions ∘ o{options = _} <$> f o.options
  _ → pure s

_Metric ∷ Prism' Port MetricPort
_Metric = prism' Metric case _ of
  Metric u → Just u
  _ → Nothing

_PivotTable ∷ Prism' Port PivotTablePort
_PivotTable = prism' PivotTable case _ of
  PivotTable u → Just u
  _ → Nothing

_TaggedResource ∷ Traversal' Port TaggedResourcePort
_TaggedResource = wander \f s → case s of
  ChartInstructions o →
    let
      cstr t = ChartInstructions $ o{taggedResource = t}
    in
      map cstr $ f o.taggedResource
  PivotTable o →
    let
      cstr t = PivotTable $ o{taggedResource = t}
    in
      map cstr $ f o.taggedResource
  Metric o →
    let
      cstr t = Metric $ o{taggedResource = t}
    in
      map cstr $ f o.taggedResource
  TaggedResource tr →
    map TaggedResource $ f tr
  _ → pure s



_resource ∷ ∀ a r. Lens' {resource ∷ a | r} a
_resource = lens _.resource _{resource = _}

_tag ∷ ∀ a r. Traversal' {tag ∷ Maybe a |r} a
_tag = wander \f s →
  case s.tag of
    Nothing → pure s
    Just r →
      let
        mappedR = f r
        cstr = s{tag = _} ∘ Just
      in
        map cstr $ mappedR
