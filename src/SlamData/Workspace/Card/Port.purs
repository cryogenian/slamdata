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
  , _SlamDown
  , _VarMap
  , _Resource
  , _Chart
  , _DownloadOptions
  , _ResourceTag
  , _Draftboard
  , _CardError
  , _Blocked
  , module SlamData.Workspace.Card.Port.VarMap
  ) where

import SlamData.Prelude

import Data.Lens (PrismP, prism', TraversalP, wander)
import Data.Set as Set

import SlamData.Workspace.Card.Port.VarMap (VarMap, URLVarMap, VarMapValue(..), parseVarMapValue, renderVarMapValue, emptyVarMap)
import SlamData.Workspace.Card.Chart.ChartType (ChartType)
import SlamData.Workspace.Card.Chart.Axis (Axes)
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
  }

data Port
  = SlamDown (VarMap × (SD.SlamDownP VarMapValue))
  | VarMap VarMap
  | CardError String
  | Chart ChartPort
  | TaggedResource TaggedResourcePort
  | DownloadOptions DownloadPort
  | Draftboard
  | Blocked

instance showPort ∷ Show Port where
  show =
    case _ of
      SlamDown sd → "SlamDown " <> show sd
      VarMap vm → "VarMap " <> show vm
      CardError str → "CardError " <> show str
      Chart p → "Chart"
      TaggedResource p → "TaggedResource (" <> show p.resource <> " " <> show p.tag <> ")"
      DownloadOptions p → "DownloadOptions"
      Draftboard → "Draftboard"
      Blocked → "Blocked"

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
_ResourceTag = wander \f → case _ of
  TaggedResource o@{tag: Just tag} →
    TaggedResource ∘ o{tag = _} ∘ Just <$> f tag
  s → pure s

_Resource ∷ TraversalP Port PU.FilePath
_Resource = wander \f s → case s of
  TaggedResource o → TaggedResource ∘ o{resource = _} <$> f o.resource
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
