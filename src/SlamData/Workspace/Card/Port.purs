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
  , _ChartOptions
  , _ResourceTag
  , _CardError
  , _Blocked
  , module SlamData.Workspace.Card.Port.VarMap
  ) where

import SlamData.Prelude

import Data.Lens (PrismP, prism', TraversalP, wander)
import ECharts.Options as EC
import SlamData.Workspace.Card.Port.VarMap (VarMap, VarMapValue(..), parseVarMapValue, renderVarMapValue)
import SlamData.Download.Model (DownloadOptions)
import Text.Markdown.SlamDown as SD
import Utils.Path as PU

type ChartPort =
  { options ∷ EC.Option
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
  = SlamDown (SD.SlamDownP VarMapValue)
  | VarMap VarMap
  | CardError String
  | ChartOptions ChartPort
  | TaggedResource TaggedResourcePort
  | DownloadOptions DownloadPort
  | Blocked

_SlamDown ∷ PrismP Port (SD.SlamDownP VarMapValue)
_SlamDown = prism' SlamDown \p → case p of
  SlamDown x → Just x
  _ → Nothing

_VarMap ∷ PrismP Port VarMap
_VarMap = prism' VarMap \p → case p of
  VarMap x → Just x
  _ → Nothing

_CardError ∷ PrismP Port String
_CardError = prism' CardError \p → case p of
  CardError x → Just x
  _ → Nothing

_ChartOptions ∷ PrismP Port ChartPort
_ChartOptions = prism' ChartOptions \p → case p of
  ChartOptions o → Just o
  _ → Nothing

_ResourceTag ∷ TraversalP Port String
_ResourceTag = wander \f s → case s of
  TaggedResource o@{tag = Just tag} →
    TaggedResource ∘ o{tag = _} ∘ Just <$> f tag
  _ → pure s

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
