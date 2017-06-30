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
  , Resource(..)
  , DataMap
  , Out
  , DownloadPort
  , BuildMetricPort
  , PivotTablePort
  , GeoChartPort
  , SetupInputPort
  , SetupSelectPort
  , BuildChartPort
  , MetricPort
  , tagPort
  , emptyOut
  , terminalOut
  , varMapOut
  , resourceOut
  , portOut
  , defaultResourceVar
  , filterResources
  , extractResource
  , extractFilePath
  , flattenResources
  , resourceToVarMapValue
  , _Initial
  , _Terminal
  , _Variables
  , _SlamDown
  , _DownloadOptions
  , _Metric
  , _PivotTable
  , _GeoChartPort
  , _filePath
  , _osmURI
  , module SlamData.Workspace.Card.Port.VarMap
  ) where

import SlamData.Prelude

import Control.Monad.Aff (Aff)

import Data.Argonaut (Json)
import Data.Lens (Prism', prism', Traversal', wander, Lens', lens, (^.), view)
import Data.List as List
import Data.Map as Map
import Data.Set as Set
import Data.StrMap as SM
import Data.Path.Pathy as Path
import Data.URI (URIRef)

import ECharts.Monad (DSL)
import ECharts.Types.Phantom (OptionI)

import Halogen.HTML.Properties as HP

import Leaflet.Core as LC

import SlamData.Effects (SlamDataEffects)
import SlamData.Download.Model (DownloadOptions)
import SlamData.Workspace.Card.Error as CE
import SlamData.Workspace.Card.Setups.Chart.PivotTable.Model as PTM
import SlamData.Workspace.Card.Setups.Semantics as Sem
import SlamData.Workspace.Card.CardType.VizType (SelectType)
import SlamData.Workspace.Card.Setups.Dimension as D
import SlamData.Workspace.Card.Port.VarMap (VarMap, URLVarMap, VarMapValue(..), emptyVarMap, _VarMapValue)

import SqlSquared as Sql

import Text.Markdown.SlamDown as SD
import Utils.Path as PU

data Resource
  = Path PU.FilePath
  | View PU.FilePath String DataMap

derive instance eqResource ∷ Eq Resource

type DataMap = SM.StrMap (Either Resource VarMapValue)

type Out = Port × DataMap

type DownloadPort =
  { resource ∷ PU.FilePath
  , compress ∷ Boolean
  , options ∷ DownloadOptions
  , targetName ∷ String
  }

type MetricPort =
  { label ∷ Maybe String
  , value ∷ String
  }

type BuildMetricPort =
  Json → String ⊹ { value ∷ String, label ∷ Maybe String }

type BuildChartPort =
  Array Json → DSL OptionI


type PivotTablePort =
  { dimensions ∷ Array (String × PTM.GroupByDimension)
  , columns ∷ Array (String × PTM.ColumnDimension)
  , isSimpleQuery ∷ Boolean
  }

type SetupInputPort =
  { projection ∷ D.LabeledJCursor
  , inputType ∷ HP.InputType
  }

type SetupSelectPort =
  { projection ∷ D.LabeledJCursor
  , inputType ∷ SelectType
  , build ∷ Array Json → CE.CardError ⊹
      { valueLabelMap ∷ Map.Map Sem.Semantics (Maybe String)
      , selectedValues ∷ Set.Set Sem.Semantics
      }
  }

type GeoChartPort =
  { build ∷ LC.Leaflet → Array Json → Aff SlamDataEffects (Array LC.Layer × Array LC.Control)
  , osmURI ∷ URIRef
  }

data Port
  = Initial
  | Terminal
  | Variables
  | CardError CE.CardError
  | ResourceKey String
  | SetupInput SetupInputPort
  | SetupSelect SetupSelectPort
  | SlamDown (SD.SlamDownP VarMapValue)
  | BuildChart BuildChartPort
  | DownloadOptions DownloadPort
  | BuildMetric BuildMetricPort
  | CategoricalMetric MetricPort
  | PivotTable PivotTablePort
  | GeoChart GeoChartPort
  | Viz

tagPort ∷ Port → String
tagPort  = case _ of
  Initial → "Initial"
  Terminal → "Terminal"
  Variables → "Variables"
  CardError err → "CardError: " ⊕ CE.showCardError err
  ResourceKey str → "ResourceKey: " ⊕ show str
  SlamDown sd → "SlamDown: " ⊕ show sd
  BuildChart _ → "BuildChart"
  DownloadOptions _ → "DownloadOptions"
  CategoricalMetric _ → "CategoricalMetric"
  PivotTable _ → "PivotTable"
  GeoChart _ → "GeoChart"
  BuildMetric _ → "BuildMetric"
  SetupInput _ → "SetupInput"
  SetupSelect _ → "SetupSelect"
  Viz → "Viz"

filterResources ∷ DataMap → SM.StrMap Resource
filterResources = SM.fold go SM.empty
  where
    go m key (Left res) = SM.insert key res m
    go m _ _ = m

extractResource ∷ DataMap → Maybe Resource
extractResource = map snd ∘ List.head ∘ SM.toUnfoldable ∘ filterResources

extractFilePath ∷ DataMap → Maybe PU.FilePath
extractFilePath = map (view _filePath) ∘ extractResource

flattenResources ∷ DataMap → VarMap
flattenResources = map go
  where
    go (Left val) = resourceToVarMapValue val
    go (Right val) = val

resourceToVarMapValue ∷ Resource → VarMapValue
resourceToVarMapValue r =
  VarMapValue $ Sql.ident $ Path.printPath $ r ^. _filePath

defaultResourceVar ∷ String
defaultResourceVar = "results"

emptyOut ∷ Out
emptyOut = Initial × SM.empty

terminalOut ∷ Out
terminalOut = Terminal × SM.empty

varMapOut ∷ DataMap → Out
varMapOut v = Variables × v

resourceOut ∷ Resource → Out
resourceOut r = ResourceKey defaultResourceVar × SM.singleton defaultResourceVar (Left r)

portOut ∷ Port → Out
portOut p = p × SM.empty

_Initial ∷ Prism' Port Unit
_Initial = prism' (const Initial) case _ of
  Initial → Just unit
  _ → Nothing

_Terminal ∷ Prism' Port Unit
_Terminal = prism' (const Terminal) case _ of
  Terminal → Just unit
  _ → Nothing

_Variables ∷ Prism' Port Unit
_Variables = prism' (const Variables) case _ of
  Variables → Just unit
  _ → Nothing

_SlamDown ∷ Traversal' Port (SD.SlamDownP VarMapValue)
_SlamDown = wander \f s → case s of
  SlamDown sd → SlamDown <$> f sd
  _ → pure s

_DownloadOptions ∷ Prism' Port DownloadPort
_DownloadOptions = prism' DownloadOptions $ case _ of
  DownloadOptions p' → Just p'
  _ → Nothing

_Metric ∷ Traversal' Port MetricPort
_Metric = wander \f s → case s of
  CategoricalMetric m → map CategoricalMetric $ f m
  _ → pure s

_PivotTable ∷ Prism' Port PivotTablePort
_PivotTable = prism' PivotTable case _ of
  PivotTable u → Just u
  _ → Nothing

_filePath ∷ Lens' Resource PU.FilePath
_filePath = lens get set
  where
    get (Path fp) = fp
    get (View fp _ _) = fp

    set (Path _) fp = Path fp
    set (View _ a b) fp = View fp a b

_GeoChartPort ∷ Prism' Port GeoChartPort
_GeoChartPort = prism' GeoChart case _ of
  GeoChart u → Just u
  _ → Nothing

_osmURI ∷ Traversal' Port URIRef
_osmURI = _GeoChartPort ∘ lens _.osmURI _{ osmURI = _ }
