{-
Copyright 2017 SlamData, Inc.

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

module SlamData.Workspace.Card.Geo.Component.State where

import SlamData.Prelude

import Data.Path.Pathy ((</>), (<.>), file, rootDir, dir)
import Data.URI (URIRef)
import Data.URI as URI

import Leaflet.Core as LC

type State =
  { osmURI ∷ URIRef
  , zoom ∷ Int
  , view ∷ { lat ∷ Number, lng ∷ Number }
  , layers ∷ Array LC.Layer
  , controls ∷ Array LC.Control
  , tileLayer ∷  Maybe LC.Layer
  }

initialState ∷ State
initialState =
  { osmURI:
      Left $ URI.URI
      (Just $ URI.URIScheme "http")
      (URI.HierarchicalPart
       (Just $ URI.Authority Nothing [(URI.NameAddress "{s}.tile.osm.org") × Nothing])
       (Just $ Right $ rootDir </> dir "{z}" </> dir "{x}" </> file "{y}" <.> "png"))
      Nothing
      Nothing
  , zoom: zero
  , view: { lat: zero, lng: zero }
  , layers: [ ]
  , controls: [ ]
  , tileLayer: Nothing
  }
import SlamData.Prelude

import Data.Path.Pathy ((</>), (<.>), file, rootDir, dir)
import Data.URI (URIRef)
import Data.URI as URI

import Leaflet.Core as LC

type State =
  { osmURI ∷ URIRef
  , zoom ∷ Int
  , view ∷ { lat ∷ Number, lng ∷ Number }
  , layers ∷ Array LC.Layer
  , controls ∷ Array LC.Control
  , tileLayer ∷  Maybe LC.Layer
  }

initialState ∷ State
initialState =
  { osmURI:
      Left $ URI.URI
      (Just $ URI.URIScheme "http")
      (URI.HierarchicalPart
       (Just $ URI.Authority Nothing [(URI.NameAddress "{s}.tile.osm.org") × Nothing])
       (Just $ Right $ rootDir </> dir "{z}" </> dir "{x}" </> file "{y}" <.> "png"))
      Nothing
      Nothing
  , zoom: zero
  , view: { lat: zero, lng: zero }
  , layers: [ ]
  , controls: [ ]
  , tileLayer: Nothing
  }
