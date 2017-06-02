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

module SlamData.Workspace.Card.Setups.Geo.Heatmap.Component.State where

import SlamData.Prelude

import Data.Path.Pathy ((</>), (<.>), file, rootDir, dir)
import Data.URI (URIRef)
import Data.URI as URI

type State =
  { osmURIString ∷ String
  , osmURI ∷ URIRef
  }

initialState ∷ State
initialState =
  { osmURIString: "http://{s}.tile.osm.org/{z}/{x}/{y}.png"
  , osmURI:
      Left $ URI.URI
      (Just $ URI.URIScheme "http")
      (URI.HierarchicalPart
       (Just $ URI.Authority Nothing [(URI.NameAddress "{s}.tile.osm.org") × Nothing])
       (Just $ Right $ rootDir </> dir "{z}" </> dir "{x}" </> file "{y}" <.> "png"))
      Nothing
      Nothing
  }
