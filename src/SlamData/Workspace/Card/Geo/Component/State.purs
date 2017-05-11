module SlamData.Workspace.Card.Geo.Component.State where

import SlamData.Prelude

import Data.Path.Pathy ((</>), (<.>), file, currentDir, rootDir, dir)
import Data.URI (URIRef)
import Data.URI as URI

type State =
  { osmURI ∷ URIRef
  , zoom ∷ Int
  , view ∷ { lat ∷ Number, lng ∷ Number }
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
  }