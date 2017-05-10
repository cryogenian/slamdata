module SlamData.Workspace.Card.Geo.Model where

import SlamData.Prelude

import Data.Argonaut as J
import Data.Argonaut ((~>), (:=), (.?))
import Data.URI (URIRef)
import Data.URI as URI

import Test.StrongCheck.Arbitrary (arbitrary)
import Test.StrongCheck.Gen as Gen

type ModelR =
  { osmURI ∷ URIRef
  , view ∷ { lat ∷ Number, lng ∷ Number }
  , zoom ∷ Int
  }

type Model = Maybe ModelR

initialModel ∷ Model
initialModel = Nothing

eqR ∷ ModelR → ModelR → Boolean
eqR r1 r2 =
  r1.osmURI ≡ r2.osmURI
  && r1.view.lat ≡ r2.view.lat
  && r1.view.lng ≡ r2.view.lng
  && r1.zoom ≡ r2.zoom

eqModel ∷ Model → Model → Boolean
eqModel Nothing Nothing = true
eqModel (Just r1) (Just r2) = eqR r1 r2
eqModel _ _ = false

genModel ∷ Gen.Gen Model
genModel = do
  isNothing ← arbitrary
  if isNothing
    then pure Nothing
    else map Just do
    scheme ← arbitrary
    address ← arbitrary
    zoom ← arbitrary
    lat ← arbitrary
    lng ← arbitrary
    pure
      { osmURI: Left $ URI.URI
        (map URI.URIScheme scheme)
        (URI.HierarchicalPart
         (Just $ URI.Authority Nothing [(URI.NameAddress address) × Nothing ])
         Nothing)
        Nothing
        Nothing
      , zoom
      , view: {lat, lng}
      }

encode ∷ Model → J.Json
encode Nothing = J.jsonNull
encode (Just r) =
  "configType" := "show-geo-chart"
  ~> "osmURI" := URI.printURIRef r.osmURI
  ~> "viewLat" := r.view.lat
  ~> "viewLng" := r.view.lng
  ~> "zoom" := r.zoom
  ~> J.jsonEmptyObject

decode ∷ J.Json → String ⊹ Model
decode js
  | J.isNull js = pure Nothing
  | otherwise = map Just $ decode' js
  where
  decode' ∷ J.Json → String ⊹ ModelR
  decode' js' = do
    obj ← J.decodeJson js'
    configType ← obj .? "configType"
    unless (configType ≡ "show-geo-chart")
      $ throwError "This is not show geo chart model"
    osmURIStr ← obj .? "osmURI"
    osmURI ← lmap show $ URI.runParseURIRef osmURIStr
    zoom ← obj .? "zoom"
    lat ← obj .? "viewLat"
    lng ← obj .? "lng"
    pure { osmURI
         , zoom
         , view: { lat, lng }
         }
