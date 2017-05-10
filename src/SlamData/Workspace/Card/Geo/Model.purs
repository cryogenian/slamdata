module SlamData.Workspace.Card.Geo.Model where

import SlamData.Prelude

import Data.Argonaut as J
import Data.Argonaut ((~>), (:=), (.?))

import Test.StrongCheck.Arbitrary (arbitrary)
import Test.StrongCheck.Gen as Gen

type ModelR = Unit

type Model = Maybe ModelR

initialModel ∷ Model
initialModel = Nothing

eqR ∷ ModelR → ModelR → Boolean
eqR _ _ = true

eqModel ∷ Model → Model → Boolean
eqModel _ _ = true

genModel ∷ Gen.Gen Model
genModel = do
  isNothing ← arbitrary
  pure if isNothing
    then Nothing
    else Just unit

encode ∷ Model → J.Json
encode Nothing = J.jsonNull
encode (Just r) =
  "configType" := "show-geo-chart"
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
    pure unit
