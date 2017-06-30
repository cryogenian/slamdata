module SlamData.Workspace.Card.Metric.Model where

import SlamData.Prelude

import Data.Argonaut ((:=), (~>), (.?))
import Data.Argonaut as J

import Test.StrongCheck.Arbitrary (arbitrary)
import Test.StrongCheck.Gen as Gen

type Model = Unit

initialModel ∷ Model
initialModel = unit

eqModel ∷ Model → Model → Boolean
eqModel _ _ = true

encode ∷ Model → J.Json
encode _ =
  "tag" := "metric"
  ~> J.jsonEmptyObject

decode ∷ J.Json → String ⊹ Model
decode = J.decodeJson >=> \obj → do
  tag ← obj .? "tag"
  unless (tag ≡ "metric") $ Left "this is not a metric"
  pure unit

genModel ∷ Gen.Gen Model
genModel = arbitrary
