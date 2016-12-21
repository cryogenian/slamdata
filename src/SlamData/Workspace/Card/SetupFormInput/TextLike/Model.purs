module SlamData.Workspace.Card.SetupFormInput.TextLike.Model where

import SlamData.Prelude

import Data.Argonaut (JCursor, Json, JObject, jsonEmptyObject, (~>), (:=), (.?))
import Data.Foldable as F

import Test.StrongCheck.Arbitrary (arbitrary)
import Test.StrongCheck.Gen as Gen
import Test.StrongCheck.Data.Argonaut (runArbJCursor)

type TextLikeR =
  { name ∷ String
  , value ∷ JCursor
  }

type Model = Maybe TextLikeR

initialModel ∷ Model
initialModel = Nothing

eqTextLikeR ∷ TextLikeR → TextLikeR → Boolean
eqTextLikeR r1 r2 =
  F.and
    [ r1.name ≡ r2.name
    , r1.value ≡ r2.value
    ]

eqModel ∷ Model → Model → Boolean
eqModel Nothing Nothing = true
eqModel (Just r1) (Just r2) = eqTextLikeR r1 r2
eqModel _ _ = false

genModel ∷ Gen.Gen Model
genModel = do
  isNothing ← arbitrary
  if isNothing
    then pure Nothing
    else map Just do
    name ← arbitrary
    value ← map runArbJCursor arbitrary
    pure { name
         , value
         }

encode ∷ TextLikeR → Json
encode r =
  "name" := r.name
  ~> "value" := r.value
  ~> jsonEmptyObject

decode ∷ JObject → String ⊹ TextLikeR
decode obj = do
  name ← obj .? "name"
  value ← obj .? "value"
  pure { name
       , value
       }
