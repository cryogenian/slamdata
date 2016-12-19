module SlamData.Workspace.Card.SetupFormInput.TextLike.Model where

import SlamData.Prelude

import Data.Argonaut (JCursor, Json, JObject, jsonEmptyObject, (~>), (:=), (.?))
import Data.Foldable as F

import SlamData.Common.Align (Align)

import Test.StrongCheck.Arbitrary (arbitrary)
import Test.StrongCheck.Gen as Gen
import Test.StrongCheck.Data.Argonaut (runArbJCursor)

type TextLikeR =
  { name ∷ String
  , value ∷ JCursor
  , verticalAlign ∷ Align
  , horizontalAlign ∷ Align
  }

type Model = Maybe TextLikeR

initialModel ∷ Model
initialModel = Nothing

eqTextLikeR ∷ TextLikeR → TextLikeR → Boolean
eqTextLikeR r1 r2 =
  F.and
    [ r1.name ≡ r2.name
    , r1.value ≡ r2.value
    , r1.verticalAlign ≡ r2.verticalAlign
    , r1.horizontalAlign ≡ r2.horizontalAlign
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
    verticalAlign ← arbitrary
    horizontalAlign ← arbitrary
    pure { name
         , value
         , verticalAlign
         , horizontalAlign
         }

encode ∷ TextLikeR → Json
encode r =
  "name" := r.name
  ~> "value" := r.value
  ~> "verticalAlign" := r.verticalAlign
  ~> "horizontalAlign" :=  r.horizontalAlign
  ~> jsonEmptyObject

decode ∷ JObject → String ⊹ TextLikeR
decode obj = do
  name ← obj .? "name"
  value ← obj .? "value"
  horizontalAlign ← obj .? "horizontalAlign"
  verticalAlign ← obj .? "verticalAlign"
  pure { name
       , value
       , horizontalAlign
       , verticalAlign
       }
