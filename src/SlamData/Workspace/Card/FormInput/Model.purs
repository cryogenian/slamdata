module SlamData.Workspace.Card.FormInput.Model where

import SlamData.Prelude

import Data.Argonaut (Json, JCursor, decodeJson, (~>), (:=), isNull, jsonNull, (.?), jsonEmptyObject)
import Data.Foldable as F

import Test.StrongCheck.Arbitrary (arbitrary)
import Test.StrongCheck.Gen as Gen
import Test.StrongCheck.Data.Argonaut (runArbJCursor)

import SlamData.Workspace.Card.CardType.FormInputType (FormInputType)

type FormInputR =
  { value ∷ JCursor
  , label ∷ Maybe JCursor
  , name ∷ Maybe JCursor
  , selected ∷ Maybe JCursor
  , fiType ∷ FormInputType
  }

type Model = Maybe FormInputR

eqR ∷ FormInputR → FormInputR → Boolean
eqR r1 r2 =
  F.and
    [ r1.value ≡ r2.value
    , r1.label ≡ r2.label
    , r1.name ≡ r2.name
    , r1.selected ≡ r2.selected
    , r1.fiType ≡ r2.fiType
    ]

initialModel ∷ Model
initialModel = Nothing

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
    value ← map runArbJCursor arbitrary
    label ← map (map runArbJCursor) arbitrary
    name ← map (map runArbJCursor) arbitrary
    selected ← map (map runArbJCursor) arbitrary
    fiType ← arbitrary
    pure { value
         , label
         , name
         , selected
         , fiType
         }

encode ∷ Model → Json
encode Nothing = jsonNull
encode (Just r) =
  "value" := r.value
  ~> "label" := r.label
  ~> "name" := r.name
  ~> "selected" := r.selected
  ~> "fiType" := r.fiType
  ~> jsonEmptyObject

decode ∷ Json → String ⊹ Model
decode js
  | isNull js = pure Nothing
  | otherwise = map Just do
    obj ← decodeJson js
    fiType ← obj .? "fiType"
    selected ← obj .? "selected"
    name ← obj .? "name"
    label ← obj .? "label"
    value ← obj .? "value"
    pure { fiType
         , selected
         , name
         , label
         ,  value
         }
