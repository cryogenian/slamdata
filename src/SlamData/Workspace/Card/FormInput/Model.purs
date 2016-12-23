module SlamData.Workspace.Card.FormInput.Model where

import SlamData.Prelude

import Data.Array as Arr
import Data.Argonaut (Json, decodeJson, (~>), (:=), (.?), jsonEmptyObject)
import Data.Set as Set

import Test.StrongCheck.Arbitrary (arbitrary)
import Test.StrongCheck.Gen as Gen

import SlamData.Workspace.Card.BuildChart.Semantics as Sem

data Model
  = TextLike String
  | Labeled (Set.Set Sem.Semantics)

initialModel ∷ Model
initialModel = TextLike ""

eqModel ∷ Model → Model → Boolean
eqModel (TextLike t) (TextLike tt) = t ≡ tt
eqModel (Labeled s) (Labeled ss) = s ≡ ss
eqModel _ _ = false

genModel ∷ Gen.Gen Model
genModel = do
  isTextLike ← arbitrary
  case isTextLike of
    true →
      TextLike <$> arbitrary
    false →
      map (Labeled ∘ Set.fromFoldable) $ Gen.arrayOf arbitrary

encode ∷ Model → Json
encode (TextLike str) =
  "modelType" := "textLike"
  ~> "value" := str
  ~> jsonEmptyObject
encode (Labeled selected) =
  "modelType" := "labeled"
  ~> "selected" := foldMap Arr.singleton selected
  ~> jsonEmptyObject

decode ∷ Json → String ⊹ Model
decode = decodeJson >=> \obj → do
  mType ← obj .? "modelType"
  case mType of
    "textLike" → do
      v ← obj .? "value"
      pure $ TextLike v
    "labeled" → do
      (arr ∷ Array Sem.Semantics) ← obj .? "selected"
      pure $ Labeled $ Set.fromFoldable arr
    _ →
      Left "Incorrect form input model type"
