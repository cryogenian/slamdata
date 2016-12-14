module SlamData.Workspace.Card.SetupFormInput.Checkbox.Model
  ( encode
  , decode
  , module SlamData.Workspace.Card.SetupFormInput.Labeled.Model
  ) where

import SlamData.Prelude

import Data.Argonaut (Json, decodeJson, (~>), (:=), (.?), isNull, jsonNull)

import SlamData.Workspace.Card.SetupFormInput.Labeled.Model
  ( Model
  , initialModel
  , eqModel
  , genModel
  )
import SlamData.Workspace.Card.SetupFormInput.Labeled.Model as Lbl

encode ∷ Model → Json
encode = case _ of
  Nothing → jsonNull
  Just r →
    "formInputType" := "checkbox"
    ~> Lbl.encode r

decode ∷ Json → String ⊹ Model
decode js
  | isNull js = pure Nothing
  | otherwise = map Just $ do
    obj ← decodeJson js
    fiType ← obj .? "formInputType"
    unless (fiType ≡ "checkbox")
      $ throwError "This is not checkbox form input setup"
    Lbl.decode obj
