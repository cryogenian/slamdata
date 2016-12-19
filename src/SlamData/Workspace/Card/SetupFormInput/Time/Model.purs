module SlamData.Workspace.Card.SetupFormInput.Time.Model
  ( encode
  , decode
  , module SlamData.Workspace.Card.SetupFormInput.TextLike.Model
  ) where

import SlamData.Prelude

import Data.Argonaut (Json, decodeJson, (~>), (:=), (.?), isNull, jsonNull)

import SlamData.Workspace.Card.SetupFormInput.TextLike.Model
  ( Model
  , initialModel
  , eqModel
  , genModel
  )
import SlamData.Workspace.Card.SetupFormInput.TextLike.Model as Txt

encode ∷ Model → Json
encode = case _ of
  Nothing → jsonNull
  Just r →
    "formInputType" := "time"
    ~> Txt.encode r

decode ∷ Json → String ⊹ Model
decode js
  | isNull js = pure Nothing
  | otherwise = map Just $ do
    obj ← decodeJson js
    fiType ← obj .? "formInputType"
    unless (fiType ≡ "time")
      $ throwError "This is not text form input setup"
    Txt.decode obj
