{-
Copyright 2016 SlamData, Inc.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
-}

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
