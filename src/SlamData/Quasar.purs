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

module SlamData.Quasar where

import SlamData.Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Exception as Exn

import Data.Argonaut ((~>), (:=))
import Data.Argonaut as JS
import Data.Array as Arr
import Data.MediaType (MediaType(..), mediaTypeToString)
import Data.String as S

import Global (encodeURIComponent)

import Network.HTTP.RequestHeader (RequestHeader(..))

import Quasar.Advanced.Auth.Provider (Provider) as Auth
import Quasar.Advanced.QuasarAF as QF

import SlamData.Quasar.Aff (QEff, runQuasarF)

ldJSON ∷ MediaType
ldJSON = MediaType "application/ldjson"

encodeURI ∷ String → String
encodeURI str =
  let
    str' = fromMaybe str $ S.stripPrefix "." str

    encode ∷ String → String
    encode = encodeURIComponent

    qmSplit ∷ Array String
    qmSplit = S.split "?" str'

    ampSplit ∷ Maybe (Array String)
    ampSplit = map (S.split "&") $ qmSplit Arr.!! 1

    eqSplit ∷ Maybe (Array (Array String))
    eqSplit = map (map $ S.split "=") $ ampSplit

    maybeModify ∷ ∀ a. Int → (a → a) → Array a → Array a
    maybeModify ix fn arr =
      fromMaybe arr $ Arr.modifyAt ix fn arr

    eqSplitEncoded ∷ Maybe (Array (Array String))
    eqSplitEncoded = map (map (maybeModify 1 encode)) eqSplit

    eqMerged ∷ Maybe (Array String)
    eqMerged = map (map (S.joinWith "=")) eqSplitEncoded

    ampMerged ∷ Maybe String
    ampMerged = map (S.joinWith "&") eqMerged

    slashSplit ∷ Maybe (Array String)
    slashSplit = map (S.split "/") $ Arr.head qmSplit

    slashSplitEncoded ∷ Maybe (Array String)
    slashSplitEncoded = map (map encode) $ slashSplit

    slashMerged ∷ Maybe String
    slashMerged = map (S.joinWith "/") slashSplitEncoded

    afterQM ∷ String
    afterQM = foldMap ("?" ⊕ _) ampMerged

    beforeQM ∷ String
    beforeQM = fromMaybe "" slashMerged

  in
    beforeQM ⊕ afterQM

reqHeadersToJSON ∷ Array RequestHeader → JS.Json
reqHeadersToJSON = foldl go JS.jsonEmptyObject
  where
  go obj (Accept mime) = "Accept" := mediaTypeToString mime ~> obj
  go obj (ContentType mime) = "Content-Type" := mediaTypeToString mime ~> obj
  go obj (RequestHeader k v) = k := v ~> obj

-- | Returns `Nothing` in case the authorization service is not available, and `Just` in case
-- | Quasar responded with a valid array of OIDC providers.
retrieveAuthProviders
  ∷ ∀ eff
  . Aff (QEff eff) (Either Exn.Error (Maybe (Array Auth.Provider)))
retrieveAuthProviders =
  runQuasarF QF.authProviders <#> case _ of
    Left (QF.Error err) → Left err
    Left QF.NotFound → Right Nothing
    Right providers → Right (Just providers)
