{-
Copyright 2015 SlamData, Inc.

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

module Model.Notebook.Port where

import Prelude
import Data.Argonaut.Combinators ((~>), (:=), (.?))
import Data.Argonaut.Core (jsonEmptyObject)
import Data.Argonaut.Decode (DecodeJson, decodeJson)
import Data.Argonaut.Encode (EncodeJson, encodeJson)
import Data.Maybe
import Data.StrMap
import Model.Path
import Model.Resource
import Optic.Core

type VarMapValue = String

-- | PortResourcePending is converted into a PortResource once a notebook is
-- | saved - before a notebook is saved, it is not possible to provide a
-- | filename for a PortResource for a temporary output file.
data Port
  = Closed
  | PortResourcePending
  | PortResource Resource
  | PortInvalid String
  | VarMap (StrMap VarMapValue)

_PortResource :: PrismP Port Resource
_PortResource = prism' PortResource $ \p -> case p of
  PortResource r -> Just r
  _ -> Nothing

_PortInvalid :: PrismP Port String
_PortInvalid = prism' PortInvalid $ \p -> case p of
  PortInvalid s -> Just s
  _ -> Nothing

_VarMap :: PrismP Port (StrMap VarMapValue)
_VarMap = prism' VarMap $ \p -> case p of
  VarMap m -> Just m
  _ -> Nothing

instance encodeJsonPort :: EncodeJson Port where
  encodeJson Closed
    =  "type" := "closed"
    ~> jsonEmptyObject
  encodeJson PortResourcePending
    =  "type" := "pending"
    ~> jsonEmptyObject
  encodeJson (PortResource res)
    =  "type" := "resource"
    ~> "res" := encodeJson res
    ~> jsonEmptyObject
  encodeJson (PortInvalid msg)
    =  "type" := "invalid"
    ~> "message" := msg
    ~> jsonEmptyObject
  encodeJson (VarMap sm)
    =  "type" := "map"
    ~> "content" := sm
    ~> jsonEmptyObject

instance decodeJsonPort :: DecodeJson Port where
  decodeJson json = do
    obj <- decodeJson json
    portType <- obj .? "type"
    case portType of
      "resource" -> PortResource <$> obj .? "res"
      "invalid" -> PortInvalid <$> obj .? "message"
      "map" -> VarMap <$> obj .? "content"
      "pending" -> pure PortResourcePending
      _ -> pure Closed
