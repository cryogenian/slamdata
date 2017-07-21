{-
Copyright 2017 SlamData, Inc.
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

module SlamData.Theme
  ( Theme(..)
  , getURI
  , codec
  ) where

import SlamData.Prelude
import Data.Argonaut as J
import Data.Codec as C
import Data.Codec.Argonaut as JC
import Data.Codec.Argonaut.Variant as JCV
import Data.Variant as V
import Data.URI as URI

data Theme
  = Light
  | Dark
  | Custom URI.URIRef

derive instance eqTheme ∷ Eq Theme

_Light = SProxy ∷ SProxy "light"

_Dark = SProxy ∷ SProxy "dark"

_Custom = SProxy ∷ SProxy "custom"

builtInThemeUri ∷ String → URI.URIRef
builtInThemeUri t =
  unsafePartial $ fromRight $ URI.runParseURIRef $ "./css/" <> t <> ".css"

getURI ∷ Theme → URI.URIRef
getURI = case _ of
  Light → builtInThemeUri "light"
  Dark → builtInThemeUri "dark"
  Custom u → u

-- At some point this should be somewhere generic or in the libaray
codecURIRef ∷ JC.JsonCodec URI.URIRef
codecURIRef =
  C.mapCodec parse URI.printURIRef JC.string
  where
    parse ∷ String → Either JC.JsonDecodeError URI.URIRef
    parse s =
      lmap (const (JC.UnexpectedValue (J.fromString s))) (URI.runParseURIRef s)

codec ∷ JC.JsonCodec Theme
codec =
  dimap toVariant fromVariant themeVariantCase
  where
    toVariant = case _ of
      Light → V.inj _Light unit
      Dark → V.inj _Dark  unit
      Custom c → V.inj _Custom c

    fromVariant =
      V.case_
        # V.on _Light (const Light)
        # V.on _Dark (const Dark)
        # V.on _Custom Custom

    themeVariantCase =
      JCV.variant
        # JCV.variantCase _Light (Left unit)
        # JCV.variantCase _Dark (Left unit)
        # JCV.variantCase _Custom (Right codecURIRef)
