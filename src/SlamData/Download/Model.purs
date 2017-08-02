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

module SlamData.Download.Model
  ( module SlamData.Download.Model
  , module Exports
  ) where

import SlamData.Prelude

import Data.Argonaut as J
import Data.Codec ((>~>))
import Data.Codec as C
import Data.Codec.Argonaut.Compat as CA
import Data.Codec.Argonaut.Migration as CAM
import Data.Lens (Lens')
import Data.Lens.Record as LR
import Data.MediaType (MediaType(..))
import Data.Newtype (un)
import Data.Path.Pathy as P
import Data.String as Str
import Global as Global
import Network.HTTP.RequestHeader (RequestHeader(..))
import Quasar.Data (JSONMode(..)) as Exports
import Quasar.Data.CSVOptions as CSV
import Quasar.Data.JSONMode as JSON
import Quasar.Paths as QP
import SlamData.FileSystem.Resource as R
import SlamData.Quasar as Q

type DownloadModel r =
  { targetName ∷ String
  , compress ∷ Boolean
  , options ∷ Either CSVOptions JSONOptions
  | r
  }

initialOptions ∷ R.Resource → Either CSVOptions JSONOptions
initialOptions res
  | R.isWorkspace res = Right initialJSONOptions
  | otherwise = Left initialCSVOptions

renderURL ∷ ∀ r. Array RequestHeader → DownloadModel (resource ∷ R.Resource | r) → String
renderURL authHeaders opts =
  let
    headers =
      "?request-headers="
        <> (Global.encodeURIComponent
             $ show
             $ Q.reqHeadersToJSON
             $ append authHeaders
             $ toHeaders opts (shouldCompress opts)
             $ Just (opts.targetName <> ext))
    ext = extension opts.compress opts.options
  in
    Q.encodeURI (P.printPath QP.data_ <> R.resourcePath opts.resource) <> headers

type DownloadOptions = Either CSVOptions JSONOptions

newtype CSVOptions = CSVOptions CSV.CSVOptions

derive instance newtypeCSVOptions ∷ Newtype CSVOptions _
derive instance eqCSVOptions ∷ Eq CSVOptions

codecCSVOptions ∷ CA.JsonCodec CSVOptions
codecCSVOptions =
  migrationCodec >~> (_Newtype $ CA.object "CSVOptions" $ CA.record
    # CA.recordProp (SProxy ∷ SProxy "columnDelimiter") CA.string
    # CA.recordProp (SProxy ∷ SProxy "rowDelimiter") CA.string
    # CA.recordProp (SProxy ∷ SProxy "quoteChar") CA.string
    # CA.recordProp (SProxy ∷ SProxy "escapeChar") CA.string)
  where
    -- Added in 4.2.5
    migrationCodec = CAM.renameField "colDelimiter" "columnDelimiter"

initialCSVOptions ∷ CSVOptions
initialCSVOptions = CSVOptions CSV.defaultCSVOptions

_colDelimiter ∷ Lens' CSVOptions String
_colDelimiter = _Newtype ∘ LR.prop (SProxy ∷ SProxy "columnDelimiter")

_rowDelimiter ∷ Lens' CSVOptions String
_rowDelimiter = _Newtype ∘ LR.prop (SProxy ∷ SProxy "rowDelimiter")

_quoteChar ∷ Lens' CSVOptions String
_quoteChar = _Newtype ∘ LR.prop (SProxy ∷ SProxy "quoteChar")

_escapeChar ∷ Lens' CSVOptions String
_escapeChar = _Newtype ∘ LR.prop (SProxy ∷ SProxy "escapeChar")

type JSONOptionsRec =
  { multivalues ∷ MultiValueMode
  , precision ∷ JSON.JSONMode
  }

newtype JSONOptions = JSONOptions JSONOptionsRec

derive instance newtypeJSONOptions ∷ Newtype JSONOptions _
derive instance eqJsonOptions ∷ Eq JSONOptions

codecJSONOptions ∷ CA.JsonCodec JSONOptions
codecJSONOptions =
  _Newtype $ CA.object "JSONOptions" $ CA.record
    # CA.recordProp (SProxy ∷ SProxy "multivalues") codecMultiValueMode
    # CA.recordProp (SProxy ∷ SProxy "precision") codecJSONMode

initialJSONOptions ∷ JSONOptions
initialJSONOptions =
  JSONOptions
    { multivalues: ArrayWrapped
    , precision: JSON.Readable
    }

_multivalues ∷ Lens' JSONOptions MultiValueMode
_multivalues = _Newtype ∘ LR.prop (SProxy ∷ SProxy "multivalues")

_precision ∷ Lens' JSONOptions JSON.JSONMode
_precision = _Newtype ∘ LR.prop (SProxy ∷ SProxy "precision")

data MultiValueMode = ArrayWrapped | LineDelimited

derive instance eqMultiValueMode ∷ Eq MultiValueMode

-- TODO-codec: replace with generic-based codec?
codecMultiValueMode ∷ CA.JsonCodec MultiValueMode
codecMultiValueMode = C.basicCodec dec enc
  where
  dec j = case J.toString j of
    Just "ArrayWrapped" → Right ArrayWrapped
    Just "LineDelimited" → Right LineDelimited
    _ → Left (CA.UnexpectedValue j)
  enc = J.fromString ∘ case _ of
    ArrayWrapped → "ArrayWrapped"
    LineDelimited → "LineDelimited"

-- TODO-codec: replace with generic-based codec?
codecJSONMode ∷ CA.JsonCodec JSON.JSONMode
codecJSONMode = C.basicCodec dec enc
  where
  dec j = case J.toString j of
    Just "Readable" → Right JSON.Readable
    Just "Precise" → Right JSON.Precise
    _ → Left (CA.UnexpectedValue j)
  enc = J.fromString ∘ case _ of
    JSON.Readable → "Readable"
    JSON.Precise → "Precise"

alwaysCompress ∷ R.Resource → Boolean
alwaysCompress = not R.isFile

shouldCompress ∷ ∀ r. DownloadModel (resource ∷ R.Resource | r) → Boolean
shouldCompress r = alwaysCompress r.resource || r.compress

extension ∷ Boolean → Either CSVOptions JSONOptions → String
extension compress options
  | compress = ".zip"
  | otherwise = case options of
      Right (JSONOptions { multivalues: LineDelimited }) →  ".ldjson"
      Right _ → ".json"
      Left _ → ".csv"

validFilename ∷ String → Either String String
validFilename s =
  if not Str.null s && isJust (Str.indexOf (Str.Pattern "/") s)
    then Left s
    else Right s

toHeaders
  ∷ ∀ r
  . DownloadModel r
  → Boolean
  → Maybe String
  → Array RequestHeader
toHeaders r compress filename =
  [ RequestHeader "Accept-Encoding" "gzip"
  , Accept $ MediaType
      $ mimeCompress
      <> un MediaType (mimeType r.options)
      <> ";disposition=\"attachment" <> encFilename <> "\""
  ]
  where
  encFilename ∷ String
  encFilename = case filename of
    Nothing → ""
    Just fn → "; filename*=UTF-8''" <> Global.encodeURIComponent fn

  mimeCompress ∷ String
  mimeCompress = if compress then "application/zip," else ""

  mimeType ∷ Either CSVOptions JSONOptions → MediaType
  mimeType (Left (CSVOptions opts)) = CSV.toMediaType opts
  mimeType (Right (JSONOptions opts)) =
    let subtype = if opts.multivalues == ArrayWrapped then "json" else "ldjson"
    in JSON.decorateMode (MediaType ("application/" <> subtype)) opts.precision

data OutputType = CSV | JSON

derive instance eqOutputType ∷ Eq OutputType
