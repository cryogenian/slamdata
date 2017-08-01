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

module SlamData.Download.Model where

import SlamData.Prelude

import Data.Argonaut as J
import Data.Codec ((<~<), (>~>))
import Data.Codec as C
import Data.Codec.Argonaut.Compat as CA
import Data.Codec.Argonaut.Migration as CAM
import Data.Codec.Argonaut.Variant as CAV
import Data.Lens (Lens', lens)
import Data.MediaType (MediaType(..))
import Data.Path.Pathy as P
import Data.String as Str
import Data.String.Regex as Rx
import Data.String.Regex.Flags as RXF
import Data.Variant as V
import Global as Global
import Quasar.Paths as QP
import Network.HTTP.RequestHeader (RequestHeader(..))
import SlamData.Quasar as Q
import SlamData.FileSystem.Resource as R

type DownloadModel r =
  { resource ∷ R.Resource
  , targetName ∷ String
  , compress ∷ Boolean
  , options ∷ Either CSVOptions JSONOptions
  | r
  }

renderURL ∷ ∀ r. Array RequestHeader → DownloadModel r → String
renderURL authHeaders opts =
  let
    headers =
      "?request-headers="
        <> (Global.encodeURIComponent
             $ show
             $ Q.reqHeadersToJSON
             $ append authHeaders
             $ toHeaders opts
             $ Just (opts.targetName <> ext))
    ext = extension opts.compress opts.options
  in
    Q.encodeURI (P.printPath QP.data_ <> R.resourcePath opts.resource) <> headers

type DownloadOptions = Either CSVOptions JSONOptions

type CSVOptionsRec =
  { colDelimiter ∷ String
  , rowDelimiter ∷ String
  , quoteChar ∷ String
  , escapeChar ∷ String
  , arrays ∷ ArrayMode
  }

newtype CSVOptions = CSVOptions CSVOptionsRec

derive instance newtypeCSVOptions ∷ Newtype CSVOptions _
derive instance eqCSVOptions ∷ Eq CSVOptions
derive instance genericCSVOptions ∷ Generic CSVOptions _
instance showCSVOptions ∷ Show CSVOptions where show = genericShow

codecCSVOptions ∷ CA.JsonCodec CSVOptions
codecCSVOptions =
  _Newtype $ CA.object "CSVOptions" $ CA.record
    # CA.recordProp (SProxy ∷ SProxy "colDelimiter") CA.string
    # CA.recordProp (SProxy ∷ SProxy "rowDelimiter") CA.string
    # CA.recordProp (SProxy ∷ SProxy "quoteChar") CA.string
    # CA.recordProp (SProxy ∷ SProxy "escapeChar") CA.string
    # CA.recordProp (SProxy ∷ SProxy "arrays") codecArrayMode

initialCSVOptions ∷ CSVOptions
initialCSVOptions =
  CSVOptions
    { colDelimiter: ","
    , rowDelimiter: "\\n"
    , quoteChar: "\""
    , escapeChar: "\""
    , arrays: Flatten
    }

_CSVOptions ∷ Lens' CSVOptions CSVOptionsRec
_CSVOptions = _Newtype

_colDelimiter ∷ Lens' CSVOptions String
_colDelimiter = _CSVOptions <<< lens _.colDelimiter (_ { colDelimiter = _ })

_rowDelimiter ∷ Lens' CSVOptions String
_rowDelimiter = _CSVOptions <<< lens _.rowDelimiter (_ { rowDelimiter = _ })

_quoteChar ∷ Lens' CSVOptions String
_quoteChar = _CSVOptions <<< lens _.quoteChar (_ { quoteChar = _ })

_escapeChar ∷ Lens' CSVOptions String
_escapeChar = _CSVOptions <<< lens _.escapeChar (_ { escapeChar = _ })

_arrays ∷ Lens' CSVOptions ArrayMode
_arrays = _CSVOptions <<< lens _.arrays (_ { arrays = _ })

type JSONOptionsRec =
  { multivalues ∷ MultiValueMode
  , precision ∷ PrecisionMode
  }

newtype JSONOptions = JSONOptions JSONOptionsRec

derive instance newtypeJSONOptions ∷ Newtype JSONOptions _
derive instance eqJsonOptions ∷ Eq JSONOptions
derive instance genericJSONOptions ∷ Generic JSONOptions _
instance showJSONOptions ∷ Show JSONOptions where show = genericShow

codecJSONOptions ∷ CA.JsonCodec JSONOptions
codecJSONOptions =
  _Newtype $ CA.object "JSONOptions" $ CA.record
    # CA.recordProp (SProxy ∷ SProxy "multivalues") codecMultiValueMode
    # CA.recordProp (SProxy ∷ SProxy "precision") codecPrecisionMode

initialJSONOptions ∷ JSONOptions
initialJSONOptions =
  JSONOptions
    { multivalues: ArrayWrapped
    , precision: Readable
    }

_JSONOptions ∷ Lens' JSONOptions JSONOptionsRec
_JSONOptions = _Newtype

_multivalues ∷ Lens' JSONOptions MultiValueMode
_multivalues = _JSONOptions <<< lens _.multivalues (_ { multivalues = _ })

_precision ∷ Lens' JSONOptions PrecisionMode
_precision = _JSONOptions <<< lens _.precision (_ { precision = _ })

data ArrayMode = Flatten | Separate String

derive instance eqArrayMode ∷ Eq ArrayMode
derive instance genericArrayMode ∷ Generic ArrayMode _
instance showArrayMode ∷ Show ArrayMode where show = genericShow

codecArrayMode ∷ CA.JsonCodec ArrayMode
codecArrayMode =
  migrationCodec >~> dimap toVariant fromVariant
    (CAV.variant
      # CAV.variantCase _Separate (Right CA.string)
      # CAV.variantCase _Flatten (Left unit))
  where
  toVariant = case _ of
    Separate a → V.inj _Separate a
    Flatten → V.inj _Flatten unit
  fromVariant = V.case_
    # V.on _Separate Separate
    # V.on _Flatten (const Flatten)
  _Separate = SProxy ∷ SProxy "separate"
  _Flatten = SProxy ∷ SProxy "flatten"
  -- added in 4.2.3
  migrationCodec =
    CAM.renameField "ty" "tag" <~< CAM.renameField "val" "value"

separateValue ∷ ArrayMode → Maybe String
separateValue (Separate s) = Just s
separateValue _ = Nothing

data MultiValueMode = ArrayWrapped | LineDelimited

derive instance eqMultiValueMode ∷ Eq MultiValueMode
derive instance genericMultiValueMode ∷ Generic MultiValueMode _
instance showMultiValueMode ∷ Show MultiValueMode where show = genericShow

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

data PrecisionMode = Readable | Precise

derive instance eqPrecisionMode ∷ Eq PrecisionMode
derive instance genericPrecisionMode ∷ Generic PrecisionMode _
instance showPrecisionMode ∷ Show PrecisionMode where show = genericShow

-- TODO-codec: replace with generic-based codec?
codecPrecisionMode ∷ CA.JsonCodec PrecisionMode
codecPrecisionMode = C.basicCodec dec enc
  where
  dec j = case J.toString j of
    Just "Readable" → Right Readable
    Just "Precise" → Right Precise
    _ → Left (CA.UnexpectedValue j)
  enc = J.fromString ∘ case _ of
    Readable → "Readable"
    Precise → "Precise"

shouldCompress ∷ ∀ r. DownloadModel r → Boolean
shouldCompress state = not R.isFile state.resource || state.compress

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
  ∷ forall r
  . { compress ∷ Boolean
    , options ∷ Either CSVOptions JSONOptions
    | r
    }
  → Maybe String
  → Array RequestHeader
toHeaders r filename =
  [ RequestHeader "Accept-Encoding" "gzip"
  , Accept $ MediaType $ mimeType r.options <> ";disposition=\"attachment" <> encFilename <> "\""
  ]
  where
  encFilename ∷ String
  encFilename = case filename of
    Nothing → ""
    Just fn → "; filename*=UTF-8''" <> Global.encodeURIComponent fn

  mimeType ∷ Either CSVOptions JSONOptions → String
  mimeType (Left (CSVOptions opts)) =
    "text/csv"
    <> ";columnDelimiter=" <> esc opts.colDelimiter
    <> (if opts.rowDelimiter == "\\n"
        then ""
        else ";rowDelimiter=" <> esc opts.rowDelimiter)
    <> ";quoteChar=" <> esc opts.quoteChar
    <> ";escapeChar=" <> esc opts.escapeChar
  mimeType (Right (JSONOptions opts)) =
    let suffix = if opts.precision == Precise then ";mode=precise" else ""
        subtype = if opts.multivalues == ArrayWrapped then "json" else "ldjson"
    in "application/" <> subtype <> suffix
  esc ∷ String → String
  esc s =
    (\a → "\"" <> a <> "\"")
    $ Rx.replace (grx "\"") "\\\""
    $ Str.replace (Str.Pattern "\\t") (Str.Replacement "\t")
    $ Str.replace (Str.Pattern "\\r") (Str.Replacement "\r")
    s
    where
    grx ∷ String → Rx.Regex
    grx pat = unsafePartial fromRight $ Rx.regex pat RXF.global

data OutputType = CSV | JSON

derive instance eqOutputType ∷ Eq OutputType
derive instance genericOutputType ∷ Generic OutputType _
instance showOutputType ∷ Show OutputType where show = genericShow
