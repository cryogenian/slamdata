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

import Data.Argonaut (class EncodeJson, class DecodeJson, encodeJson, decodeJson, (:=), (~>), (.?), jsonEmptyObject)
import Data.Lens (LensP, lens)
import Data.MediaType (MediaType(..))
import Data.String as Str
import Data.String.Regex as Rx

import Network.HTTP.RequestHeader (RequestHeader(..))

type DownloadOptions = Either CSVOptions JSONOptions

type CSVOptionsRec =
  { colDelimiter :: String
  , rowDelimiter :: String
  , quoteChar :: String
  , escapeChar :: String
  , arrays :: ArrayMode
  }
newtype CSVOptions = CSVOptions CSVOptionsRec

instance eqCSVOptions :: Eq CSVOptions where
  eq (CSVOptions opts) (CSVOptions opts') =
    opts.colDelimiter == opts'.colDelimiter
    && opts.rowDelimiter == opts'.rowDelimiter
    && opts.quoteChar == opts'.quoteChar
    && opts.escapeChar == opts'.escapeChar
    && opts.arrays == opts'.arrays

instance encodeJsonCSVOptions :: EncodeJson CSVOptions where
  encodeJson (CSVOptions r) =
       "colDelimiter" := r.colDelimiter
    ~> "rowDelimiter" := r.rowDelimiter
    ~> "quoteChar" := r.quoteChar
    ~> "escapeChar" := r.escapeChar
    ~> "arrays" := r.arrays
    ~> jsonEmptyObject

instance decodeJsonCSVOptions :: DecodeJson CSVOptions where
  decodeJson json = do
    obj <- decodeJson json
    r <- { colDelimiter: _
         , rowDelimiter: _
         , quoteChar: _
         , escapeChar: _
         , arrays: _
         }
         <$> (obj .? "colDelimiter")
         <*> (obj .? "rowDelimiter")
         <*> (obj .? "quoteChar")
         <*> (obj .? "escapeChar")
         <*> (obj .? "arrays")
    pure $ CSVOptions r

initialCSVOptions :: CSVOptions
initialCSVOptions =
  CSVOptions { colDelimiter: ","
             , rowDelimiter: "\\n"
             , quoteChar: "\""
             , escapeChar: "\""
             , arrays: Flatten
             }

_CSVOptions :: LensP CSVOptions CSVOptionsRec
_CSVOptions = lens (\(CSVOptions obj) -> obj) (const CSVOptions)

_colDelimiter :: LensP CSVOptions String
_colDelimiter = _CSVOptions <<< lens _.colDelimiter (_ { colDelimiter = _ })

_rowDelimiter :: LensP CSVOptions String
_rowDelimiter = _CSVOptions <<< lens _.rowDelimiter (_ { rowDelimiter = _ })

_quoteChar :: LensP CSVOptions String
_quoteChar = _CSVOptions <<< lens _.quoteChar (_ { quoteChar = _ })

_escapeChar :: LensP CSVOptions String
_escapeChar = _CSVOptions <<< lens _.escapeChar (_ { escapeChar = _ })

_arrays :: LensP CSVOptions ArrayMode
_arrays = _CSVOptions <<< lens _.arrays (_ { arrays = _ })

type JSONOptionsRec =
  { multivalues :: MultiValueMode
  , precision :: PrecisionMode
  }
newtype JSONOptions = JSONOptions JSONOptionsRec

instance eqJsonOptions :: Eq JSONOptions where
  eq (JSONOptions opts) (JSONOptions opts') =
    opts.multivalues == opts'.multivalues && opts.precision == opts'.precision

instance encodeJsonJSONOptions :: EncodeJson JSONOptions where
  encodeJson (JSONOptions r) =
       "multivalues" := r.multivalues
    ~> "precision" := r.precision
    ~> jsonEmptyObject

instance decodeJsonJSONOptions :: DecodeJson JSONOptions where
  decodeJson json = do
    obj <- decodeJson json
    r <- { multivalues: _, precision: _ }
         <$> (obj .? "multivalues")
         <*> (obj .? "precision")
    pure $ JSONOptions r

initialJSONOptions :: JSONOptions
initialJSONOptions = JSONOptions
  { multivalues: ArrayWrapped
  , precision: Readable
  }

_JSONOptions :: LensP JSONOptions JSONOptionsRec
_JSONOptions = lens (\(JSONOptions obj) -> obj) (const JSONOptions)

_multivalues :: LensP JSONOptions MultiValueMode
_multivalues = _JSONOptions <<< lens _.multivalues (_ { multivalues = _ })

_precision :: LensP JSONOptions PrecisionMode
_precision = _JSONOptions <<< lens _.precision (_ { precision = _ })

data ArrayMode = Flatten | Separate String

instance eqArrayMode :: Eq ArrayMode where
  eq Flatten Flatten = true
  eq (Separate s) (Separate s') = s == s'
  eq _ _ = false

instance encodeJsonArrayMode :: EncodeJson ArrayMode where
  encodeJson Flatten =
    "ty" := "flatten"
    ~> "val" := ""
    ~> jsonEmptyObject
  encodeJson (Separate str) =
    "ty" := "separate"
    ~> "val" := str
    ~> jsonEmptyObject

instance decodeJsonArrayMode :: DecodeJson ArrayMode where
  decodeJson json = do
    obj <- decodeJson json
    ty <- obj .? "ty"
    val <- obj .? "val"
    case ty of
      "flatten" -> Right Flatten
      "separate" -> Right $ Separate val
      _ -> Left "Incorrect ArrayMode encoding"

separateValue :: ArrayMode -> Maybe String
separateValue (Separate s) = Just s
separateValue _ = Nothing

data MultiValueMode = ArrayWrapped | LineDelimited
data PrecisionMode = Readable | Precise

instance eqMultiValueMode :: Eq MultiValueMode where
  eq ArrayWrapped ArrayWrapped = true
  eq LineDelimited LineDelimited = true
  eq _ _ = false

instance eqPrecisionMode :: Eq PrecisionMode where
  eq Readable Readable = true
  eq Precise Precise = true
  eq _ _ = false

instance encodeJsonMultivalueMode :: EncodeJson MultiValueMode where
  encodeJson ArrayWrapped = encodeJson "ArrayWrapped"
  encodeJson LineDelimited = encodeJson "LineDelimited"

instance encodeJsonPrecision :: EncodeJson PrecisionMode where
  encodeJson Readable = encodeJson "Readable"
  encodeJson Precise = encodeJson "Precise"

instance decodeJsonMultiValueMode :: DecodeJson MultiValueMode where
  decodeJson json = do
    str <- decodeJson json
    case str of
      "ArrayWrapped" -> pure ArrayWrapped
      "LineDelimited" -> pure LineDelimited
      _ -> Left "Incorrect MultiValueMode"

instance decodeJsonPrecision :: DecodeJson PrecisionMode where
  decodeJson json = do
    str <- decodeJson json
    case str of
      "Readable" -> pure Readable
      "Precise" -> pure Precise
      _ -> Left "Incorrect Precision"

toHeaders
  :: forall r
   . { compress :: Boolean
     , options :: Either CSVOptions JSONOptions|r}
  -> Array RequestHeader
toHeaders r =
  encHeaderArray r.compress ++ [ acceptHeader ]
  where
  acceptHeader :: RequestHeader
  acceptHeader = Accept $ MediaType $ mimeType r.options ++ attachmentDisposition

  encHeaderArray :: Boolean -> Array RequestHeader
  encHeaderArray true = [ RequestHeader "Accept-Encoding" "gzip" ]
  encHeaderArray _ = [ ]

  attachmentDisposition = ";disposition=attachment"

  mimeType :: Either CSVOptions JSONOptions -> String
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
    in "application/" ++ subtype ++ suffix
  esc :: String -> String
  esc s =
    (\a -> "\"" <> a <> "\"")
    $ Rx.replace (grx "\"") "\\\""
    $ Str.replace "\\t" "\t"
    $ Str.replace "\\r" "\r"
    s
    where
    grx :: String -> Rx.Regex
    grx pat = Rx.regex pat Rx.noFlags { global = true }

data OutputType = CSV | JSON
