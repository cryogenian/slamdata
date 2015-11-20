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

module Dialog.Download.State where

import Prelude

import Data.Array (findIndex)
import Data.Either (Either(..), isLeft)
import Data.Either.Unsafe as U
import Data.Lens (LensP(), lens, (^.), (?~), (.~))
import Data.Maybe (Maybe(..), isJust)
import Data.String as Str
import Data.String.Regex as Rx

import Model.Resource (Resource(), resourceName, root, getPath)
import Network.HTTP.MimeType (MimeType(..))
import Network.HTTP.RequestHeader (RequestHeader(..))

data OutputType = CSV | JSON

type CSVOptionsRec =
  { colDelimiter :: String
  , rowDelimiter :: String
  , quoteChar :: String
  , escapeChar :: String
  , arrays :: ArrayMode
  }
newtype CSVOptions = CSVOptions CSVOptionsRec

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


toHeaders :: State -> Array RequestHeader
toHeaders r =
  encHeader (r ^. _compress) <> [ Accept $ MimeType $ mimeType (r ^. _options)]
  where
  encHeader :: Boolean -> Array RequestHeader
  encHeader true = [ RequestHeader "Accept-Encoding" "gzip" ]
  encHeader _ = [ ]

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


type StateRec =
  { source :: Either String Resource
  , sources :: (Array Resource)
  , showSourcesList :: Boolean
  , targetName :: Either String String
  , compress :: Boolean
  , options :: Either CSVOptions JSONOptions
  , error :: Maybe String
  }
newtype State = State StateRec


initialState :: Resource -> State
initialState res =
   State { source: Right res
         , sources: [root]
         , showSourcesList: false
         , targetName: let name = resourceName res
                       in Right $ if name == "" then "archive" else name
         , compress: false
         , options: Left initialCSVOptions
         , error: Nothing
         }

_State :: LensP State StateRec
_State = lens (\(State obj) -> obj) (const State)

_source :: LensP State (Either String Resource)
_source = _State <<< lens _.source (_ { source = _ })

_sources :: LensP State (Array Resource)
_sources = _State <<< lens _.sources (_ { sources = _ })

_showSourcesList :: LensP State Boolean
_showSourcesList = _State <<< lens _.showSourcesList (_ { showSourcesList = _ })

_targetName :: LensP State (Either String String)
_targetName = _State <<< lens _.targetName (_ { targetName = _ })

_compress :: LensP State Boolean
_compress = _State <<< lens _.compress (_ { compress = _ })

_options :: LensP State (Either CSVOptions JSONOptions)
_options = _State <<< lens _.options (_ { options = _ })

_error :: LensP State (Maybe String)
_error = _State <<< lens _.error (_ { error = _ })

validate :: State -> State
validate r
  | isLeft (r ^. _source) =
    r # _error ?~ "Please enter a valid source path to download"
  | not $ checkExists (U.fromRight $ r ^. _source) (r ^. _sources) =
    r # _error ?~ "The source resource does not exists"
  | isLeft (r ^. _targetName) =
    r # _error ?~ "Please enter a valid target filename"
  | otherwise =
    r # _error .~ Nothing

checkExists :: Resource -> Array Resource -> Boolean
checkExists r rs =
  let path = getPath r
  in isJust $ findIndex (getPath >>> eq path) rs
