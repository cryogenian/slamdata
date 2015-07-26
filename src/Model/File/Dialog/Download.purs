module Model.File.Dialog.Download where

import Prelude
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Path.Pathy (rootDir)
import Model.Resource (Resource(..), resourceName, root)
import Optic.Core 
import Optic.Extended (TraversalP())
import Network.HTTP.MimeType (MimeType(..))
import Network.HTTP.RequestHeader (RequestHeader(..))

import qualified Data.String.Regex as Rx

data OutputType = CSV | JSON

newtype DownloadDialogRec = DownloadDialogRec
  { source :: Either String Resource
  , sources :: (Array Resource)
  , showSourcesList :: Boolean
  , targetName :: Either String String
  , compress :: Boolean
  , options :: Either CSVOptions JSONOptions
  , error :: Maybe String
  }

initialDownloadDialog :: Resource -> DownloadDialogRec
initialDownloadDialog res = DownloadDialogRec
  { source: Right res
  , sources: [root]
  , showSourcesList: false
  , targetName: let name = resourceName res
                in Right $ if name == "" then "archive" else name
  , compress: false
  , options: Left initialCSVOptions
  , error: Nothing
  }

_DownloadDialogRec :: LensP DownloadDialogRec _
_DownloadDialogRec = lens (\(DownloadDialogRec obj) -> obj) (const DownloadDialogRec)

_source :: LensP DownloadDialogRec (Either String Resource)
_source = _DownloadDialogRec <<< lens _.source (_ { source = _ })

_sources :: LensP DownloadDialogRec (Array Resource)
_sources = _DownloadDialogRec <<< lens _.sources (_ { sources = _ })

_showSourcesList :: LensP DownloadDialogRec Boolean
_showSourcesList = _DownloadDialogRec <<< lens _.showSourcesList (_ { showSourcesList = _ })

_targetName :: LensP DownloadDialogRec (Either String String)
_targetName = _DownloadDialogRec <<< lens _.targetName (_ { targetName = _ })

_compress :: LensP DownloadDialogRec Boolean
_compress = _DownloadDialogRec <<< lens _.compress (_ { compress = _ })

_options :: LensP DownloadDialogRec (Either CSVOptions JSONOptions)
_options = _DownloadDialogRec <<< lens _.options (_ { options = _ })

_error :: LensP DownloadDialogRec (Maybe String)
_error = _DownloadDialogRec <<< lens _.error (_ { error = _ })

newtype CSVOptions = CSVOptions
  { colDelimiter :: String
  , rowDelimiter :: String
  , quoteChar :: String
  , escapeChar :: String
  , arrays :: ArrayMode
  }

initialCSVOptions :: CSVOptions
initialCSVOptions = CSVOptions
  { colDelimiter: ","
  , rowDelimiter: "\\n"
  , quoteChar: "\""
  , escapeChar: "\""
  , arrays: Flatten
  }

_CSVOptions :: LensP CSVOptions _
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

newtype JSONOptions = JSONOptions
  { multivalues :: MultiValueMode
  , precision :: PrecisionMode
  }

initialJSONOptions :: JSONOptions
initialJSONOptions = JSONOptions
  { multivalues: ArrayWrapped
  , precision: Readable
  }

_JSONOptions :: LensP JSONOptions _
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


toHeaders :: DownloadDialogRec -> Array RequestHeader
toHeaders rec = encHeader (rec ^. _compress) ++ [Accept $ MimeType $ mimeType (rec ^. _options)]
  where

  encHeader :: Boolean -> Array RequestHeader
  encHeader true = [RequestHeader "Accept-Encoding" "gzip"]
  encHeader false = []

  mimeType :: Either CSVOptions JSONOptions -> String
  mimeType (Left (CSVOptions opts)) =
    "text/csv;columnDelimiter=" ++ esc opts.colDelimiter
         ++ "&rowDelimiter=" ++ esc opts.rowDelimiter
         ++ "&quoteChar=" ++ esc opts.quoteChar
         ++ "&escapeChar=" ++ esc opts.escapeChar
  mimeType (Right (JSONOptions opts)) =
    let suffix = if opts.precision == Precise then ";mode=precise" else ""
        subtype = if opts.multivalues == ArrayWrapped then "json" else "ldjson"
    in "application/" ++ subtype ++ suffix

  esc :: String -> String
  esc s = "\"" ++ Rx.replace (grx "\"") "\\\"" s ++ "\""
    where
    grx :: String -> Rx.Regex
    grx pat = Rx.regex pat Rx.noFlags { global = true }
