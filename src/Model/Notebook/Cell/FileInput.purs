module Model.Notebook.Cell.FileInput where

import Data.Argonaut.Combinators ((~>), (:=), (.?))
import Data.Argonaut.Core (Json(), jsonEmptyObject)
import Data.Argonaut.Decode (DecodeJson, decodeJson)
import Data.Argonaut.Encode (EncodeJson)
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..))
import Data.Path.Pathy ((</>), parseAbsFile, sandbox, rootDir)
import Model.Resource (Resource(), resourcePath, newFile, _path)
import Model.Notebook.Port (Port(PortInvalid, PortResource))
import Optic.Core (LensP(), lens, (.~))

newtype FileInput =
  FileInput { showFiles :: Boolean
            , files :: [Resource]
            , file :: Either String Resource
            }

initialFileInput :: FileInput
initialFileInput =
  FileInput { showFiles: false
            , files: []
            , file: Left ""
            }

_fileInput :: LensP FileInput _
_fileInput = lens (\(FileInput obj) -> obj) (const FileInput)

_showFiles :: LensP FileInput Boolean
_showFiles = _fileInput <<< lens _.showFiles (_ { showFiles = _ })

_files :: LensP FileInput [Resource]
_files = _fileInput <<< lens _.files (_ { files = _ })

_file :: LensP FileInput (Either String Resource)
_file = _fileInput <<< lens _.file (_ { file = _ })

fileFromString :: String -> Either String Resource
fileFromString path =
  case (rootDir </>) <$> (parseAbsFile path >>= sandbox rootDir) of
    Just path' -> Right (newFile # _path .~ Left path')
    Nothing -> Left path

portFromFile :: Either String Resource -> Port
portFromFile = either (\_ -> PortInvalid "Please enter a valid file path") PortResource

instance encodeJsonFileInput :: EncodeJson FileInput where
  encodeJson (FileInput rec)
    =  "file" := either id resourcePath rec.file
    ~> jsonEmptyObject

instance decodeJsonFileInput :: DecodeJson FileInput where
  decodeJson json = do
    obj <- decodeJson json
    rec <- { showFiles: false, files: [], file: _ }
        <$> (fileFromString <$> obj .? "file")
    return $ FileInput rec
