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

module Model.Notebook.Cell.FileInput
  ( FileInput(..)
  , initialFileInput
  , _showFiles
  , _files
  , _file
  , fileFromString
  , portFromFile
  ) where

import Prelude
import Data.Argonaut.Combinators ((~>), (:=), (.?))
import Data.Argonaut.Core (Json(), jsonEmptyObject)
import Data.Argonaut.Decode (DecodeJson, decodeJson)
import Data.Argonaut.Encode (EncodeJson)
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..))
import Data.Path.Pathy ((</>), parseAbsFile, sandbox, rootDir)
import Model.Resource (Resource(), resourcePath, newFile, _path)
import Model.Notebook.Port (Port(PortInvalid, PortResource))
import Optic.Core

newtype FileInput =
  FileInput { showFiles :: Boolean
            , files :: Array Resource
            , file :: Either String Resource
            }

initialFileInput :: FileInput
initialFileInput =
  FileInput { showFiles: false
            , files: []
            , file: Left ""
            }

_FileInput :: LensP FileInput _
_FileInput = lens (\(FileInput obj) -> obj) (const FileInput)

_showFiles :: LensP FileInput Boolean
_showFiles = _FileInput <<< lens _.showFiles (_ { showFiles = _ })

_files :: LensP FileInput (Array Resource)
_files = _FileInput <<< lens _.files (_ { files = _ })

_file :: LensP FileInput (Either String Resource)
_file = _FileInput <<< lens _.file (_ { file = _ })

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
