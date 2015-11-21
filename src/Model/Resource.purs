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

module Model.Resource
  ( Resource(..)
  , _filePath
  , _name
  , _nameAnyPath
  , _path
  , _root
  , _tempFile
  , getPath
  , hiddenTopLevel
  , isDatabase
  , isDirectory
  , isFile
  , isHidden
  , isNotebook
  , isTempFile
  , mkDatabase
  , mkDirectory
  , mkFile
  , mkNotebook
  , newDatabase
  , newDirectory
  , newFile
  , newNotebook
  , parent
  , resourceDir
  , resourceName
  , resourcePath
  , resourceTag
  , root
  , sortResource
  ) where

import Prelude
import Config
import Control.Bind ((=<<))
import Data.Argonaut.Combinators ((~>), (:=), (.?))
import Data.Argonaut.Core (jsonEmptyObject)
import Data.Argonaut.Decode (DecodeJson, decodeJson)
import Data.Argonaut.Encode (EncodeJson)
import Data.Bifunctor (bimap, rmap)
import Data.Either
import Data.Either.Unsafe (fromRight)
import Data.Foreign (ForeignError(TypeMismatch))
import Data.Foreign.Class (readProp, read, IsForeign)
import Data.Foreign.NullOrUndefined
import Data.Inject1
import Data.Maybe
import Data.Path.Pathy
import Data.Tuple
import Model.File.Sort (Sort(..))
import Model.Path
import Optic.Core
import Optic.Refractor.Prism (_Right)
import Optic.Types
import Utils

import qualified Data.String as S

-- TODO: should we have a constructor for views?
data Resource
  = File FilePath
  | Notebook DirPath
  | Directory DirPath
  | Database DirPath

instance showResource :: Show Resource where
  show r =
    case r of
      File fp -> "File " ++ show fp
      Notebook dp -> "Notebook " ++ show dp
      Directory dp -> "Directory " ++ show dp
      Database dp -> "Database " ++ show dp

_tempFile :: LensP Resource Resource
_tempFile = lens id \r s -> case r of
  File p ->
    if isTempFile r
    then s
    else r
  _ -> r

isTempFile :: Resource -> Boolean
isTempFile r =
  (takeDirExt <$> (dirName (resourceDir r))) == Just notebookExtension

_filePath :: PrismP Resource FilePath
_filePath = prism' File $ \s -> case s of
  File fp -> Just fp
  _ -> Nothing

isNotebook :: Resource -> Boolean
isNotebook (Notebook _) = true
isNotebook _ = false

isFile :: Resource -> Boolean
isFile (File _) = true
isFile _ = false

isDirectory :: Resource -> Boolean
isDirectory (Directory _) = true
isDirectory _ = false

isDatabase :: Resource -> Boolean
isDatabase (Database _) = true
isDatabase _ = false

resourceTag :: Resource -> String
resourceTag r = case r of
  File _ -> "file"
  Database _ -> "directory"
  Notebook _ -> "notebook"
  Directory _ -> "directory"

-- | TODO: we don't currently know when a file is really a view; should we?
resourceMountTypeTag :: Resource -> Maybe String
resourceMountTypeTag r = case r of
  Database _ -> Just "mongodb"
  _ -> Nothing

getPath :: Resource -> AnyPath
getPath r = case r of
  File p -> inj p
  Notebook p -> inj p
  Directory p -> inj p
  Database p -> inj p

isHidden :: Resource -> Boolean
isHidden r =
  either isHidden' isHidden' (getPath r)
  where
  isHidden' :: forall a b s. Path a b s -> Boolean
  isHidden' p = fromMaybe false do
    Tuple p' name <- peel p
    if "." == S.take 1 (nameOfFileOrDir name)
      then pure true
      else pure $ isHidden' p'

hiddenTopLevel :: Resource -> Boolean
hiddenTopLevel r = "." == S.take 1 (resourceName r)


getNameStr :: AnyPath -> String
getNameStr ap = either getNameStr' getNameStr' ap
  where
  getNameStr' :: forall b a s. Path a b s -> String
  getNameStr' p = maybe "" (snd >>> nameOfFileOrDir) $ peel p

getDir :: AnyPath -> DirPath
getDir ap = either getDir' getDir' ap
  where
  getDir' :: forall b. Path Abs b Sandboxed -> DirPath
  getDir' p = maybe rootDir fst $ peel p

setDir :: AnyPath -> DirPath -> AnyPath
setDir ap d = bimap (setFile' d) (setDir' d) ap
  where
  setDir' :: DirPath -> DirPath -> DirPath
  setDir' d p =
    d </>
    (maybe currentDir (snd >>> nameOfFileOrDir >>> dir) $ peel p)

  setFile' :: DirPath -> FilePath -> FilePath
  setFile' d p =
    d </>
    (maybe (file "") (snd >>> nameOfFileOrDir >>> file) $ peel p)

renameAny :: (String -> String) -> AnyPath -> AnyPath
renameAny fn ap = bimap (renameFile $ liftFile fn) (renameDir $ liftDir fn) ap
  where
  liftFile fn (FileName a) = FileName $ fn a
  liftDir fn (DirName a) = DirName $ fn a

resourceName :: Resource -> String
resourceName = getPath >>> getNameStr

resourceDir :: Resource -> DirPath
resourceDir = getPath >>> getDir

nameOfFileOrDir :: Either DirName FileName -> String
nameOfFileOrDir (Left (DirName name)) = name
nameOfFileOrDir (Right (FileName name)) = name

root :: Resource
root = Directory rootDir

-- This is not real parent because it can't determine
-- is it a directory or mount
parent :: Resource -> Resource
parent = Directory <<< resourceDir

resourcePath :: Resource -> String
resourcePath r = either printPath printPath $ getPath r

newNotebook :: Resource
newNotebook = Notebook $ rootDir </> dir newNotebookName <./> notebookExtension

newFile :: Resource
newFile = File $ rootDir </> file newFileName

newDirectory :: Resource
newDirectory = Directory $ rootDir </> dir newFolderName

newDatabase :: Resource
newDatabase = Database $ rootDir </> dir newDatabaseName

mkNotebook :: AnyPath -> Resource
mkNotebook ap =
  either go (Notebook <<< (<./> notebookExtension)) ap
  where
  go :: FilePath -> Resource
  go p = maybe newNotebook id do
    Tuple pp dirOrFile <- peel p
    pure $ Notebook $
      (pp </> dir (nameOfFileOrDir dirOrFile) <./> notebookExtension)

mkFile :: AnyPath -> Resource
mkFile ap = either File go ap
  where
  go :: DirPath -> Resource
  go p = maybe newFile id do
    Tuple pp dirOrFile <- peel p
    pure $ File (pp </> file (nameOfFileOrDir dirOrFile))

mkDirectory :: AnyPath -> Resource
mkDirectory ap = either go Directory ap
  where
  go :: FilePath -> Resource
  go p = maybe newDirectory id do
    Tuple pp dirOrFile <- peel p
    pure $ Directory (pp </> dir (nameOfFileOrDir dirOrFile))

mkDatabase :: AnyPath -> Resource
mkDatabase ap = either go Database ap
  where
  go :: FilePath -> Resource
  go p = maybe newDatabase id do
    Tuple pp dirOrFile <- peel p
    pure $ Database (pp </> dir (nameOfFileOrDir dirOrFile))

setPath :: Resource -> AnyPath -> Resource
setPath (Notebook _) p = mkNotebook p
setPath (File _) p = mkFile p
setPath (Database _) p = mkDatabase p
setPath (Directory _) p = mkDirectory p

setName :: Resource -> String -> Resource
setName r name =
  setPath r ((renameAny (const name) (getPath r)))

_path :: LensP Resource AnyPath
_path = lens getPath setPath

_nameAnyPath :: LensP AnyPath String
_nameAnyPath = lens getNameStr (\p x -> renameAny (const x) p)

_name :: LensP Resource String
_name = _path ..  _nameAnyPath

_rootAnyPath :: LensP AnyPath DirPath
_rootAnyPath = lens getDir setDir

_root :: LensP Resource DirPath
_root = _path .. _rootAnyPath

instance resourceEq :: Eq Resource where
  eq (File p) (File p') = p == p'
  eq (Notebook p) (Notebook p') = p == p'
  eq (Directory p) (Directory p') = p == p'
  eq (Database p) (Database p') = p == p'
  eq _ _ = false

instance resourceIsForeign :: IsForeign Resource where
  read f = do
    name <- readProp "name" f
    ty <- readProp "type" f
    mountType <- runNullOrUndefined <$> readProp "mount" f
    template <- case ty of
      "directory" ->
        case mountType of
          Just "mongodb" -> pure newDatabase
          _ ->
            pure $
              if endsWith notebookExtension name
                then newNotebook
                else newDirectory
      "file" -> pure newFile
      _ -> Left $ TypeMismatch "resource" "string"
    pure $ setName template name

instance encodeJsonResource :: EncodeJson Resource where
  encodeJson res =
    "type" := resourceTag res
    ~> "path" := resourcePath res
    ~> maybe
        jsonEmptyObject
        (\t -> "mount" := t ~> jsonEmptyObject)
        (resourceMountTypeTag res)

instance decodeJsonResource :: DecodeJson Resource where
  decodeJson json = do
    obj <- decodeJson json
    resType <- obj .? "type"
    path <- obj .? "path"
    let mountType = Data.StrMap.lookup "mount" obj >>= decodeJson >>> either (const Nothing) pure
    case resType of
      -- type inference bug prevents use of a generic `parsePath` which accepts `parseAbsFile` or `parseAbsDir` as an argument
      "file" -> maybe (Left $ "Invalid file path") (Right <<< File) $ (rootDir </>) <$> (sandbox rootDir =<< parseAbsFile path)
      "notebook" -> parseDirPath "notebook" Notebook path
      "directory" ->
        case mountType of
          Just "mongodb" -> parseDirPath "mount" Database path
          _ -> parseDirPath "directory" Directory path
      _ -> Left "Unrecognized resource type"
    where
    parseDirPath :: String -> (DirPath -> Resource) -> String -> Either String Resource
    parseDirPath ty ctor s = maybe (Left $ "Invalid " ++ ty ++ " path") (Right <<< ctor) $ (rootDir </>) <$> (sandbox rootDir =<< parseAbsDir s)

sortResource :: (Resource -> String) -> Sort -> Resource -> Resource -> Ordering
sortResource project direction a b =
  if (isHidden a) && (not $ isHidden b) then GT else
    if (isHidden b) && (not $ isHidden a) then LT else
      case direction of
        Asc -> compare (project a) (project b)
        Desc -> compare (project b) (project a)

instance resourceOrd :: Ord Resource where
  compare = sortResource resourcePath Asc
