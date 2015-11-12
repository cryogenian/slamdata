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
  , fileResourceFromString
  ) where

import Config
  (newFileName,
   newDatabaseName,
   newNotebookName,
   newFolderName,
   notebookExtension)
import Control.Bind ((=<<))
import Data.Argonaut.Combinators ((~>), (:=), (.?))
import Data.Argonaut.Core (jsonEmptyObject)
import Data.Argonaut.Decode (DecodeJson, decodeJson)
import Data.Argonaut.Encode (EncodeJson)
import Data.Bifunctor (bimap, rmap)
import Data.Either (Either(..), either)
import Data.Foreign (ForeignError(TypeMismatch))
import Data.Foreign.Class (readProp, read, IsForeign)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Path.Pathy
  (Path(),
   DirName(..),
   FileName(..),
   Abs(),
   Sandboxed(),
   dirName,
   fileName,
   peel,
   (</>),
   rootDir,
   printPath,
   rootDir,
   currentDir,
   file,
   dir,
   sandbox,
   parseAbsFile,
   parseAbsDir,
   renameFile,
   renameDir)
import Data.Tuple (Tuple(..), fst, snd)
import Model.Sort (Sort(..))
import Data.Lens (lens, prism', LensP(), PrismP(), (.~))
import Prelude
import Utils.Path (DirPath(), FilePath(), AnyPath(), (<./>), takeDirExt)

import qualified Data.String as S

data Resource
  = File FilePath
  | Notebook DirPath
  | Directory DirPath
  | Database DirPath

-- PREDICATES
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

isTempFile :: Resource -> Boolean
isTempFile r =
  (takeDirExt <$> (dirName (resourceDir r))) == Just notebookExtension

-- EMPTY
newNotebook :: Resource
newNotebook = Notebook $ rootDir </> dir newNotebookName <./> notebookExtension

newFile :: Resource
newFile = File $ rootDir </> file newFileName

newDirectory :: Resource
newDirectory = Directory $ rootDir </> dir newFolderName

newDatabase :: Resource
newDatabase = Database $ rootDir </> dir newDatabaseName

-- CONSTRUCTORS
root :: Resource
root = Directory rootDir

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

-- This is not real parent because it can't determine
-- is it a directory or mount
parent :: Resource -> Resource
parent = Directory <<< resourceDir

-- GETTERS
resourceTag :: Resource -> String
resourceTag r = case r of
  File _ -> "file"
  Database _ -> "mount"
  Notebook _ -> "notebook"
  Directory _ -> "directory"

resourceName :: Resource -> String
resourceName = getPath >>> getNameStr

resourceDir :: Resource -> DirPath
resourceDir = getPath >>> getDir

resourcePath :: Resource -> String
resourcePath r = either printPath printPath $ getPath r

getPath :: Resource -> AnyPath
getPath r = case r of
  File p -> Left p
  Notebook p -> Right p
  Directory p -> Right p
  Database p -> Right p

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

-- SETTERS
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

setPath :: Resource -> AnyPath -> Resource
setPath (Notebook _) p = mkNotebook p
setPath (File _) p = mkFile p
setPath (Database _) p = mkDatabase p
setPath (Directory _) p = mkDirectory p

setName :: Resource -> String -> Resource
setName r name =
  setPath r ((renameAny (const name) (getPath r)))

-- MODIFIERS (PRIVATE)

renameAny :: (String -> String) -> AnyPath -> AnyPath
renameAny fn ap = bimap (renameFile $ liftFile fn) (renameDir $ liftDir fn) ap
  where
  liftFile fn (FileName a) = FileName $ fn a
  liftDir fn (DirName a) = DirName $ fn a


nameOfFileOrDir :: Either DirName FileName -> String
nameOfFileOrDir (Left (DirName name)) = name
nameOfFileOrDir (Right (FileName name)) = name

-- TRAVERSALS
_tempFile :: LensP Resource Resource
_tempFile = lens id \r s -> case r of
  File p ->
    if isTempFile r
    then s
    else r
  _ -> r

_filePath :: PrismP Resource FilePath
_filePath = prism' File $ \s -> case s of
  File fp -> Just fp
  _ -> Nothing

_path :: LensP Resource AnyPath
_path = lens getPath setPath

_nameAnyPath :: LensP AnyPath String
_nameAnyPath = lens getNameStr (\p x -> renameAny (const x) p)

_name :: LensP Resource String
_name = _path <<< _nameAnyPath

_rootAnyPath :: LensP AnyPath DirPath
_rootAnyPath = lens getDir setDir

_root :: LensP Resource DirPath
_root = _path <<< _rootAnyPath


-- INSTANCES
sortResource :: (Resource -> String) -> Sort -> Resource -> Resource -> Ordering
sortResource project direction a b
  | (isHidden a) && (not $ isHidden b) = GT
  | (isHidden b) && (not $ isHidden a) = LT
  | otherwise = case direction of
    Asc -> compare (project a) (project b)
    Desc -> compare (project b) (project a)

instance eqResource :: Eq Resource where
  eq (File p) (File p') = p == p'
  eq (Notebook p) (Notebook p') = p == p'
  eq (Directory p) (Directory p') = p == p'
  eq (Database p) (Database p') = p == p'
  eq _ _ = false


instance resourceOrd :: Ord Resource where
  compare = sortResource resourcePath Asc

instance resourceIsForeign :: IsForeign Resource where
  read f = do
    name <- readProp "name" f
    ty <- readProp "type" f
    template <- case ty of
      "mount" -> pure newDatabase
      "directory" -> pure
                     $ maybe newDirectory (const newNotebook)
                     $ S.stripSuffix notebookExtension name
      "file" -> pure newFile
      _ -> Left $ TypeMismatch "resource" "string"
    pure $ setName template name

instance encodeJsonResource :: EncodeJson Resource where
  encodeJson res =
       "type" := resourceTag res
    ~> "path" := resourcePath res
    ~> jsonEmptyObject

instance decodeJsonResource :: DecodeJson Resource where
  decodeJson json = do
    obj <- decodeJson json
    resType <- obj .? "type"
    path <- obj .? "path"
    case resType of
      -- type inference bug prevents use of a generic `parsePath` which accepts
      -- `parseAbsFile` or `parseAbsDir` as an argument
      "file" -> maybe (Left $ "Invalid file path") (Right <<< File)
                $ (rootDir </>) <$> (sandbox rootDir =<< parseAbsFile path)
      "notebook" -> parseDirPath "notebook" Notebook path
      "directory" -> parseDirPath "directory" Directory path
      "mount" -> parseDirPath "mount" Database path
      _ -> Left "Unrecognized resource type"
    where
    parseDirPath :: String -> (DirPath -> Resource) -> String
                    -> Either String Resource
    parseDirPath ty ctor s =
      maybe (Left $ "Invalid " ++ ty ++ " path") (Right <<< ctor) $
        (rootDir </>) <$> (sandbox rootDir =<< parseAbsDir s)

fileResourceFromString
  :: String
  -> Either String Resource
fileResourceFromString path =
  case (rootDir </>) <$> (parseAbsFile path >>= sandbox rootDir) of
    Just path' -> Right $ newFile # _path .~ Left path'
    Nothing -> Left path
