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
  , isViewMount
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
  , newViewMount
  , newNotebook
  , parent
  , resourceDir
  , resourceName
  , resourcePath
  , resourceTag
  , resourceMountTypeTag
  , root
  , sortResource
  , fileResourceFromString
  ) where

import Config as Config
import Control.Bind ((=<<))
import Data.Argonaut.Combinators ((~>), (:=), (.?))
import Data.Argonaut.Core (jsonEmptyObject)
import Data.Argonaut.Decode (DecodeJson, decodeJson)
import Data.Argonaut.Encode (EncodeJson)
import Data.Bifunctor (bimap)
import Data.Either (Either(..), either)
import Data.Foreign as F
import Data.Foreign.Class as F
import Data.Foreign.NullOrUndefined as F
import Data.Maybe (Maybe(..), fromMaybe, maybe)

import Data.Path.Pathy ((</>))
import Data.Path.Pathy as P
import Utils.Path ((<./>))
import Utils.Path as PU

import Data.Tuple (Tuple(..), fst, snd)
import Model.Sort (Sort(..))
import Data.Lens (lens, prism', LensP(), PrismP(), (.~))
import Prelude

import qualified Data.String as S

data Resource
  = File PU.FilePath
  | ViewMount PU.FilePath
  | Notebook PU.DirPath
  | Directory PU.DirPath
  | Database PU.DirPath

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

isViewMount :: Resource -> Boolean
isViewMount (ViewMount _) = true
isViewMount _ = false

isHidden :: Resource -> Boolean
isHidden r =
  either isHidden' isHidden' (getPath r)
  where
  isHidden' :: forall a b s. P.Path a b s -> Boolean
  isHidden' p = fromMaybe false do
    Tuple p' name <- P.peel p
    if "." == S.take 1 (nameOfFileOrDir name)
      then pure true
      else pure $ isHidden' p'

hiddenTopLevel :: Resource -> Boolean
hiddenTopLevel r = "." == S.take 1 (resourceName r)

isTempFile :: Resource -> Boolean
isTempFile r =
  (PU.takeDirExt <$> P.dirName (resourceDir r)) == Just Config.notebookExtension

-- EMPTY
newNotebook :: Resource
newNotebook = Notebook $ P.rootDir </> P.dir Config.newNotebookName <./> Config.notebookExtension

newFile :: Resource
newFile = File $ P.rootDir </> P.file Config.newFileName

newDirectory :: Resource
newDirectory = Directory $ P.rootDir </> P.dir Config.newFolderName

newDatabase :: Resource
newDatabase = Database $ P.rootDir </> P.dir Config.newDatabaseName

newViewMount :: Resource
newViewMount = Database $ P.rootDir </> P.dir Config.newViewMountName

-- CONSTRUCTORS
root :: Resource
root = Directory P.rootDir

mkNotebook :: PU.AnyPath -> Resource
mkNotebook ap =
  either go (Notebook <<< (<./> Config.notebookExtension)) ap
  where
  go :: PU.FilePath -> Resource
  go p = maybe newNotebook id do
    Tuple pp dirOrFile <- P.peel p
    pure $ Notebook $
      (pp </> P.dir (nameOfFileOrDir dirOrFile) <./> Config.notebookExtension)

mkFile :: PU.AnyPath -> Resource
mkFile ap = either File go ap
  where
  go :: PU.DirPath -> Resource
  go p = maybe newFile id do
    Tuple pp dirOrFile <- P.peel p
    pure $ File (pp </> P.file (nameOfFileOrDir dirOrFile))

mkViewMount :: PU.AnyPath -> Resource
mkViewMount ap = either ViewMount go ap
  where
  go :: PU.DirPath -> Resource
  go p = maybe newViewMount id do
    Tuple pp dirOrFile <- P.peel p
    pure $ ViewMount (pp </> P.file (nameOfFileOrDir dirOrFile))

mkDirectory :: PU.AnyPath -> Resource
mkDirectory ap = either go Directory ap
  where
  go :: PU.FilePath -> Resource
  go p = maybe newDirectory id do
    Tuple pp dirOrFile <- P.peel p
    pure $ Directory (pp </> P.dir (nameOfFileOrDir dirOrFile))

mkDatabase :: PU.AnyPath -> Resource
mkDatabase ap = either go Database ap
  where
  go :: PU.FilePath -> Resource
  go p = maybe newDatabase id do
    Tuple pp dirOrFile <- P.peel p
    pure $ Database (pp </> P.dir (nameOfFileOrDir dirOrFile))

-- This is not real parent because it can't determine
-- is it a directory or mount
parent :: Resource -> Resource
parent = Directory <<< resourceDir

-- GETTERS
resourceTag :: Resource -> String
resourceTag r = case r of
  File _ -> "file"
  ViewMount _ -> "file"
  Database _ -> "directory"
  Notebook _ -> "notebook"
  Directory _ -> "directory"

resourceMountTypeTag :: Resource -> Maybe String
resourceMountTypeTag r = case r of
  ViewMount _ -> Just "view"
  Database _ -> Just "mongodb"
  _ -> Nothing

resourceName :: Resource -> String
resourceName = getPath >>> getNameStr

resourceDir :: Resource -> PU.DirPath
resourceDir = getPath >>> getDir

resourcePath :: Resource -> String
resourcePath r = either P.printPath P.printPath $ getPath r

getPath :: Resource -> PU.AnyPath
getPath r = case r of
  File p -> Left p
  ViewMount p -> Left p
  Notebook p -> Right p
  Directory p -> Right p
  Database p -> Right p

getNameStr :: PU.AnyPath -> String
getNameStr ap = either getNameStr' getNameStr' ap
  where
  getNameStr' :: forall b a s. P.Path a b s -> String
  getNameStr' p = maybe "" (snd >>> nameOfFileOrDir) $ P.peel p

getDir :: PU.AnyPath -> PU.DirPath
getDir ap = either getDir' getDir' ap
  where
  getDir' :: forall b. P.Path P.Abs b P.Sandboxed -> PU.DirPath
  getDir' = maybe P.rootDir fst <<< P.peel

-- SETTERS
setDir :: PU.AnyPath -> PU.DirPath -> PU.AnyPath
setDir ap d = bimap (setFile' d) (setDir' d) ap
  where
  setDir' :: PU.DirPath -> PU.DirPath -> PU.DirPath
  setDir' d p =
    d </>
    (maybe P.currentDir (snd >>> nameOfFileOrDir >>> P.dir) $ P.peel p)

  setFile' :: PU.DirPath -> PU.FilePath -> PU.FilePath
  setFile' d p =
    d </>
    (maybe (P.file "") (snd >>> nameOfFileOrDir >>> P.file) $ P.peel p)

setPath :: Resource -> PU.AnyPath -> Resource
setPath (Notebook _) p = mkNotebook p
setPath (File _) p = mkFile p
setPath (Database _) p = mkDatabase p
setPath (Directory _) p = mkDirectory p
setPath (ViewMount _) p = mkViewMount p

setName :: Resource -> String -> Resource
setName r name =
  setPath r ((renameAny (const name) (getPath r)))

-- MODIFIERS (PRIVATE)

renameAny :: (String -> String) -> PU.AnyPath -> PU.AnyPath
renameAny fn ap = bimap (P.renameFile $ liftFile fn) (P.renameDir $ liftDir fn) ap
  where
  liftFile fn (P.FileName a) = P.FileName $ fn a
  liftDir fn (P.DirName a) = P.DirName $ fn a


nameOfFileOrDir :: Either P.DirName P.FileName -> String
nameOfFileOrDir (Left (P.DirName name)) = name
nameOfFileOrDir (Right (P.FileName name)) = name

-- TRAVERSALS
_tempFile :: LensP Resource Resource
_tempFile = lens id \r s -> case r of
  File p ->
    if isTempFile r
    then s
    else r
  _ -> r

_filePath :: PrismP Resource PU.FilePath
_filePath = prism' File $ \s -> case s of
  File fp -> Just fp
  _ -> Nothing

_path :: LensP Resource PU.AnyPath
_path = lens getPath setPath

_nameAnyPath :: LensP PU.AnyPath String
_nameAnyPath = lens getNameStr (\p x -> renameAny (const x) p)

_name :: LensP Resource String
_name = _path <<< _nameAnyPath

_rootAnyPath :: LensP PU.AnyPath PU.DirPath
_rootAnyPath = lens getDir setDir

_root :: LensP Resource PU.DirPath
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
  eq (ViewMount p) (ViewMount p') = p == p'
  eq _ _ = false


instance resourceOrd :: Ord Resource where
  compare = sortResource resourcePath Asc

instance resourceIsForeign :: F.IsForeign Resource where
  read f = do
    name <- F.readProp "name" f
    ty <- F.readProp "type" f
    mountType <- F.readProp "mount" f <#> F.runNullOrUndefined
    template <-
      case ty of
        "directory" ->
          case mountType of
            Just "mongodb" -> pure newDatabase
            _ -> pure $ maybe newDirectory (const newNotebook) $ S.stripSuffix Config.notebookExtension name
        "file" ->
          case mountType of
            Just "view" -> pure newViewMount
            _ -> pure newFile
        _ -> Left $ F.TypeMismatch "resource" "string"
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
      "file" ->
        let
          constr =
            case mountType of
              Just "view" -> ViewMount
              _ -> File
        in
          parsePath "file" constr P.parseAbsFile path
      "notebook" -> parsePath "notebook" Notebook P.parseAbsDir path
      "directory" ->
        case mountType of
          Just "mongodb" -> parsePath "mount" Database P.parseAbsDir path
          _ -> parsePath "directory" Directory P.parseAbsDir path
      _ -> Left "Unrecognized resource type"
    where
    parsePath
      :: forall a
       . String
      -> (P.Path P.Abs a P.Sandboxed -> Resource)
      -> (String -> Maybe (P.Path P.Abs a P.Unsandboxed))
      -> String
      -> Either String Resource
    parsePath ty ctor parse s =
      maybe
        (Left $ "Invalid " ++ ty ++ " path")
        (Right <<< ctor) $
          (P.rootDir </>) <$> (P.sandbox P.rootDir =<< parse s)

fileResourceFromString
  :: String
  -> Either String Resource
fileResourceFromString path =
  case (P.rootDir </>) <$> (P.parseAbsFile path >>= P.sandbox P.rootDir) of
    Just path' -> Right $ newFile # _path .~ Left path'
    Nothing -> Left path
