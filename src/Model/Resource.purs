module Model.Resource (
  Resource(..),
  isNotebook, isFile, isDatabase, isDirectory,
  resourceTag, getPath, getNameStr, getDir,
  setDir, renameAny, resourceName, resourceDir,
  nameOfFileOrDir, root, resourcePath,
  newFile, newDatabase, newDirectory, newNotebook,
  mkFile, mkDatabase, mkDirectory, mkNotebook,
  setPath, setName, _path, _root, _name, _notebookPath, _filePath,
  sortResource, parent, resourceFileName, child, _tempFile
  ) where

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
import Data.Inject1
import Data.Maybe
import Data.Path.Pathy
import Data.Tuple
import Model.Sort (Sort(..))
import Model.Path
import Optic.Core (lens, (..), (^.), (%~), (.~))
import Optic.Prism
import Optic.Refractor.Prism (_Right)
import Optic.Types
import Utils

import qualified Data.String as S

data Resource
  = File FilePath
  | Notebook DirPath
  | Directory DirPath
  | Database DirPath

_File :: PrismP Resource FilePath
_File = prism' File $ \s -> case s of
  File p -> Just p
  _ -> Nothing

_Notebook :: PrismP Resource DirPath
_Notebook = prism' Notebook $ \s -> case s of
  Notebook p -> Just p
  _ -> Nothing
  
_Directory :: PrismP Resource DirPath
_Directory = prism' Directory $ \s -> case s of
  Directory p -> Just p
  _ -> Nothing

_Database :: PrismP Resource DirPath
_Database = prism' Database $ \s -> case s of
  Database p -> Just p
  _ -> Nothing

_tempFile :: LensP Resource Resource
_tempFile = lens id \r s -> case s of 
  File p ->
    if (takeDirExt <$> (dirName (resourceDir s))) == Just notebookExtension
    then s
    else r 
  _ -> r


  
_notebookPath :: PrismP Resource DirPath
_notebookPath = prism' Notebook $ \s -> case s of
  Notebook fp -> Just fp
  _ -> Nothing

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
  Database _ -> "mount"
  Notebook _ -> "notebook"
  Directory _ -> "directory"

getPath :: Resource -> AnyPath
getPath r = case r of
  File p -> inj p
  Notebook p -> inj p
  Directory p -> inj p
  Database p -> inj p




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

resourceFileName :: Resource -> String
resourceFileName r =
  if isNotebook r
  then resourceFileName' $ getPath r
  else resourceName r
  where
  resourceFileName' ap = maybe "" (snd >>> dropExt >>> nameOfFileOrDir) $
                         either peel peel ap
  dropExt = bimap dropDirExt dropExtension


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

child :: Resource -> String -> Resource
child res name =
  let p = fromRight (mkDirectory (res ^. _path) ^. _path)
  in mkFile $ Left $ p </> file name


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
  (==) (File p) (File p') = p == p'
  (==) (Notebook p) (Notebook p') = p == p'
  (==) (Directory p) (Directory p') = p == p'
  (==) (Database p) (Database p') = p == p'
  (==) _ _ = false
  (/=) a b = not $ a == b

instance resourceIsForeign :: IsForeign Resource where
  read f = do
    name <- readProp "name" f
    ty <- readProp "type" f
    template <- case ty of
      "mount" -> pure newDatabase
      "directory" -> pure $ if endsWith notebookExtension name
                            then newNotebook
                            else newDirectory
      "file" -> pure newFile
      _ -> Left $ TypeMismatch "resource" "string"
    pure $ setName template name

instance encodeJsonResource :: EncodeJson Resource where
  encodeJson res = "type" := resourceTag res
                ~> "path" := resourcePath res
                ~> jsonEmptyObject

instance decodeJsonResource :: DecodeJson Resource where
  decodeJson json = do
    obj <- decodeJson json
    resType <- obj .? "type"
    path <- obj .? "path"
    case resType of
      -- type inference bug prevents use of a generic `parsePath` which accepts `parseAbsFile` or `parseAbsDir` as an argument
      "file" -> maybe (Left $ "Invalid file path") (Right <<< File) $ (rootDir </>) <$> (sandbox rootDir =<< parseAbsFile path)
      "notebook" -> parseDirPath "notebook" Notebook path
      "directory" -> parseDirPath "directory" Directory path
      "mount" -> parseDirPath "mount" Database path
    where
    parseDirPath :: String -> (DirPath -> Resource) -> String -> Either String Resource
    parseDirPath ty ctor s = maybe (Left $ "Invalid " ++ ty ++ " path") (Right <<< ctor) $ (rootDir </>) <$> (sandbox rootDir =<< parseAbsDir s)

sortResource :: (Resource -> String) -> Sort -> Resource -> Resource -> Ordering
sortResource project direction a b =
  case direction of
    Asc -> compare (project a) (project b)
    Desc -> compare (project b) (project a)

instance resourceOrd :: Ord Resource where
  compare = sortResource resourcePath Asc
