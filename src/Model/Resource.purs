module Model.Resource (
  FilePath(), DirPath(), AnyPath(), Resource(),
  isNotebook, isFile, isDatabase, isDirectory,
  resourceTag, getPath, getNameStr, getDir,
  setDir, renameAny, resourceName, resourceDir,
  nameOfFileOrDir, root, resourcePath,
  newFile, newDatabase, newDirectory, newNotebook,
  mkFile, mkDatabase, mkDirectory, mkNotebook,
  setPath, setName, nameAnyPath, rootAnyPath,
  pathL, rootL, nameL, sortResource, parent,
  resourceFileName, notebookPath
  ) where

import Data.Tuple
import Data.Maybe
import Data.Either
import Data.Inject1
import Data.Path.Pathy
import Data.Foreign.Class (readProp, read, IsForeign)
import Data.Foreign (ForeignError(TypeMismatch))
import Data.Bifunctor (bimap, rmap)
import Optic.Core (lens)
import Optic.Prism
import Optic.Types
import Model.Sort (Sort(..))
import qualified Data.String as S
import Utils
import Config

type FilePath = AbsFile Sandboxed
type DirPath = AbsDir Sandboxed
type AnyPath = Either FilePath DirPath

data Resource
  = File FilePath
  | Notebook DirPath
  | Directory DirPath
  | Database DirPath

infixl 6 <./>
(<./>) :: forall a s. Path a Dir s -> String -> Path a Dir s
(<./>) p ext = renameDir (changeDirExt $ const ext) p

changeDirExt :: (String -> String) -> DirName -> DirName
changeDirExt f (DirName name) =
  DirName ((if ext == ""
            then name
            else n) <> "." <> f ext)
  where
  idx = S.lastIndexOf "." name
  n = case idx of
    -1 -> name
    _ -> S.take idx name
  ext = case idx of
    -1 -> ""
    _ -> S.drop (idx + 1) name

dropDirExt :: DirName -> DirName
dropDirExt (DirName d) =
  DirName $ case idx of
    -1 -> d
    _ -> S.take idx d
  where
  idx = S.lastIndexOf "." d

notebookPath :: PrismP Resource DirPath
notebookPath = prism' Notebook $ \s -> case s of
  Notebook fp -> Just fp
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

setPath :: Resource -> AnyPath -> Resource
setPath (Notebook _) p = mkNotebook p
setPath (File _) p = mkFile p
setPath (Database _) p = mkDatabase p
setPath (Directory _) p = mkDirectory p

setName :: Resource -> String -> Resource
setName r name =
  setPath r ((renameAny (const name) (getPath r)))

pathL :: LensP Resource AnyPath
pathL = lens getPath setPath

nameAnyPath :: LensP AnyPath String
nameAnyPath = lens getNameStr (\p x -> renameAny (const x) p)

nameL :: LensP Resource String
nameL = pathL <<< nameAnyPath

rootAnyPath :: LensP AnyPath DirPath
rootAnyPath = lens getDir setDir

rootL :: LensP Resource DirPath
rootL = pathL <<< rootAnyPath

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
--                pure $ if endsWith notebookExtension name
--                       then newNotebook
--                       else newFile
      _ -> Left $ TypeMismatch "resource" "string"
    pure $ setName template name


sortResource :: (Resource -> String) -> Sort -> Resource -> Resource -> Ordering
sortResource project direction a b =
  case direction of
    Asc -> compare (project a) (project b)
    Desc -> compare (project b) (project a)

instance resourceOrd :: Ord Resource where
  compare = sortResource resourcePath Asc
