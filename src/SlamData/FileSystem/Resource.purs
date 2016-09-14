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

module SlamData.FileSystem.Resource
  ( Resource(..)
  , Mount(..)
  , _filePath
  , _dirPath
  , _Workspace
  , _name
  , _nameAnyPath
  , _path
  , _root
  , _tempFile
  , getPath
  , hiddenTopLevel
  , isDirectory
  , isFile
  , isMount
  , isDatabaseMount
  , isViewMount
  , isHidden
  , isWorkspace
  , isTempFile
  , canHaveChildren
  , mkDatabase
  , mkDirectory
  , mkFile
  , mkWorkspace
  , mkViewMount
  , newDatabase
  , newDirectory
  , newFile
  , newViewMount
  , newWorkspace
  , parent
  , resourceDir
  , resourceName
  , resourcePath
  , resourceTag
  , resourceMount
  , mountTypeTag
  , root
  , sortResource
  ) where

import SlamData.Prelude

import Data.Argonaut (class EncodeJson, class DecodeJson, decodeJson, jsonEmptyObject, (~>), (:=), (.?))
import Data.Foreign (ForeignError(..)) as F
import Data.Foreign.Class (class IsForeign, readProp) as F
import Data.Foreign.NullOrUndefined (unNullOrUndefined) as F
import Data.Lens (lens, prism', PrismP, LensP, TraversalP, wander)
import Data.Path.Pathy ((</>))
import Data.Path.Pathy as P
import Data.String as S
import Data.StrMap as SM

import SlamData.Config as Config
import SlamData.Common.Sort (Sort(..))

import Test.Property.Utils.Path as TP
import Test.StrongCheck.Arbitrary as SC
import Test.StrongCheck.Gen as Gen

import Utils.Path ((<./>))
import Utils.Path as PU

data Resource
  = File PU.FilePath
  | Workspace PU.DirPath
  | Directory PU.DirPath
  | Mount Mount

data Mount
  = Database PU.DirPath
  | View PU.FilePath

instance arbitaryMount ∷ SC.Arbitrary Mount where
  arbitrary = do
    b ← SC.arbitrary
    if b
      then Database ∘ TP.runArbDirPath <$> SC.arbitrary
      else View ∘ TP.runArbFilePath <$> SC.arbitrary

instance arbitraryResource ∷ SC.Arbitrary Resource where
  arbitrary = do
    Gen.oneOf (Mount <$> SC.arbitrary)
      [ File ∘ TP.runArbFilePath <$> SC.arbitrary
      , Workspace ∘ TP.runArbDirPath <$> SC.arbitrary
      , Directory ∘ TP.runArbDirPath <$> SC.arbitrary
      , Mount <$> SC.arbitrary
      ]

instance showResource ∷ Show Resource where
  show =
    case _ of
      File p → "File " <> show p
      Workspace p → "Workspace " <> show p
      Directory p → "Directory " <> show p
      Mount m → "Mount " <> show m

instance showMount ∷ Show Mount where
  show =
    case _ of
      Database p → "Database " <> show p
      View p → "View " <> show p

-- PREDICATES
isWorkspace ∷ Resource → Boolean
isWorkspace (Workspace _) = true
isWorkspace _ = false

isFile ∷ Resource → Boolean
isFile (File _) = true
isFile _ = false

isDirectory ∷ Resource → Boolean
isDirectory (Directory _) = true
isDirectory _ = false

isMount ∷ Resource → Boolean
isMount (Mount _) = true
isMount _ = false

isDatabaseMount ∷ Resource → Boolean
isDatabaseMount (Mount (Database _)) = true
isDatabaseMount _ = false

isViewMount ∷ Resource → Boolean
isViewMount (Mount (View _)) = true
isViewMount _ = false

canHaveChildren ∷ Resource → Boolean
canHaveChildren (Mount (Database _)) = true
canHaveChildren (Directory _) = true
canHaveChildren _ = false

isHidden ∷ Resource → Boolean
isHidden r =
  either isHidden' isHidden' (getPath r)
  where
  isHidden' ∷ forall a b s. P.Path a b s → Boolean
  isHidden' p = fromMaybe false do
    Tuple p' name ← P.peel p
    if "." ≡ S.take 1 (PU.nameOfFileOrDir name)
      then pure true
      else pure $ isHidden' p'

hiddenTopLevel ∷ Resource → Boolean
hiddenTopLevel r = "." ≡ S.take 1 (resourceName r)

isTempFile ∷ Resource → Boolean
isTempFile r =
  (PU.takeDirExt <$> P.dirName (resourceDir r)) ≡ Just Config.workspaceExtension

-- EMPTY
newWorkspace ∷ Resource
newWorkspace = Workspace $ P.rootDir </> P.dir Config.newWorkspaceName <./> Config.workspaceExtension

newFile ∷ Resource
newFile = File $ P.rootDir </> P.file Config.newFileName

newDirectory ∷ Resource
newDirectory = Directory $ P.rootDir </> P.dir Config.newFolderName

newDatabase ∷ Resource
newDatabase = Mount $ Database $ P.rootDir </> P.dir Config.newDatabaseName

newViewMount ∷ Resource
newViewMount = Mount $ View $ P.rootDir </> P.file Config.newViewMountName

-- CONSTRUCTORS
root ∷ Resource
root = Directory P.rootDir

mkWorkspace ∷ PU.AnyPath → Resource
mkWorkspace ap =
  either (Workspace ∘ (_ <./> Config.workspaceExtension)) go ap
  where
  go ∷ PU.FilePath → Resource
  go p = maybe newWorkspace id do
    Tuple pp dirOrFile ← P.peel p
    pure $ Workspace $
      (pp </> P.dir (PU.nameOfFileOrDir dirOrFile) <./> Config.workspaceExtension)

mkFile ∷ PU.AnyPath → Resource
mkFile ap = either go File ap
  where
  go ∷ PU.DirPath → Resource
  go p = maybe newFile id do
    Tuple pp dirOrFile ← P.peel p
    pure $ File (pp </> P.file (PU.nameOfFileOrDir dirOrFile))

mkViewMount ∷ PU.AnyPath → Resource
mkViewMount ap = either go (Mount ∘ View) ap
  where
  go ∷ PU.DirPath → Resource
  go p = maybe newViewMount id do
    Tuple pp dirOrFile ← P.peel p
    pure $ Mount $ View (pp </> P.file (PU.nameOfFileOrDir dirOrFile))

mkDirectory ∷ PU.AnyPath → Resource
mkDirectory ap = either Directory go ap
  where
  go ∷ PU.FilePath → Resource
  go p = maybe newDirectory id do
    Tuple pp dirOrFile ← P.peel p
    pure $ Directory (pp </> P.dir (PU.nameOfFileOrDir dirOrFile))

mkDatabase ∷ PU.AnyPath → Resource
mkDatabase ap = either (Mount ∘ Database) go ap
  where
  go ∷ PU.FilePath → Resource
  go p = maybe newDatabase id do
    Tuple pp dirOrFile ← P.peel p
    pure $ Mount $ Database (pp </> P.dir (PU.nameOfFileOrDir dirOrFile))

-- This is not real parent because it can't determine
-- is it a directory or mount
parent ∷ Resource → Resource
parent = Directory ∘ resourceDir

-- GETTERS
resourceTag ∷ Resource → String
resourceTag = case _ of
  File _ → "file"
  Workspace _ → "workspace"
  Directory _ → "directory"
  Mount (View _) → "file"
  Mount (Database _) → "directory"

resourceMount ∷ Resource → Maybe Mount
resourceMount = case _ of
  Mount m → Just m
  _ → Nothing

mountTypeTag ∷ Mount → String
mountTypeTag = case _ of
  View _ → "view"
  Database _ → "mongodb"

resourceName ∷ Resource → String
resourceName = getPath ⋙ PU.getNameStr

resourceDir ∷ Resource → PU.DirPath
resourceDir = getPath ⋙ PU.getDir

resourcePath ∷ Resource → String
resourcePath r = either P.printPath P.printPath $ getPath r

getPath ∷ Resource → PU.AnyPath
getPath = case _ of
  File p → Right p
  Workspace p → Left p
  Directory p → Left p
  Mount (View p) → Right p
  Mount (Database p) → Left p


-- SETTERS
setDir ∷ PU.AnyPath → PU.DirPath → PU.AnyPath
setDir ap d = bimap (setDir' d) (setFile' d) ap
  where
  setDir' ∷ PU.DirPath → PU.DirPath → PU.DirPath
  setDir' d p =
    d </>
    (maybe P.currentDir (snd ⋙ PU.nameOfFileOrDir ⋙ P.dir) $ P.peel p)

  setFile' ∷ PU.DirPath → PU.FilePath → PU.FilePath
  setFile' d p =
    d </>
    (maybe (P.file "") (snd ⋙ PU.nameOfFileOrDir ⋙ P.file) $ P.peel p)

setPath ∷ Resource → PU.AnyPath → Resource
setPath (Workspace _) p = mkWorkspace p
setPath (File _) p = mkFile p
setPath (Directory _) p = mkDirectory p
setPath (Mount (Database _)) p = mkDatabase p
setPath (Mount (View _)) p = mkViewMount p

setName ∷ Resource → String → Resource
setName r name =
  setPath r ((renameAny (const name) (getPath r)))

-- MODIFIERS (PRIVATE)

renameAny ∷ (String → String) → PU.AnyPath → PU.AnyPath
renameAny fn ap = bimap (P.renameDir $ liftDir fn) (P.renameFile $ liftFile fn) ap
  where
  liftFile fn (P.FileName a) = P.FileName $ fn a
  liftDir fn (P.DirName a) = P.DirName $ fn a


-- TRAVERSALS
_tempFile ∷ LensP Resource Resource
_tempFile = lens id \r s → case r of
  File p →
    if isTempFile r
    then s
    else r
  _ → r

_filePath ∷ TraversalP Resource PU.FilePath
_filePath = wander \f s → case s of
  File fp → File <$> f fp
  Mount (View fp) → map (Mount ∘ View) $ f fp
  _ → pure s

_dirPath ∷ TraversalP Resource PU.DirPath
_dirPath = wander \f s → case s of
  Directory dp → Directory <$> f dp
  Mount (Database dp) → map (Mount ∘ Database) $ f dp
  _ → pure s

_Workspace ∷ PrismP Resource PU.DirPath
_Workspace = prism' Workspace case _ of
  Workspace dp → Just dp
  _ → Nothing

_path ∷ LensP Resource PU.AnyPath
_path = lens getPath setPath

_nameAnyPath ∷ LensP PU.AnyPath String
_nameAnyPath = lens PU.getNameStr (\p x → renameAny (const x) p)

_name ∷ LensP Resource String
_name = _path ∘ _nameAnyPath

_rootAnyPath ∷ LensP PU.AnyPath PU.DirPath
_rootAnyPath = lens PU.getDir setDir

_root ∷ LensP Resource PU.DirPath
_root = _path ∘ _rootAnyPath


-- INSTANCES
sortResource ∷ (Resource → String) → Sort → Resource → Resource → Ordering
sortResource project direction a b
  | (isHidden a) && (not $ isHidden b) = GT
  | (isHidden b) && (not $ isHidden a) = LT
  | otherwise = case direction of
    Asc → compare (project a) (project b)
    Desc → compare (project b) (project a)

instance eqResource ∷ Eq Resource where
  eq (File p) (File p') = p ≡ p'
  eq (Workspace p) (Workspace p') = p ≡ p'
  eq (Directory p) (Directory p') = p ≡ p'
  eq (Mount m) (Mount m') = m ≡ m'
  eq _ _ = false

instance eqMount ∷ Eq Mount where
  eq (Database p) (Database p') = p ≡ p'
  eq (View p) (View p') = p ≡ p'
  eq _ _ = false

instance resourceOrd ∷ Ord Resource where
  compare = sortResource resourcePath Asc

instance resourceIsForeign ∷ F.IsForeign Resource where
  read f = do
    name ← F.readProp "name" f
    ty ← F.readProp "type" f
    mountType ←
      F.readProp "mount" f
      <#> F.unNullOrUndefined

    template ← case ty of
      "directory" →
        pure case mountType of
          Just "mongodb" →
            newDatabase
          _ →
            maybe newDirectory (const newWorkspace)
              $ S.stripSuffix Config.workspaceExtension name

      "file" →
        pure case mountType of
          Just "view" →
            newViewMount
          _ →
            newFile

      _ → Left $ F.TypeMismatch "resource" "string"
    pure $ setName template name

instance encodeJsonResource ∷ EncodeJson Resource where
  encodeJson res =
    "type" := resourceTag res
    ~> "path" := resourcePath res
    ~> maybe
        jsonEmptyObject
        (\t → "mount" := t ~> jsonEmptyObject)
        (mountTypeTag <$> resourceMount res)

instance decodeJsonResource ∷ DecodeJson Resource where
  decodeJson json = do
    obj ← decodeJson json
    resType ← obj .? "type"
    path ← obj .? "path"
    let
      mountType =
        SM.lookup "mount" obj
        >>= decodeJson
        ⋙ either (const Nothing) pure
    case resType of
      "file" →
        let
          constr =
            case mountType of
              Just "view" → Mount ∘ View
              _ → File
        in
          parsePath "file" constr P.parseAbsFile path
      "workspace" →
        parsePath "workspace" Workspace P.parseAbsDir path
      "directory" →
        case mountType of
          Just "mongodb" →
            parsePath "mount" (Mount ∘ Database) P.parseAbsDir path
          _ →
            parsePath "directory" Directory P.parseAbsDir path
      _ → Left "Unrecognized resource type"
    where
    parsePath
      ∷ forall a
       . String
      → (P.Path P.Abs a P.Sandboxed → Resource)
      → (String → Maybe (P.Path P.Abs a P.Unsandboxed))
      → String
      → Either String Resource
    parsePath ty ctor parse s =
      maybe
        (Left $ "Invalid " ⊕ ty ⊕ " path")
        (Right ∘ ctor) $
          (P.rootDir </> _) <$> (P.sandbox P.rootDir =<< parse s)
