module Model.File.Dialog.Rename where

import Data.Maybe (Maybe(..))
import Data.Path.Pathy (rootDir)
import Model.Path (DirPath(), dropNotebookExt)
import Model.Resource (Resource(..), resourceDir, resourceName, root, isNotebook)
import Optic.Core (lens, LensP())

newtype RenameDialogRec = RenameDialogRec
  { showList :: Boolean
  , initial :: Resource
  , name :: String
  , dirs :: [Resource]
  , dir :: DirPath
  , siblings :: [Resource]
  , error :: Maybe String
  }

initialRenameDialog :: Resource -> RenameDialogRec
initialRenameDialog resource = RenameDialogRec
  { showList: false
  , initial: resource
  , name: if isNotebook resource
          then dropNotebookExt $ resourceName resource
          else resourceName resource
  , dir: resourceDir resource
  , siblings: []
  , dirs: [root]
  , error: Nothing
  }

_renameDialogRec :: LensP RenameDialogRec _
_renameDialogRec = lens (\(RenameDialogRec obj) -> obj) (const RenameDialogRec)

_showList :: LensP RenameDialogRec Boolean
_showList = _renameDialogRec <<< lens _.showList (_ { showList = _ })

_initial :: LensP RenameDialogRec Resource
_initial = _renameDialogRec <<< lens _.initial (_ { initial = _ })

_name :: LensP RenameDialogRec String
_name = _renameDialogRec <<< lens _.name (_ { name = _ })

_dirs :: LensP RenameDialogRec [Resource]
_dirs = _renameDialogRec <<< lens _.dirs (_ { dirs = _ })

_dir :: LensP RenameDialogRec DirPath
_dir = _renameDialogRec <<< lens _.dir (_ { dir = _ })

_siblings :: LensP RenameDialogRec [Resource]
_siblings = _renameDialogRec <<< lens _.siblings (_ { siblings = _ })

_error :: LensP RenameDialogRec (Maybe String)
_error = _renameDialogRec <<< lens _.error (_ { error = _ })
